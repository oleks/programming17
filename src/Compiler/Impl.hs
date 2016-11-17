module Compiler.Impl where

import Prelude hiding ( break )

import Ast

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S

data CompileError
  = OtherError String
  deriving (Eq, Show)

lineBreak :: BC.ByteString
lineBreak = BC.pack "\n"

wrap :: String -> String -> B.ByteString -> B.ByteString
wrap left right s = (BC.pack left) `B.append` s `B.append` (BC.pack right)

break :: B.ByteString -> B.ByteString
break s = s `B.append` lineBreak

data CEnv
  = CEnv
  { envDefs :: M.Map B.ByteString B.ByteString
  , envIncludes :: S.Set B.ByteString
  , envFuns :: [B.ByteString]
  , envWorkerIndex :: Word
  }
  deriving (Eq, Show)

data CmplEnv
  = CmplEnv
  { envIndent :: B.ByteString
  , envc :: CEnv
  }
  deriving (Eq, Show)

newtype CmplM a
  = CmplM { runCmplM :: CmplEnv ->
      Either CompileError (a, CEnv, B.ByteString) }

instance Functor CmplM where
  fmap f m = m >>= \a -> return (f a)

instance Applicative CmplM where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

instance Monad CmplM where
  return a = CmplM $ \ e -> Right (a, envc e, B.empty)
  m >>= f = CmplM $ \ e -> do
    (a, e1, s1) <- runCmplM m e
    (b, e2, s2) <- runCmplM (f a) (e { envc = e1 })
    return (b, e2, s1 `B.append` s2)
  fail s = CmplM $ \ _ -> Left $ OtherError s

get :: (CmplEnv -> a) -> CmplM a
get f = CmplM $ \ e -> Right (f e, envc e, B.empty)

getCEnv :: (CEnv -> a) -> CmplM a
getCEnv f = fmap f $ get envc

setCEnv :: (CEnv -> CEnv) -> CmplM ()
setCEnv f = CmplM $ \ e -> Right ((), (f . envc) e, B.empty)

with :: (CmplEnv -> CmplEnv) -> CmplM a -> CmplM a
with f m = CmplM $ \ e -> runCmplM m $ f e

indent :: CmplM a -> CmplM a
indent = with $ \ e ->
  e { envIndent = (envIndent e) `B.append` (BC.pack "  ") }

write :: B.ByteString -> CmplM ()
write s = CmplM $ \ e -> Right ((), envc e, s)

writeLn :: String -> CmplM ()
writeLn "" = write lineBreak
writeLn s = do
  i <- get envIndent
  write $ i `B.append` (BC.pack s) `B.append` lineBreak

data FunEnv
  = FunEnv
    { funRetvalIndex :: Word
    , funStackIndex :: Word
    }

newtype FunM a
  = FunM { runFunM :: (CmplEnv, FunEnv) ->
      Either CompileError (a, FunEnv, CEnv, B.ByteString) }

instance Functor FunM where
  fmap f m = m >>= \a -> return (f a)

instance Applicative FunM where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

instance Monad FunM where
  return a = FunM $ \ (e, fune) -> Right (a, fune, envc e, B.empty)
  m >>= f = FunM $ \ (e, fune) -> do
    (a, fune1, e1, s1) <- runFunM m (e, fune)
    (b, fune2, e2, s2) <- runFunM (f a) (e { envc = e1 }, fune1)
    return (b, fune2, e2, s1 `B.append` s2)
  fail s = FunM $ \ _ -> Left $ OtherError s

liftCmpl :: CmplM a -> FunM a
liftCmpl m = FunM $ \ (e, fune) -> do
  (a, envc0, s) <- runCmplM m e
  return $ (a, fune, envc0, s)

initialFunEnv :: FunEnv
initialFunEnv = FunEnv
  { funRetvalIndex = 0
  , funStackIndex = 0
  }

coliftFun :: FunM a -> CmplM a
coliftFun m = CmplM $ \ e -> do
  (a, _, envc0, s) <- runFunM m (e, initialFunEnv)
  return $ (a, envc0, s)

getFF :: (FunEnv -> a) -> FunM a
getFF f = FunM $ \ (e, fune) -> Right (f fune, fune, envc e, B.empty)

setFF :: (FunEnv -> FunEnv) -> FunM ()
setFF f = FunM $ \ (e, fune) -> Right ((), f fune, envc e, B.empty)

getFC :: (CmplEnv -> a) -> FunM a
getFC f = FunM $ \ (e, fune) -> Right (f e, fune, envc e, B.empty)

newRetval :: FunM String
newRetval = do
  i <- getFF funRetvalIndex
  setFF $ \ fune -> fune { funRetvalIndex = i + 1 }
  return $ "retval" ++ (show i)

newStack :: FunM String
newStack = do
  i <- getFF funStackIndex
  setFF $ \ fune -> fune { funStackIndex = i + 1 }
  return $ "stack" ++ (show i)

addInclude :: String -> CmplM ()
addInclude s = setCEnv $ \ e ->
  e { envIncludes = S.insert (BC.pack s) (envIncludes e) }

cmplArgs :: [TaskArg] -> B.ByteString
cmplArgs [] = BC.empty
cmplArgs args = wrap "\"" "\", " $ B.intercalate (BC.pack "\", \"") $ map BC.pack args

cmplWorker :: String -> (TaskName, [TaskArg]) -> CmplM ()
cmplWorker worker (name, args) = do
  addInclude "<unistd.h>"
  writeLn $ "int " ++ worker ++ "(void *arg) {"
  indent $ do
    let argss = BC.unpack $ cmplArgs args
    writeLn $ "arg = arg;"
    writeLn $ "execlp(\"" ++ name ++ "\", \"" ++ name ++ "\", " ++ argss ++ "NULL);"
    writeLn $ "return 0;"
  writeLn $ "}"

newWorker :: (TaskName, [TaskArg]) -> CmplM String
newWorker (name, args) = do
  i <- getCEnv envWorkerIndex
  setCEnv $ \ e -> e { envWorkerIndex = i + 1 }
  let worker = "worker" ++ (show i)

  workerfun <- compileM (cmplWorker worker (name, args))
  funs <- getCEnv envFuns
  setCEnv $ \ e -> e { envFuns = workerfun : funs }

  return worker

cmplCommand :: Command -> FunM ()
cmplCommand (TaskCall (name, args)) = do
  retval <- newRetval
  stack <- newStack
  worker <- liftCmpl $ newWorker (name, args)
  liftCmpl $ do
    writeLn $ "long " ++ retval ++ ";"
    writeLn $ "char " ++ stack ++ "[STACK_SIZE];"
    writeLn $ retval ++ " = clone(" ++ worker ++ ", " ++ stack ++ " + STACK_SIZE, " ++
      "SIGCHLD, NULL);"
    writeLn $ "if (" ++ retval ++ " < 0) {"
    indent $ writeLn "error(1, errno, \"system b0rked ;-/\");"
    writeLn "}"
    writeLn ""

cmplPipeline :: Pipeline -> FunM ()
cmplPipeline (Pipeline (c, _)) = cmplCommand c

-- Normal pipelines do just a fork followed by execve as you'd expect

cloneDefs :: M.Map B.ByteString B.ByteString
cloneDefs = (M.fromList . map (\ (n, d) -> (BC.pack n, BC.pack d)))
  [ ("_GNU_SOURCE", "")
  , ("STACK_SIZE", "0x10000")
  ]

cloneIncludes :: S.Set B.ByteString
cloneIncludes = (S.fromList . map BC.pack)
  [ "<error.h>"
  , "<errno.h>"
  , "<sched.h>"
  , "<signal.h>"
  , "<stdlib.h>"
  ]

cmplScript :: Script -> CmplM ()
cmplScript (Script []) = do
  addInclude "<stdlib.h>"
  writeLn "int main() {"
  indent $ do
    writeLn "exit(EXIT_SUCCESS);"
  writeLn "}"
cmplScript (Script ps) = do
  setCEnv $ \ e -> e
    { envDefs = M.union (envDefs e) cloneDefs
    , envIncludes = S.union (envIncludes e) cloneIncludes
    }
  writeLn "int main() {"
  indent $ do
    coliftFun $ mapM_ cmplPipeline ps
    writeLn "exit(EXIT_SUCCESS);"
  writeLn "}"

initialEnv :: CmplEnv
initialEnv = CmplEnv
  { envIndent = B.empty
  , envc = CEnv
    { envDefs = M.empty
    , envIncludes = S.empty
    , envFuns = []
    , envWorkerIndex = 0
    }
  }

compileM :: CmplM () -> CmplM B.ByteString
compileM m = CmplM $ \ e -> do
  let env = initialEnv { envc = envc e }
  fmap (\ ((), envc0, s) -> (s, envc0, B.empty)) $ runCmplM m env

defsMap :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
defsMap n d ds | B.length d == 0 =
  ds `B.append` (BC.pack "#define ") `B.append` n
    `B.append` lineBreak
defsMap n d ds =
  ds `B.append` (BC.pack "#define ") `B.append` n
    `B.append` (break $ wrap " (" ")" d)

includeMap :: B.ByteString -> B.ByteString
includeMap i = break $ (BC.pack "#include ") `B.append` i

breakOff :: B.ByteString -> B.ByteString
breakOff s | B.length s > 0 = break s
breakOff s = s

compile :: (a -> CmplM ()) -> a -> Either CompileError String
compile f a = do
  ((), envc0, fun) <- runCmplM (f a) initialEnv
  let defs = breakOff $ M.foldrWithKey defsMap B.empty (envDefs envc0)
  let includes = breakOff $ B.concat $ map includeMap ((S.toList . envIncludes) envc0)
  let funs = (reverse . envFuns) envc0
  let s = B.intercalate lineBreak (funs ++ [fun])
  return $ BC.unpack $ defs `B.append` includes `B.append` s

compileScript :: Script -> Either CompileError String
compileScript s = compile cmplScript s

-- TODO:
-- exec command - special command which execve's instead of fork+execve
