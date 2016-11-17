module Ast where

type TaskName = String
type TaskArg = String

newtype Command
  = TaskCall (TaskName, [TaskArg])
  deriving (Eq, Read, Show)

newtype Pipeline
  = Pipeline (Command, [Command])
  deriving (Eq, Read, Show)

newtype Script
  = Script [Pipeline]
  deriving (Eq, Read, Show)
