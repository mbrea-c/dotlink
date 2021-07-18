module EvalTypes where

data Result a = Success a | Failure String

data ActionEval0 = CopySubst String String | LinkSubst String String | IncludeSubst String | LogSubst String

data ActionEval1 = CopyChecked String String | LinkChecked String String | IncludeChecked String | LogChecked String

data ActionEval2 = CopyExpanded String String | LinkExpanded String String | LogExpanded String
