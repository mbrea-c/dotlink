module EvalTypes where

data Result a = Success a | Failure String

data ActionEval0 = LinkSubst String String | IncludeSubst String | LogSubst String

data ActionEval1 = LinkChecked String String | IncludeChecked String | LogChecked String

data ActionEval2 = LinkExpanded String String | LogExpanded String
