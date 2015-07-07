data Statement = Oper Expr Oper Expr | Statement

type Variable = String
type Value    = Int

type Variables = Map.Map Variable Value

data Expr = Var Variable
          | Val Value
          | Op  Expr Oper Expr
  deriving (Show, Eq)

data State = State [Statement] Variables
  deriving (Show, Eq)

data Oper = ...
