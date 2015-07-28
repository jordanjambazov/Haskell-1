module StatementParser where

import Parser
import Statement
import Control.Applicative
import qualified Data.Char as C
  ( isAlpha
  , isDigit
  , digitToInt
  )


plusP :: Parser Oper
plusP = const Plus <$> char '+'

minusP :: Parser Oper
minusP = const Minus <$> char '-'

oper :: Parser Oper
oper = plusP <|> minusP

variable :: Parser Variable
variable = oneOrMore $ satisfy (C.isAlpha)

value :: Parser Value
value = read <$> (oneOrMore $ satisfy (C.isDigit))

expr :: Parser Expr
expr =    Var <$> variable
      <|> Val <$> value
      <|> liftA3 Op expr oper expr

assignP :: Parser Statement
assignP = liftA2 Assign (variable) expr

incrP :: Parser Statement
incrP = Incr <$> (string "++" *> variable)

decrP :: Parser Statement
decrP = Decr <$> (string "--" *> variable)

ifP :: Parser Statement
ifP = liftA2 If (string "if(" *> expr <* string "){") (statement <* char '}')

str = string

forP :: Parser Statement
forP = (liftA3 For (str "for(" *> statement) (char ';' *> expr <* char ';') (statement <* str "){")) <*> (statement <* char '}')

statement :: Parser Statement
statement = assignP <|> ifP <|> forP <|> incrP
