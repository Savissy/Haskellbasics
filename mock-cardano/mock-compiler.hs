{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

-- Abstract Syntax Tree (AST)
data Stmt = Assign String Expr
          | Print Expr
          deriving (Show)

data Expr = Number Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- Virtual Machine (VM) Instructions
data Instruction = PUSH Int
                 | LOAD String
                 | STORE String
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | PRINT
                 deriving (Show)

-- Compiler State
type CompilerState = Map String Int

-- Parser
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

identifier :: Parser String
identifier = lexeme $ (:) <$> letter <*> many alphaNum

number :: Parser Int
number = lexeme $ read <$> many1 digit

assign :: Parser Stmt
assign = do
  var <- identifier
  void $ lexeme $ char '='
  expr <- expression
  void $ lexeme $ char ';'
  return $ Assign var expr

printStmt :: Parser Stmt
printStmt = do
  void $ lexeme $ string "print"
  expr <- expression
  void $ lexeme $ char ';'
  return $ Print expr

expression :: Parser Expr
expression = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor =  Number <$> number
      <|> Var <$> identifier
      <|> parens expression

addOp :: Parser (Expr -> Expr -> Expr)
addOp =  Add <$ lexeme (char '+')
        <|> Sub <$ lexeme (char '-')

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp =  Mul <$ lexeme (char '*')
        <|> Div <$ lexeme (char '/')

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

stmt :: Parser Stmt
stmt = assign <|> printStmt

program :: Parser [Stmt]
program = many1 stmt

-- Compiler
compileExpr :: Expr -> [Instruction]
compileExpr (Number n) = [PUSH n]
compileExpr (Var x) = [LOAD x]
compileExpr (Add e1 e2) = compileExpr e2 ++ compileExpr e1 ++ [ADD]
compileExpr (Sub e1 e2) = compileExpr e2 ++ compileExpr e1 ++ [SUB]
compileExpr (Mul e1 e2) = compileExpr e2 ++ compileExpr e1 ++ [MUL]
compileExpr (Div e1 e2) = compileExpr e2 ++ compileExpr e1 ++ [DIV]

compileStmt :: Stmt -> [Instruction]
compileStmt (Assign x e) = compileExpr e ++ [STORE x]
compileStmt (Print e) = compileExpr e ++ [PRINT]

compileProgram :: [Stmt] -> [Instruction]
compileProgram = concatMap compileStmt

-- Virtual Machine
type VMState = Map String Int

runVM :: [Instruction] -> VMState -> IO VMState
runVM [] state = return state
runVM (PUSH n : ins) state = runVM ins (Map.insert "stack" n state)
runVM (LOAD x : ins) state =
  case Map.lookup x state of
    Just val -> runVM (PUSH val : ins) state
    Nothing -> error $ "Variable not found: " ++ x
runVM (STORE x : ins) state =
  case Map.lookup "stack" state of
    Just val -> runVM ins (Map.insert x val state)
    Nothing -> error "Stack underflow"
runVM (ADD : ins) state =
  case (Map.lookup "stack" state, Map.lookup "stack2" state) of
    (Just a, Just b) -> runVM ins (Map.insert "stack" (a + b) state)
    _ -> error "Stack underflow"
runVM (PRINT : ins) state =
  case Map.lookup "stack" state of
    Just val -> do
      print val
      runVM ins state
    Nothing -> error "Stack underflow"
-- Similar implementations for SUB, MUL, DIV...

-- Main
main :: IO ()
main = do
  let source = "x = 10; y = 20; z = x + y; print z;"
  case parse program "" source of
    Left err -> print err
    Right stmts -> do
      let instructions = compileProgram stmts
      putStrLn "Generated VM Code:"
      print instructions
      putStrLn "Execution Output:"
      _ <- runVM instructions Map.empty
      return ()
