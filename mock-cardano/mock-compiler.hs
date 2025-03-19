-- Saviour Uzoukwu
-- 17th march 2025
import Data.Map (Map)
import qualified Data.Map as Map

-- Abstract Syntax Tree (AST)
data Expr = Number Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          deriving (Show)

-- Virtual Machine (VM) Instructions
data Instruction = PUSH Int
                 | LOAD String
                 | STORE String
                 | ADD
                 | SUB
                 deriving (Show)

-- Compiler
compile :: Expr -> [Instruction]
compile (Number n) = [PUSH n]
compile (Var x) = [LOAD x]
compile (Add e1 e2) = compile e2 ++ compile e1 ++ [ADD]
compile (Sub e1 e2) = compile e2 ++ compile e1 ++ [SUB]

-- Virtual Machine
type VMState = (Map String Int, [Int])  -- (Variables, Stack)

runVM :: [Instruction] -> VMState -> IO VMState
runVM [] state = return state
runVM (PUSH n : ins) (vars, stack) = runVM ins (vars, n : stack)
runVM (LOAD x : ins) (vars, stack) =
  case Map.lookup x vars of
    Just val -> runVM ins (vars, val : stack)
    Nothing -> error $ "Variable not found: " ++ x
runVM (STORE x : ins) (vars, val : stack) = runVM ins (Map.insert x val vars, stack)
runVM (ADD : ins) (vars, a : b : stack) = runVM ins (vars, (a + b) : stack)
runVM (SUB : ins) (vars, a : b : stack) = runVM ins (vars, (a - b) : stack)
runVM _ _ = error "Invalid VM state"

-- Example Usage
main :: IO ()
main = do
  -- Example expression: (x + 10) - (y - 5)
  let expr = Sub (Add (Var "x") (Number 10)) (Sub (Var "y") (Number 5))
  
  -- Compile the expression
  let instructions = compile expr
  
  -- Initial VM state
  let vars = Map.fromList [("x", 20), ("y", 15)]  -- x = 20, y = 15
  let initialState = (vars, [])
  
  -- Run the VM
  putStrLn "Generated VM Code:"
  print instructions
  putStrLn "Execution Output:"
  (finalVars, finalStack) <- runVM instructions initialState
  putStrLn $ "Final Stack: " ++ show finalStack
  putStrLn $ "Final Variables: " ++ show finalVars
