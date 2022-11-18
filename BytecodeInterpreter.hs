module Main where

import System.Environment
import Data.Map as M hiding (foldl) 
data Stack = Stack [Float] deriving Show 
data ByteCode =  Load Float
              |  Write String
              |  Read String
              |  Add
              |  Mult
              |  Sub
              |  Div
              |  Return
              |  Goto Int

-- define goto such that if the value at the top of the stack is not zero then go to the line indicated, else continue
emptyStack :: Stack
emptyStack = Stack []

emptyVarList :: Map String Float
emptyVarList = M.fromList []

pushStack :: Maybe Float -> Stack -> Stack
pushStack (Just x) (Stack xs) = Stack (x:xs)
pushStack Nothing  (Stack xs) = Stack (xs)

popStack :: Stack  -> (Float, Stack)
popStack (Stack [])     = error "empty stack"
popStack (Stack (x:[])) = (x, Stack []) 
popStack (Stack (x:xs)) = (x, Stack xs) 

nameCheck :: String -> Maybe String
nameCheck (x:xs) = if x =='\'' && '\'' `elem` xs
                   then
                     Just $ takeWhile (/='\'') xs
                   else
                     Nothing

addStack :: Stack -> Stack
addStack (Stack [])        = error "empty stack"
addStack (Stack (x:[]))    = error "not enough arguments"
addStack (Stack (x:y: xs)) = Stack $ (x+y):xs

multStack :: Stack -> Stack
multStack (Stack [])         = error "empty stack"
multStack (Stack (x:[]))     = error "not enough arguments"
multStack (Stack (x:y:xs))   = Stack $ (x*y):xs

subStack :: Stack -> Stack
subStack (Stack [])         = error "empty stack"
subStack (Stack (x: []))    = error "no enough arguments"
subStack (Stack (x: y: xs)) = Stack $ (x-y):xs

divStack :: Stack -> Stack
divStack (Stack [])       = error "empty stack"
divStack (Stack (x: []))  = error "no enough arguments"
divStack (Stack (x:y:xs)) = Stack $ ( x/ y):xs


main :: IO (Stack, Map String Float)
main = do
  args <- getArgs
  code  <- readFile (head args) 
  foldl (flip cmd) (pure (emptyStack, emptyVarList)) $ fmap parseByteCode $ words <$> lines code
               
parseByteCode :: [String] -> Maybe ByteCode 
parseByteCode []                  = Nothing
parseByteCode  (cmd:[])           = case cmd of
    "ADD"          -> Just Add
    "MULT"         -> Just Mult
    "SUB"          -> Just Sub
    "DIV"          -> Just Div
    "RETURN_VAL"   -> Just Return
    _              -> Nothing
parseByteCode  (cmd:arg:whatever) = case (cmd,arg) of
    ("ADD",_)          -> Just Add
    ("MULT",_)         -> Just Mult
    ("SUB",_)          -> Just Sub
    ("DIV",_)          -> Just Div
    ("RETURN_VAL",_)   -> Just Return
    ("LOAD_VAL",x)     -> Just $ Load (read x :: Float)
    ("WRITE_VAR",x)    -> Just $ Write x
    ("READ_VAR",x)     -> Just $ Read x
    ("GOTO", x)        -> Just $ Goto (read x :: Int)
    (_,_)              -> Nothing

cmd :: Maybe ByteCode -> IO (Stack, Map String Float) -> IO (Stack, Map String Float) 
cmd Nothing stackvar    =  stackvar
cmd (Just cmd) stackvar = do (Stack stk, var) <- stackvar
                             let (x,y) = popStack $ Stack stk in case cmd of
                              Load  a  ->  pure (pushStack (Just a) $ Stack stk, var)
                              Write k  ->  pure (Stack stk,  M.insert k x var)
                              Read  k  ->  pure (pushStack (M.lookup k var) $ Stack stk, var) 
                              Add      ->  pure (addStack $ Stack stk, var)
                              Mult     ->  pure (multStack $ Stack stk, var)
  			      Sub      ->  pure (subStack $ Stack stk, var)
  			      Div      ->  pure (divStack $ Stack stk, var)
                              Return   ->  do putStrLn $ show x
                                              pure (y, var) 
                              _        ->  pure (Stack stk, var)



{-
cmd2 :: Int -> [Maybe ByteCode] -> IO (Stack, Map String Float) -> IO (Stack, Map String Float)
cmd2 n code (Stack stk, var) | n<= length code = case (code !!n) of
                                           Just (Goto x) -> if (fst $ popStack $ Stack stk==0) then cmd2 (x-1) code stackvar else cmd2 (n+1) code  stackvar
                                           _             -> cmd2 (n+1) code (cmd (code!!n) stackvar)
                            | otherwise       = stackvar
-}
