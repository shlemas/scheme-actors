--
-- Copyright (c) 2016 Shawn LeMaster
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

{-

-- Some example code. If you want to execute this, then either execute the
-- compiled binary, or load this file into ghci and execute the `main`
-- function. Then copy/paste these lines in one-by-one.

(define fib   (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(define fact  (lambda (n) (if (= n 0)  1 (* n (fact (- n 1))))))
(define fib@  (alpha  (n) (begin0 (fib n)  (display "(fib ")  (display n) (displayln ") is available."))))
(define fact@ (alpha  (n) (begin0 (fact n) (display "(fact ") (display n) (displayln ") is available."))))
(let ((fib-future (fib@ 25)) (fact-future (fact@ 9))) (displayln (- (fact-future) (fib-future))))

Output:
(fact 9) is available.
(fib 25) is available.
287855

-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Text.ParserCombinators.Parsec

import qualified Data.HashMap.Strict as H

-- ------- --
--  Types  --
-- ------- --

type Env = H.HashMap String LispVal

-- An actor's mailbox is simply a concurrent queue of LispVals.
-- Specifically, these will be MessageVals, as defined below.
type Mailbox = TQueue LispVal

-- A future is a simple concurrent container for a LispVal. This type will be
-- contained in a FutureVal as defined below. When a sender sends a message to
-- an actor, a future is created and sent along with it, so that the actor
-- knows how to return a result to the sender.
type Future = MVar LispVal

data LispVal = AtomVal String
             | ListVal [LispVal]
             | NumberVal Integer
             | StringVal String
             | BoolVal Bool
             | PrimitiveFuncVal ([LispVal] -> LispVal)
             | FuncVal Env [String] [LispVal]
             | ActorVal Env [String] [LispVal] Mailbox
             | MessageVal LispVal Future
             | FutureVal Future
             | VoidVal

instance Show LispVal where
  show (StringVal contents)    = contents
  show (AtomVal name)          = name
  show (NumberVal contents)    = show contents
  show (BoolVal True)          = "#t"
  show (BoolVal False)         = "#f"
  show (ListVal contents)      = "(" ++ unwords (map show contents) ++ ")"
  show (PrimitiveFuncVal _)    = "<primitive>"
  show (FuncVal _ args _)      = "(lambda (" ++ unwords args ++ ") ...)"
  show (ActorVal _ args _ _)   = "(alpha (" ++ unwords args ++ ") ...)"
  show (MessageVal contents _) = "<message " ++ show contents ++ ">"
  show (FutureVal _)           = "<future>"
  show VoidVal                 = "<void>"

-- -------- --
--  Parser  --
-- -------- --

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do char '"'
                 string <- many (noneOf "\"")
                 char '"'
                 return $ StringVal string

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> BoolVal True
                          "#f" -> BoolVal False
                          _    -> AtomVal atom

parseNumber :: Parser LispVal
parseNumber = do number <- many1 digit
                 return $ NumberVal (read number)

parseList :: Parser LispVal
parseList = do list <- sepBy parseExpr spaces
               return $ ListVal list

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 lispVal <- parseExpr
                 return $ ListVal [AtomVal "quote", lispVal]

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                listVal <- try parseList
                char ')'
                return listVal

-- ------------ --
--  Primitives  --
-- ------------ --

unpackNum :: LispVal -> Integer
unpackNum (NumberVal n) = n
unpackNum _             = error "unpackNum: Type mismatch"

unpackStr :: LispVal -> String
unpackStr (StringVal s) = s
unpackStr _             = error "unpackStr: Type mismatch"

unpackBool :: LispVal -> Bool
unpackBool (BoolVal b) = b
unpackBool _           = error "unpackBool: Type mismatch"

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = NumberVal $ foldl1 op (map unpackNum params)

boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpacker op args = BoolVal $ (unpacker $ args !! 0) `op` (unpacker $ args !! 1)

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> LispVal
car [ListVal (x:_)] = x
car _               = error "car: Type mismatch"

cdr :: [LispVal] -> LispVal
cdr [ListVal (_ : xs)] = ListVal xs
cdr _                  = error "cdr: Type mismatch"

cons :: [LispVal] -> LispVal
cons [x1, ListVal []] = ListVal [x1]
cons [x,  ListVal xs] = ListVal (x:xs)
cons _                = error "cons: Type mismatch"

eqv :: [LispVal] -> LispVal
eqv [(BoolVal arg1),       (BoolVal arg2)]       = BoolVal $ arg1 == arg2
eqv [(NumberVal arg1),     (NumberVal arg2)]     = BoolVal $ arg1 == arg2
eqv [(StringVal arg1),     (StringVal arg2)]     = BoolVal $ arg1 == arg2
eqv [(AtomVal arg1),       (AtomVal arg2)]       = BoolVal $ arg1 == arg2
eqv [(ListVal arg1),       (ListVal arg2)]       = BoolVal $ (length arg1 == length arg2) && (all eqvPair (zip arg1 arg2))
                                                     where eqvPair (x1, x2) = let (BoolVal val) = eqv [x1, x2] in val
eqv [_, _]                                       = BoolVal False
eqv _                                            = error "eqv: Type mismatch"

primitives :: [(String, LispVal)]
primitives = [("+",         PrimitiveFuncVal $ numericBinop (+)),
              ("-",         PrimitiveFuncVal $ numericBinop (-)),
              ("*",         PrimitiveFuncVal $ numericBinop (*)),
              ("/",         PrimitiveFuncVal $ numericBinop div),
              ("mod",       PrimitiveFuncVal $ numericBinop mod),
              ("quotient",  PrimitiveFuncVal $ numericBinop quot),
              ("remainder", PrimitiveFuncVal $ numericBinop rem),
              ("=",         PrimitiveFuncVal $ numBoolBinop (==)),
              ("<",         PrimitiveFuncVal $ numBoolBinop (<)),
              (">",         PrimitiveFuncVal $ numBoolBinop (>)),
              ("/=",        PrimitiveFuncVal $ numBoolBinop (/=)),
              (">=",        PrimitiveFuncVal $ numBoolBinop (>=)),
              ("<=",        PrimitiveFuncVal $ numBoolBinop (<=)),
              ("&&",        PrimitiveFuncVal $ boolBoolBinop (&&)),
              ("||",        PrimitiveFuncVal $ boolBoolBinop (||)),
              ("string=?",  PrimitiveFuncVal $ strBoolBinop (==)),
              ("string<?",  PrimitiveFuncVal $ strBoolBinop (<)),
              ("string>?",  PrimitiveFuncVal $ strBoolBinop (>)),
              ("string<=?", PrimitiveFuncVal $ strBoolBinop (<=)),
              ("string>=?", PrimitiveFuncVal $ strBoolBinop (>=)),
              ("car",       PrimitiveFuncVal $ car),
              ("cdr",       PrimitiveFuncVal $ cdr),
              ("cons",      PrimitiveFuncVal $ cons),
              ("eq?",       PrimitiveFuncVal $ eqv),
              ("eqv?",      PrimitiveFuncVal $ eqv)]

-- ------------ --
--  Evaluation  --
-- ------------ --

-- A function that monitors a mailbox in a loop. readTQueue will block until a
-- message is received. Once a message is received, it is executed, and the
-- resulting value is placed into the future associated with the message so that
-- the value can be received at the calling site.
monitorMailbox :: Mailbox -> IO ()
monitorMailbox mailbox = do (MessageVal funcVal future) <- atomically $ readTQueue mailbox
                            lispVal <- exec funcVal
                            putMVar future lispVal
                            monitorMailbox mailbox

-- Create a new actor mailbox (message queue).
newMailbox :: IO Mailbox
newMailbox = atomically newTQueue

eval :: Env -> LispVal -> IO LispVal
eval env val@(StringVal _)                  = return val
eval env val@(NumberVal _)                  = return val
eval env val@(BoolVal _)                    = return val
eval env (AtomVal id)                       = return $ H.lookupDefault (error $ "eval: " ++ id ++ " does not exist") id env
eval env (ListVal [AtomVal "quote", val])   = return val
eval env (ListVal [AtomVal "display", val]) = do lispVal <- eval env val
                                                 putStr $ show lispVal
                                                 return VoidVal
eval env (ListVal [AtomVal "newline"])      = do putStrLn ""
                                                 return VoidVal
eval env (ListVal [AtomVal "displayln", val]) = do lispVal <- eval env val
                                                   putStrLn $ show lispVal
                                                   return VoidVal
eval env (ListVal [AtomVal "if", pred, v1, v2]) = do boolVal <- eval env pred
                                                     case boolVal of
                                                       (BoolVal True)  -> eval env v1
                                                       (BoolVal False) -> eval env v2
                                                       otherwise       -> error "eval: Invalid predicate"
eval env (ListVal ((AtomVal "begin"):vals)) = do results <- mapM (eval env) vals
                                                 return $ last results
eval env (ListVal ((AtomVal "begin0"):vals)) = do results <- mapM (eval env) vals
                                                  return $ head results
eval env (ListVal ((AtomVal "let"):(ListVal pairs):vals)) = let bindingList []                            = return []
                                                                bindingList ((ListVal [AtomVal s, v]):ps) = do v'   <- eval env v
                                                                                                               rest <- bindingList ps
                                                                                                               return $ (s, v') : rest in
                                                              do bindings <- bindingList pairs
                                                                 results  <- mapM (eval (H.union (H.fromList bindings) env)) vals
                                                                 return $ last results
eval env (ListVal (AtomVal "lambda" : ListVal params : body)) = return $ FuncVal  env (map show params) body

-- Create an actor. First, create a new mailbox for the actor, and then spawn
-- a new thread to monitor the mailbox for new messages indefinitely. Then
-- return an ActorVal. Note how the mailbox for an actor is stored in its
-- ActorVal.
eval env (ListVal (AtomVal "alpha" : ListVal params : body))  = do mailbox <- newMailbox
                                                                   (forkIO . monitorMailbox) mailbox
                                                                   return $ ActorVal env (map show params) body mailbox

eval env (ListVal (appliableName : args)) = do appliable <- eval env appliableName
                                               argVals   <- mapM (eval env) args
                                               apply env appliable argVals
eval _ badVal = error $ "eval: Bad expression " ++ show badVal

exec :: LispVal -> IO LispVal
exec (FuncVal closure params body) = do bodyVals <- mapM (eval closure) body
                                        return $ last bodyVals

apply :: Env -> LispVal -> [LispVal] -> IO LispVal
apply _   (PrimitiveFuncVal func)       args = return $ func args

apply env (FuncVal closure params body) args =
  if length params == length args
    then exec (FuncVal closure' params body)
    else error "apply: Invalid number of arguments"
  where closure' = H.unions [H.fromList (zip params args), closure, env]

-- Send a message to an actor. First, a new future MVal is created. Then,
-- the message is sent. Note how the message includes the future that the
-- actor should place the result into. Finally, a FutureVal is returned
-- containing the MVal that the actor will fill once it has processed
-- this message.
apply env (ActorVal closure params body mailbox) args =
  if length params == length args
    then do future <- newEmptyMVar
            atomically $ writeTQueue mailbox (MessageVal (FuncVal closure' params body) future)
            return $ FutureVal future
    else error "apply: Invalid number of arguments"
  where closure' = H.unions [H.fromList (zip params args), closure, env]

-- Evaluate a future. All this function needs to do is call readMVar on the
-- MVar contained in the FutureVal. This call will block until the MVar has
-- been written by the actor after processing its associated message.
apply env (FutureVal future) args =
  if null args then do lispVal <- readMVar future
                       return lispVal
               else error "apply: Futures do not accept arguments"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> StringVal $ "No match: " ++ show err
                   Right val -> val

evalString :: Env -> String -> IO Env
evalString env str = case (readExpr str) of
                       (ListVal [AtomVal "define", AtomVal var, form]) -> do lispVal <- eval env form
                                                                             return $ H.insert var lispVal env
                       lispVal -> do lispVal' <- eval env lispVal
                                     case lispVal' of
                                       VoidVal   -> return env
                                       otherwise -> do putStrLn $ "=> " ++ show lispVal'
                                                       return env

eval' :: String -> IO ()
eval' str = do evalString (H.fromList primitives) str
               return ()

repl :: Env -> IO ()
repl env = do putStr "> "
              hFlush stdout
              input <- getLine
              if not $ null input
                then if input == "quit" then return () else evalString env input >>= repl
                else repl env

main :: IO ()
main = repl $ H.fromList primitives
