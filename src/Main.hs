module Main where

import Prelude hiding (lex)
import           Syntax
import           System.Console.Repline
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Eval (whnf)
import Lexer (lex)
import Parser (parse)
import Constraints (constraints)
import Unification (unify, applySubst)

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ print (fmap (\s -> applySubst s ty) res)
  where
    (ty, cs) = constraints (const (error "no type")). parse . lex $ input
    res = unify cs


completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options =
  [ ("help", help)  -- :help
  , ("type", help)  -- :type
  ]

ini :: Repl ()
ini = return ()

main :: IO ()
main = evalRepl "λ> " cmd options (Word completer) ini
