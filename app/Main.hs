module Main where

import Data.Maybe

-- Parsing
import Text.Parsec
import Text.Parsec.Char

data Variable = Var Char deriving (Show, Eq)
data TermNode = LambdaNode [Variable] TermNode
              | VarNode Variable
              | AppNode [TermNode]
              deriving Show


lambdaTerm = do var <- variable
                return (VarNode var)
         <|> do terms <- between (char '(') (char ')') lambdaTerms1
                return (case terms of
                  [x]  -> x
                  x:xs -> AppNode (x:xs))
         <|> do char '\\'
                vars <- many1 variable
                char '.'
                terms <- lambdaTerms1
                return (LambdaNode vars (
                  case terms of
                    [x]  -> x
                    x:xs -> AppNode (x:xs)))

lambdaTerms1 = try (do x <- lambdaTerm
                       xs <- lambdaTerms1
                       return (x:xs))
           <|> do term <- lambdaTerm
                  return [term]

variable = do var <- lower
              return (Var var)

parseLambda :: String -> Either ParseError TermNode
parseLambda input = case parse lambdaTerms1 "(unknown)" input of
  Left err -> Left err
  Right [x] -> Right x
  Right xs -> Right (AppNode xs)

-- Translation
data Term = VarTerm Variable
          | Abs Variable Term
          | App Term Term
          deriving Eq

translate :: TermNode -> Term
translate (VarNode var) = VarTerm var
translate (LambdaNode vars term) = case vars of 
   [x]  -> Abs x (translate term)
   x:xs -> Abs x (translate (LambdaNode xs term))
translate (AppNode (x1:x2:xs)) =
  foldl (\term node -> App term (translate node)) (translate x1) (x2:xs)
translate (AppNode xs) = error . show $ xs

instance Show Term where
   show (VarTerm (Var v)) = [v]
   show (Abs (Var v) term) = "(λ" ++ [v] ++ "." ++ (show term) ++ ")"
   show (App term1 term2) = "(" ++ (show term1) ++ (show term2) ++ ")"

-- β-reduction
leftmostReduce :: Term -> (Maybe Term)
leftmostReduce (VarTerm x) = Nothing
leftmostReduce (App s1 s2) = case (leftmostReduce s1) of
  Just s1' -> Just (App s1' s2)
  Nothing  -> case s1 of
    (Abs var term) -> Just (sub term s2 var)
    other          -> case (leftmostReduce s2) of
      Just s2' -> Just (App s1 s2')
      Nothing  -> Nothing
leftmostReduce (Abs var term) = case (leftmostReduce term) of
  Just term' -> Just (Abs var term')
  Nothing    -> Nothing

-- calculates term[s/x]
sub :: Term -> Term -> Variable -> Term
sub (VarTerm y) s x
  | x == y = s
  | x /= y = VarTerm y
sub (App s1 s2) s x = App (sub s1 s x) (sub s2 s x)
sub (Abs y inner) s x
  | y == x = Abs y inner
  | y /= x = Abs y (sub inner s x)

main :: IO ()
main = 
  do c <- getContents
     case parseLambda c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> putStrLn . unlines . (map show) . (takeWhile isJust) . (iterate (>>= leftmostReduce)) . Just . translate $ r
