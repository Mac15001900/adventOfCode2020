import MyUtils
import Parsing
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input18.txt"

data Expression = E (Expression,[Operation]) | Atom Int deriving (Show,Eq,Read)
data Operation = Plus Expression | Times Expression deriving (Show,Eq,Read)

eval :: Expression -> Int
eval (Atom n) = n
eval (E (a,[])) = eval a
eval (E (a,(Plus  b):xs)) = eval (E (Atom ((eval a) + (eval b)), xs))
eval (E (a,(Times b):xs)) = eval (E (Atom ((eval a) * (eval b)), xs))


expressionParser :: Parser Expression
expressionParser = do a <- atom
                      do ops <- operationsParser
                         return (E (Atom a,ops))
                         <|> return (Atom a)
                      <|> 
                      do e <- paretheses
                         do ops <- operationsParser
                            return (E (e,ops))
                            <|> return e
                         

operationsParser :: Parser [Operation]
operationsParser = do o <- item
                      e <- paretheses
                      do next <- operationsParser
                         if      o == '+' then return ((Plus e):next)
                         else if o == '*' then return ((Times e):next)
                         else empty 
                         <|>
                         if      o == '+' then return [Plus e]
                         else if o == '*' then return [Times e]
                         else empty


paretheses :: Parser Expression
paretheses = do symbol "("
                e <- expressionParser
                symbol ")"
                return e
               <|>
               do a <- atom
                  return (Atom a)

atom :: Parser Int
atom = nat

parseLine :: String -> Expression
parseLine xs = case (parse expressionParser xs) of
                [(n,[])]  -> n
                [(_,out)] -> error ("Invalid input near " ++ out)
                []        -> error "Invalid input"

part1 :: [String] -> Int
part1 s = s |> map (filter (/=' ')) |> map parseLine |> map eval |> sum

--part2--

multiplicationParser :: Parser Int
multiplicationParser = do a <- additionParser
                          symbol "*"
                          b <- multiplicationParser
                          return (a*b)
                          <|>
                          additionParser
                          
additionParser :: Parser Int
additionParser = do a <- paretheses2
                    symbol "+"
                    b <- additionParser
                    return (a+b)
                    <|>
                    paretheses2
                    
paretheses2 :: Parser Int
paretheses2 = do symbol "("
                 a <- multiplicationParser
                 symbol ")"
                 return a
                 <|>
                 atom
                 
parseLine2 :: String -> Int
parseLine2 xs = case (parse multiplicationParser xs) of
                [(n,[])]  -> n
                [(_,out)] -> error ("Invalid input near " ++ out)
                []        -> error "Invalid input"

                 
part2 :: [String] -> Int
part2 s = s |> map (filter (/=' ')) |> map parseLine2 |> sum








