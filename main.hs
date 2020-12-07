import MyUtils
import Parsing
import Control.Monad
import Control.DeepSeq
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO.Unsafe
import Criterion.Main

--Data structure
data Bioperator = AND | OR | IMPL | EQUIV deriving (Show, Eq, Read)
data Term = Biop Term Bioperator Term | NOT Term | Atom String deriving (Show, Eq, Read)

--------------Processing input------------------
biParser :: String -> Bioperator -> Parser Term -> Parser Term
biParser inOp outOp next = do t <- next
                              do symbol inOp
                                 e <- (biParser inOp outOp next)
                                 return (Biop t outOp e)
                               <|> return t 

parseEquiv = biParser "<->" EQUIV parseImpl
parseImpl = biParser "->" IMPL parseOr
parseOr = biParser "|" OR parseAnd
parseAnd = biParser "&" AND parseNot

parseNot :: Parser Term
parseNot = do char '-'
              n <- paretheses
              return (NOT n)
            <|> paretheses

paretheses :: Parser Term
paretheses = do symbol "("
                e <- parseEquiv
                symbol ")"
                return e
               <|> parseAtom      
          
parseAtom :: Parser Term
parseAtom = do xs <- some alphanum
               return (Atom xs)

buildTerm :: String -> Term
buildTerm xs = case (parse parseEquiv xs) of
                [(n,[])]  -> n
                [(_,out)] -> error ("Invalid input near " ++ out)
                []        -> error "Invalid input"
                
buildTerms :: String -> [Term]
buildTerms s = s |> filter (\c-> not (c `elem` " \n")) |> splitOn ',' |> map buildTerm

--In order to parse DIMACS format simply convert it into a regular input
parseDIMACS :: [String] -> String
parseDIMACS s = s |> filter (\line -> not ('c' `elem` line)) |> concat |> addLogicToDIMACS |> ('(':)

addLogicToDIMACS :: String -> String
addLogicToDIMACS []             = []
addLogicToDIMACS " 0"           = ")"
addLogicToDIMACS (' ':('0':xs)) = ")&("++(addLogicToDIMACS xs)
addLogicToDIMACS (' ':xs)       = '|':(addLogicToDIMACS xs)
addLogicToDIMACS (x:xs)         = x:(addLogicToDIMACS xs)

--Checks if a DIMACS input was supplied, if so changes it to regular input
tryDIMACS :: [String] -> String
tryDIMACS s = if or (map (isInfixOf "p cnf") s) then parseDIMACS s else concat s

-------------Transforming into CNF--------------
data Element = TRUE String | FALSE String deriving (Show, Eq, Read)
type Clause = [Element]

getVar :: Element -> String
getVar (TRUE l)  = l
getVar (FALSE l) = l

--Step 1: eliminate <-> and ->

-- Remove all equivalence relations recursively
removeEquiv :: Term -> Term
removeEquiv (Atom s)   = (Atom s)
removeEquiv (NOT term) = NOT (removeEquiv term)
removeEquiv (Biop a EQUIV b) = Biop (Biop a' IMPL b') AND (Biop b' IMPL a') where
                                 a' = removeEquiv a
                                 b' = removeEquiv b
removeEquiv (Biop a op b) = Biop (removeEquiv a) op (removeEquiv b)

-- Remove all implication relations recursively
removeImpl :: Term -> Term
removeImpl (Atom s)   = (Atom s)
removeImpl (NOT term) = NOT (removeImpl term)
removeImpl (Biop a IMPL b) = Biop (NOT a') OR (b') where
                                 a' = removeImpl a
                                 b' = removeImpl b
removeImpl (Biop a op b) = Biop (removeImpl a) op (removeImpl b)

step1 :: Term -> Term
step1 x = x|> removeEquiv |> removeImpl

--Step 2: Put all negations in front of atoms
step2 :: Term -> Term
step2 (NOT (NOT a))        = step2 a
step2 (Atom s)             = (Atom s)
step2 (NOT (Atom s))       = (NOT (Atom s))
step2 (NOT (Biop a OR b))  = step2 (Biop (NOT a) AND (NOT b)) --Apply De Morgan 1
step2 (NOT (Biop a AND b)) = step2 (Biop (NOT a) OR (NOT b)) --Apply De Morgan 2
step2 (Biop a op b)        = Biop (step2 a) op (step2 b)
step2 (NOT a)              = error ("step2 called with NOT"++(show a))

--Step 3: Obtain the conjunctive normal form

--Checks if a given term can be turned into a clause
onlyOr :: Term -> Bool
onlyOr (Atom _)       = True
onlyOr (NOT (Atom _)) = True
onlyOr (Biop a OR b)  = (onlyOr a) && (onlyOr b)
onlyOr (Biop _ _ _)   = False
onlyOr (NOT _)        = error "onlyOr called before step2"

--Apply Distributivity_OR to a term, bringing the AND found up. Should only be applied after onlyOr returned False
distV :: Term -> Term
distV (Biop a OR (Biop b AND c)) = (Biop (Biop a OR b) AND (Biop a OR c))
distV (Biop (Biop b AND c) OR a) = (Biop (Biop a OR b) AND (Biop a OR c))
distV (Biop a OR b)              = distV (Biop (distV a) OR (distV b))
distV (NOT (Atom a))             = (NOT (Atom a)) 
distV (Atom a)                   = (Atom a)  
distV a                          = error ("Illegal parameter for distV: "++(show a))

step3 :: Term -> Term
step3 a | (onlyOr a)  = a
step3 (Biop a AND b)  = (Biop (step3 a) AND (step3 b))
step3 a               = step3 (distV a)

clausify :: Term -> [Clause]
clausify (Biop a AND b) = (clausify a)++(clausify b)
clausify a              = [buildClause a]

buildClause :: Term -> Clause
buildClause (Atom s) = [TRUE s]
buildClause (NOT (Atom s)) = [FALSE s]
buildClause (Biop a OR b)  = (buildClause a)++(buildClause b)


---------------Resolution calculus----------------

--Represents: clause, next clause to check against, and clauses it's derived from.
type ClauseData = (Clause, Int, Int, Int)

unwrap :: ClauseData -> Clause
unwrap (c,_,_,_) = c

wrapPremise :: Clause -> ClauseData
wrapPremise c  = (sortClause c,0,-1,-1)

--This will sort elements in a clause by the string, allowing for way faster comparison later
sortClause :: Clause -> Clause
sortClause = sortOn getVar

resolvable :: Clause -> Clause -> Bool
resolvable [] [] = False
resolvable _ [] = False
resolvable [] _ = False
resolvable (x:xs) [y] = x `opposite` y || (resolvable xs [y])
resolvable xs (y:ys) = (resolvable xs [y]) || (resolvable xs ys)

--Resolves two clauses. They should already be resolvable
resolve :: Clause -> Clause -> Clause
resolve [] ys         = ys
resolve (x:xs) ys     = case x `removeOppositeFrom` ys of
   Nothing  -> x:(resolve xs ys)
   (Just c) -> (resolve xs c)

--Removes the opposite of element 'e' from a clause. The boolen indicated whether anything has been removed
removeOppositeFrom :: Element -> Clause -> Maybe Clause
removeOppositeFrom e [] = Nothing
removeOppositeFrom e (x:xs) = if e `opposite` x then Just xs else case e `removeOppositeFrom` xs of
   (Just c) -> Just (x:c)
   Nothing  -> Nothing

opposite :: Element -> Element -> Bool
opposite (TRUE a) (FALSE b) = a==b
opposite (FALSE a) (TRUE b) = a==b
opposite _ _                = False

--Resolves a clause with each clause from a set of clauses. Return new all clauses created (might be [])
resolveWith :: Clause -> [Clause] -> [Clause]
resolveWith c [] = []
resolveWith c (x:xs) = if resolvable c x then (resolve c x):(resolveWith c xs) else resolveWith c xs

--Generates all new clauses from a chosen clause. Stores all (old and new) in the result
makeClausesFrom :: Int -> [ClauseData] -> [ClauseData]
makeClausesFrom n cs = case cs!!n of
   (c,next,d1,d2) | next==(length cs) -> cs
   (c,0,d1,d2) -> makeClausesFrom n (setNext (n+1) n cs)
   (c,next,d1,d2) | resolvable c (unwrap(cs!!next)) -> makeClausesFrom n (setNext (next+1) n newClauses) where
      newClauses = if (isTautology newClause) || (clauseExists newClause cs) then cs else cs++[(newClause, 0, n, next)]
      newClause = resolve c (unwrap(cs!!next)) |> unique |> sortClause
   (c,next,d1,d2) -> makeClausesFrom n (setNext (next+1) n cs)

--Checks if a clause already exists in a set   
clauseExists :: Clause -> [ClauseData] -> Bool
clauseExists c cs = length (filter (\(c2,_,_,_) -> c2==c) cs) >0

--Checks if a clause is a tautology, i.e. it contains both a literal and its negation
isTautology :: Clause -> Bool
isTautology [] = False
isTautology (x:xs) = if (removeOppositeFrom x xs)==Nothing then isTautology xs else True

notTautology :: Clause -> Bool
notTautology c = not (isTautology c)
   
--Sets the 'next' value from a specific clause in a set. Value -> Index -> Set -> Set with one modified clause
setNext :: Int -> Int -> [ClauseData] -> [ClauseData]
setNext value index cs = (take index cs) ++ [(c,value,d1,d2)] ++ (drop (index+1) cs)
   where (c,_,d1,d2) = cs!!index

--Resolves a set of clauses, generating all possible clauses that can be made from them   
resolution :: Int -> [ClauseData] -> [ClauseData]
resolution n cs | n==(length cs) = cs
resolution n cs | [] `elem` (map unwrap cs)   = cs --If derived box finish
resolution n cs = if (length newClauses)>(length cs) then resolution 0 newClauses else resolution (n+1) newClauses
   where newClauses = makeClausesFrom n cs

-----------Building a proof-----------
data Proof = Premise Clause | Dependent Clause Proof Proof deriving (Show, Eq, Read)

buildProof :: [ClauseData] -> ClauseData -> Proof
buildProof cs (c,_,-1,-1) = Premise c
buildProof cs (c,_,d1,d2) = Dependent c (buildProof cs (cs!!d1)) (buildProof cs (cs!!d2))

findBox :: [ClauseData] -> Maybe ClauseData
findBox []                 = Nothing
findBox (([],id,d1,d2):xs) = Just ([],id,d1,d2)
findBox (x:xs)             = findBox xs
   
--Returns a proof if clauses contain [], otherwise Nothing   
part2 :: [ClauseData] -> Maybe Proof
part2 cs = case findBox cs of
   Nothing  -> Nothing
   Just box -> Just (buildProof cs box)

------------------DPLL-------------------
type Model = Clause --It's stored in the same way, renaming to make it clearer

--Finds all variables in a set of clauses
extractVariables :: [Clause] -> [String]
extractVariables cs = cs |> map (map getVar) |> concat |> unique

dpll :: [Clause] -> [String] -> Model -> Maybe Model
dpll cs vs m | (length m)==(length vs)  = if models cs m then Just m else Nothing
dpll cs vs m | [] `elem` cs             = Nothing --Model is wrong if empty clause was found
dpll cs vs m                            = case find (\c->(length c)==1) cs of --Unit propagation
   Just [l] -> dpll (removeLiteral l cs) vs (l:m)  
   Nothing  -> case pureLiteral cs vs of --Pure literal
      Just l  -> dpll (filter (\c-> not (l `elem` c)) cs) vs (l:m)
      Nothing -> (case dpll (removeLiteral (TRUE newV) cs) vs ((TRUE newV):m) of --Splitting
         Just model -> Just model
         Nothing    -> dpll (removeLiteral (FALSE newV) cs) vs ((FALSE newV):m)) where
            newV = nextVariable vs m

--Removes a literal from a set of clauses, i.e. removes clauses with it and removes its negations from the rest
removeLiteral :: Element -> [Clause] -> [Clause]
removeLiteral l cs = cs |> filter (\c-> not (l `elem` c)) |> map (filter (\e-> not(opposite e l)))

--Pick the first variable in a list that's not already in the model
nextVariable :: [String] -> Model -> String
nextVariable [] m = error "Could find a new variable"
nextVariable (v:vs) m = if ((TRUE v) `elem` m) || ((FALSE v) `elem` m) then nextVariable vs m else v
   
--Finds a pure literal in a set of clauses. Returns that literal if found
-- Clauses -> Variables -> Maybe the pure literal
pureLiteral :: [Clause] -> [String] -> Maybe Element
pureLiteral [] vs  = Nothing
pureLiteral _ []   = Nothing
pureLiteral cs (v:vs) = if isTrue && (not isFalse) then Just (TRUE v) else
   if (not isTrue) && isFalse then Just (FALSE v) else pureLiteral cs vs where
      isTrue  = (exists (\c-> (TRUE v) `elem` c) cs)
      isFalse = (exists (\c-> (FALSE v) `elem` c) cs)

   
-- For each clause checks if the model models that clause (i.e. they have a common element)
models :: [Clause] -> Model -> Bool
models [] m = True
models (x:xs) m = (exists (\e -> e `elem` m) x) && (models xs m)

----------------Combining functions------------------------

clausesFromString :: String -> [Clause]
clausesFromString s = s |> buildTerms |> map step1 |> map step2 |> map step3 |> map clausify |> concat |> unique |> filter notTautology

proofFromClauses :: [Clause] -> Maybe Proof
proofFromClauses cs = cs |> map wrapPremise |> resolution 0 |> part2

dpllFromClauses :: [Clause] -> Maybe Model
dpllFromClauses cs = cs |> (\cs->dpll cs (extractVariables cs) [])

----------------Benchmarking-----------------
 
benchmark clauses = defaultMain [
  bgroup "resolution calculus" [ bench ""  $ whnf proofFromClauses clauses],
  bgroup "dpll" [ bench ""  $ whnf dpllFromClauses clauses]
  ]


-----------Access functions-----------------

resolutionOnString :: String -> Maybe Proof
resolutionOnString s = s |> clausesFromString |> proofFromClauses -- |> showProof

dpllOnString :: String -> Maybe Model
dpllOnString s = s |> clausesFromString |> dpllFromClauses

resolutionOnFile :: String -> IO ()
resolutionOnFile file = runOnFile (\lines -> lines |> tryDIMACS |> resolutionOnString) file -- |> showProof

dpllOnFile :: String -> IO ()
dpllOnFile file = runOnFile (\lines -> lines |> tryDIMACS |> dpllOnString ) file

compareTime :: String -> IO ()
compareTime file = do  
        contents <- readFile file
        contents |> clausesFromString |> benchmark 


---------------Debug versions--------------------
--Haskell really doesn't like global variables or pritns, so having a global flag to enable prints wouldn't work very well (or at all)
--Instead, for the purpose of clearly showing the inner workings of the program, some functions have been re-writen here to include prints

tracedClausesFromString :: String -> [Clause]
tracedClausesFromString s = 
   let terms = buildTerms s in
   let step1res = trace ("\nParsed input: "++(show terms)) (map step1 terms) in
   let step2res = trace ("\nRemoved <-> and ->: "++(show step1res)) (map step2 step1res) in
   let step3res = trace ("\nMoved negations to atoms: "++(show step2res)) (map step3 step2res) in
   let clausifyres = trace ("\nPulled AND operators up: "++(show step3res)) (map clausify step3res |> concat |> unique) in
   let final = trace ("\nCreated clauses: "++(show clausifyres)) (filter notTautology clausifyres) in
   trace ("\nRemoved tautologies: "++(show final)) final
   
tracedResolution :: Int -> [ClauseData] -> [ClauseData]
tracedResolution n cs | n==(length cs) = 
   trace ("\nGenerated clauses "++(show cs)++"\nNo more clauses left to resolve\n") cs
tracedResolution n cs | [] `elem` (map unwrap cs)   = trace 
   ("\nGenerated clauses "++(show cs)++"\nBox found, generating proof:\n") cs
tracedResolution n cs = if (length newClauses)>(length cs) then tracedResolution 0 newClauses 
   else tracedResolution (n+1) newClauses
      where newClauses = makeClausesFrom n cs
    
tracedProofFromClauses :: [Clause] -> Maybe Proof
tracedProofFromClauses cs = ---cs |> map wrapPremise |> resolution 0 |> part2    
   let wrapped  = map wrapPremise cs in
   let resolved = trace ("\nAdded extra data for resolution: "++(show wrapped)) (tracedResolution 0 wrapped) in
   part2 resolved
      
tracedResolutionOnFile :: String -> IO ()
tracedResolutionOnFile file = runOnFile (\lines -> lines |> tryDIMACS |> tracedClausesFromString |> 
    tracedProofFromClauses) file
    


tracedDpll :: [Clause] -> [String] -> Model -> Maybe Model
tracedDpll cs vs m | (length m)==(length vs)  = if models cs m then trace "Model found:" (Just m) 
   else trace ("Model "++(show m)++" incorrect, backtracking") Nothing
tracedDpll cs vs m | [] `elem` cs             = trace ("Model "++(show m)++" incorrect, backtracking") Nothing
tracedDpll cs vs m                            = case find (\c->(length c)==1) cs of --Unit propagation
   Just [l] -> trace ("Unit propagation on "++(show l)) (tracedDpll (removeLiteral l cs) vs (l:m))  
   Nothing  -> case pureLiteral cs vs of --Pure literal
      Just l  -> trace ("Pure literal: "++(show l)) (tracedDpll (filter (\c-> not (l `elem` c)) cs) vs (l:m))
      Nothing ->( trace ("Splitting, trying "++(show(TRUE newV))) 
         (case tracedDpll (removeLiteral (TRUE newV) cs) vs ((TRUE newV):m) of
            Just model -> Just model
            Nothing    -> trace ("Trying "++(show(FALSE newV))) (tracedDpll (removeLiteral (FALSE newV) cs) vs ((FALSE newV):m)))) where
               newV = nextVariable vs m

tracedDpllFromClauses :: [Clause] -> Maybe Model
tracedDpllFromClauses cs = cs |> (\cs->tracedDpll cs (extractVariables cs) [])

tracedDpllOnFile :: String -> IO ()
tracedDpllOnFile file = runOnFile (\lines -> lines |> tryDIMACS |> tracedClausesFromString |> tracedDpllFromClauses) file

help :: IO ()
help = do
   putStrLn "Available functions are:"
   putStrLn "resolutionOnFile file - runs the propositional resolution algorithm"
   putStrLn "dpllOnFile file - runs the DPLL algorithm"
   putStrLn "tracedResolutionOnFile file - runs resolutionOnFile and displays debug information"
   putStrLn "tracedDpllOnFile file - runs dpllOnFile and displays debug information"
   putStrLn "compareTime file - benchmarks resolution and DPLL"
   putStrLn "Parameters are Haskell strings and must use quotation marks, e.g. compareTime \"input.txt\""
   putStrLn "\nInput format"
   putStrLn "\nFile be either is DIMACS format or in the format specified in the exercise, that is"
   putStrLn "Using binary operators <->,->,&,|"
   putStrLn "Using unary operator - (meaning not)"
   putStrLn "Potentially using parentheses and separating terms with ,"
   putStrLn "Atoms fitting the regex [A-Z]+[0-9]* (though in this program this requirement in relaxed and all alphanumeric characters will be accepted)"
   putStrLn "\nOutput"
   putStrLn "Both resolution and dpll will output \"Nothing\" if they don't find a refutation/model"
   putStrLn "\nRefutation tree is constructed using the following grammar:"
   putStrLn "P ::= (Dependant Clause P P) | Premise Clause"
   putStrLn "In the first case, the clause is being proven by this proof, and the two other Ps are proofs for its dependencies, or clauses that when resolved together proove it"
   putStrLn "In the second case the clause was obtained from input and does not need proof"
   putStrLn "\nOuput of DPLL simply states the truth values for each variable in the model"
   putStrLn "\nFunctions with names starting from 'trace' will produce details of how they arrive at their conclusion"
   putStrLn "Here are some details about values in the output:"
   putStrLn "Logic expressions are defined by a grammar:"
   putStrLn "Bioperator ::= AND | OR | IMPL | EQUIV "
   putStrLn "Term ::== Biop Term Bioperator Term | NOT Term | Atom String"
   putStrLn "Clauses are just sets of variables that are either true or false"
   putStrLn "For resolution, clauses also store indexes of clauses they were derived from as their third and fourth field"












