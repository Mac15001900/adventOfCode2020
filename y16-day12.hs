import MyUtils
import Text.Read

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input2016-12.txt"

data Registry = Reg Int Int Int Int deriving (Show, Eq, Read)
data Result = Res Registry Int deriving (Show, Eq, Read) --New registry and the next instruction to execute (relatively)
data Instruction = CPY Value RegId | INC RegId | DEC RegId | JNZ Value Value deriving (Show, Eq, Read)
data Value = Atom Int | RegValue RegId deriving (Show, Eq, Read)
data RegId = A | B | C | D deriving (Show, Eq, Read)

execute :: Registry -> Instruction -> Result
execute reg (CPY v id) = Res (registrySet reg id (evalValue reg v)) 1
execute reg (INC id) = Res (registrySet reg id ((registryGet reg id)+1)) 1
execute reg (DEC id) = Res (registrySet reg id ((registryGet reg id)-1)) 1
execute reg (JNZ v1 v2) = if a==0 then Res reg 1 else Res reg b where 
    a = evalValue reg v1
    b = evalValue reg v2
    
executeAll :: Registry -> [Instruction] -> Int -> Registry
executeAll r i p = if p >= (length i) then r else executeAll r' i (p+dp) where
    Res r' dp = execute r (i!!p)


evalValue :: Registry -> Value -> Int
evalValue _ (Atom x) = x
evalValue reg (RegValue id) = registryGet reg id

registryGet :: Registry -> RegId -> Int
registryGet (Reg a _ _ _) A = a
registryGet (Reg _ b _ _) B = b
registryGet (Reg _ _ c _) C = c
registryGet (Reg _ _ _ d) D = d

registrySet :: Registry -> RegId -> Int -> Registry
registrySet (Reg a b c d) A v = Reg v b c d
registrySet (Reg a b c d) B v = Reg a v c d
registrySet (Reg a b c d) C v = Reg a b v d
registrySet (Reg a b c d) D v = Reg a b c v

parseInstruction :: [String] -> Instruction
parseInstruction ("CPY":a:[b]) = CPY (parseValue a) (read b)
parseInstruction ("INC":[a]) = INC (read a)
parseInstruction ("DEC":[a]) = DEC (read a)
parseInstruction ("JNZ":a:[b]) = JNZ (parseValue a) (parseValue b)

parseValue :: String -> Value
parseValue s = case readMaybe s :: Maybe Int of
    Just n -> Atom n
    Nothing -> RegValue (read s)


runAllFrom :: Registry -> [String] -> Registry
runAllFrom reg input = input |> map (splitOn ' ') |> map parseInstruction |> (\i -> executeAll reg i 0)

part1 :: [String] -> Registry
part1 = runAllFrom (Reg 0 0 0 0)

part2 :: [String] -> Registry
part2 = runAllFrom (Reg 0 0 1 0)




