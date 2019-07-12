---------------------------------------------------------------------------
--data type
data Token = Num Double | Op String | Err String
  deriving (Eq, Show)

---------------------------------------------------------------------------
--section for evaluate the postfix expression
calc :: String -> String
calc []         = "Empty Expression"
calc expression = parseTokens [] (map tokenize $ words expression)


parseTokens :: [Token] -> [Token] -> String
parseTokens [] []              = "empty Stack"
parseTokens _ ((Err e):xs)     = e
parseTokens ((Err e):xs) _     = e
parseTokens [(Num n)] []       = show n
parseTokens ((Num n):xs) []    = "Invalid Expression"
parseTokens [] ((Num n):xs)    = parseTokens [Num n] xs
parseTokens stack ((Num n):xs) = parseTokens ([Num n] ++ stack) xs
parseTokens stack ((Op p):xs)  = parseTokens (applyOp stack p) xs


applyOp :: [Token] -> String -> [Token]
applyOp [] op
          | op == "clear" = []
          | otherwise     = [Err (op ++ ": empty stack error")]
applyOp [Num n] op 
                    | (or (map (\i -> op == i) ["+","*","-","/", "swap"])) = ([Err (op ++ ": not enough arguments")])
applyOp ((Num n):xs) op
                    | op == "inc"   = (Num (n+1)):xs
                    | op == "dec"   = (Num (n-1)):xs
                    | op == "sqrt"  = (Num (sqrt n)):xs
                    | op == "sin"   = (Num (sin n)):xs
                    | op == "cos"   = (Num (cos n)):xs
                    | op == "inv"   = (Num (1/n)):xs
                    | op == "dup"   = (Num n):(Num n):xs
                    | op == "pop"   = xs
                    | op == "clear" = []
                    | op == "+all"  = [Num (plusAll ((Num n):xs))]
                    | op == "*all"  = [Num (multiplyAll ((Num n):xs))]         
applyOp ((Num n1):(Num n2):xs) op
                    | op == "+"    = (Num (n1 + n2)):xs
                    | op == "*"    = (Num (n1 * n2)):xs
                    | op == "-"    = (Num (n2 - n1)):xs
                    | op == "/"    = (Num (n2 / n1)):xs
                    | op == "swap" = (Num n2):(Num n1):xs

--helper function for applyOP
plusAll :: [Token] -> Double
plusAll []           = 0
plusAll ((Num n):xs) = n + plusAll xs

multiplyAll :: [Token] -> Double
multiplyAll []           = 1
multiplyAll ((Num n):xs) = n * multiplyAll xs



---------------------------------------------------------------------------
-- section for tokenize the expression
tokenize :: String -> Token
tokenize (x:xs)
    | validOperator (x:xs) == True = Op (x:xs)
    | x == '-'                     = if validDouble (xs) then Num (read (x:xs) :: Double) else Err ((x:xs) ++ ": illegal token")
    | validDouble (x:xs)           = Num (read (x:xs) :: Double) 
    | otherwise                    = Err ((x:xs) ++ ": illegal token")

--check if token is operator
validOperator :: String -> Bool
validOperator s
              | or (map (\op -> s == op) ["inc","dec","sqrt","sin","cos","inv","+","*","-","/","+all","*all","dup","pop","clear","swap"]) = True
              | otherwise = False

--check if token is a double number
validDouble :: String -> Bool
validDouble [c] = validInteger [c]
validDouble s
            | (last s) == '.' = False
            | s1 == []        = False
            | otherwise       = and [validInteger s1, validInteger s2]
                where s1 = head (split '.' [] s)
                      s2 = head $ tail (split '.' [] s)

validInteger:: String -> Bool
validInteger [] = True 
validInteger (x:xs)
                | or (map (\n -> x == n) ['1','2','3','4','5','6','7','8','9','0']) = validInteger xs
                | otherwise = False

split :: Char -> String -> String -> [String]
split delim s1 s2
            | s2 == []           = [s1, s2]
            | (head s2) == delim = [s1, rest]
            | otherwise          = split delim (s1 ++ [(head s2)]) rest
            where
              rest = tail s2



---------------------------------------------------------------------------
--Section for calcStack
calcStack :: String -> String
calcStack e = "Top of Stack -> " ++ (tokenListToString $ map tokenize $ words e)

tokenListToString :: [Token] -> String
tokenListToString []     = ""
tokenListToString (x:xs) = "<" ++ (tokenToString x) ++ "> " ++(tokenListToString xs)

tokenToString :: Token -> String
tokenToString (Op x)  =  "Op " ++ x
tokenToString (Err x) = "Err " ++ x
tokenToString x       = show x
