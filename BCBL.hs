import Data.List

{-- An implementation of the SK Calculus --}

--The language of SK combinators
data Term =  S | K | Ap Term Term
    deriving Eq

instance Show Term where
    show (Ap x y) = "(" ++ show x ++ show y ++ ")"
    show S = "S"
    show K = "K"

--Evaluating SK combinator expressions via pattern matching
reduce :: Term -> Term
reduce S = S
reduce K = K 
reduce (Ap (Ap K x) y) = reduce x
reduce (Ap (Ap (Ap S x) y) z) = reduce (Ap (Ap x z) (Ap y z))
reduce (Ap x y) | (reduce x) == x && (reduce y) == y = (Ap x y) 
                | otherwise = reduce (Ap (reduce x) (reduce y))
                -- We stop reduction when it has no effect, allowing 
                -- the computation to terminate (e.g., when x and y are K or S)

{-- We will provide a model for Propositional Logic in the SK Calculus.
    Truth values are distinguished expressions of the calculus, namely t and f. 
    Truth functions map truth values to truth values in the usual way. --}

--Truth values in SK combinator guise 
type Tvalue = Term
t = K
f = Ap S K

--Truth functions for connectives
skNOT :: Tvalue -> Tvalue
skNOT x = reduce (Ap (Ap x f) t)

skOR :: Tvalue -> Tvalue -> Tvalue
skOR x y = reduce (Ap (Ap x t) y)

{-- We define the language of propositional logic and
    interpret it in the SK calculus. --}

--Language of propositional logic
data Form = At String | Ng Form | Dsj Form Form
    deriving Eq

instance Show Form where
    show (At a) = a
    show (Ng f) = "~" ++ show f
    show (Dsj f1 f2) = "(" ++ show f1 ++ "v" ++ show f2 ++ ")"


--Get every atomic formula in a formula
atoms :: Form -> [String]
atoms (At name) = [name]
atoms (Ng f)   = atoms f
atoms (Dsj f1 f2) = (sort.nub.concat) (map atoms [f1,f2])

--Get every SK model for some list of atomic formula
models :: [String] -> [[(String,Tvalue)]]
models [] = [[]]
models (name:names) = map ((name,t) :) (models names)
                    ++ map ((name,f):) (models names)

allModels :: Form -> [[(String,Tvalue)]]
allModels = models . atoms


--The intepretation function maps an SK model and a formula to a SK truth value using the SK connectives as the interpretation of Ng and Dsj.
interp :: [(String,Tvalue)] -> Form -> Tvalue
interp [] (At at) = error ("No info on " ++ show at)
interp ((i,v):xs) (At at)
    | at == i = v
    | otherwise = interp xs (At at)
interp xs (Ng f) = skNOT (interp xs f)
interp xs (Dsj f1 f2) = skOR (interp xs f1) (interp xs f2)   

--Tarskian Truth Definition, namely a formula is True in an SK model just in case it is mapped to K by the interpretation function, else it is false.
truth :: [(String,Tvalue)] -> Form -> Bool
truth xs f | interp xs f == t = True
           | otherwise = False

