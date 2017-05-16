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
