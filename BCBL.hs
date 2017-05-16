data Term =  S | K | Ap Term Term
    deriving (Eq)

instance Show Term where
    show (Ap x y) = "(" ++ show x ++ show y ++ ")"
    show S = "S"
    show K = "K"

reduce :: Term -> Term
reduce S = S
reduce K = K 
reduce (Ap (Ap K x) y) = reduce x
reduce (Ap (Ap (Ap S x) y) z) = reduce (Ap (Ap x z) (Ap y z))
reduce (Ap x y) | (reduce x) == x && (reduce y) == y = (Ap x y) 
                | otherwise = reduce (Ap (reduce x) (reduce y))
                {-- We stop reduction when it has no effect, allowing 
                the computation to terminate (e.g., when x and y are K or S) --}

--Truth Values
t = K
f = Ap S K
