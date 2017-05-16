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
                 {-- We stop reduceuation when it accomplishes nothing, allowing                     the computation to terminate (e.g., when x and y are K or S) --}  
                 | otherwise = reduce (Ap (reduce x) (reduce y))
