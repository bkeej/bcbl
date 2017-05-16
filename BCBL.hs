data Term =  S | K | Con Term Term
    deriving (Eq)

instance Show Term where
    show (Con x y) = "(" ++ show x ++ show y ++ ")"
    show S = "S"
    show K = "K"

reduce :: Term -> Term
reduce S = S
reduce K = K 
reduce (Con (Con K x) y) = reduce x
reduce (Con (Con (Con S x) y) z) = reduce (Con (Con x z) (Con y z))
reduce (Con x y) | (reduce x) == x && (reduce y) == y = (Con x y) 
                 {-- We stop reduceuation when it accomplishes nothing, allowing                     the computation to terminate (e.g., when x and y are K or S) --}  
                 | otherwise = reduce (Con (reduce x) (reduce y))
