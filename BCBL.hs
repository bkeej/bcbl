data Term =  S | K | Con Term Term
    deriving (Eq)

instance Show Term where
    show (Con x y) = "(" ++ show x ++ show y ++ ")"
    show S = "S"
    show K = "K"

eval :: Term -> Term
eval S = S
eval K = K 
eval (Con (Con K x) y) = eval x
eval (Con (Con (Con S x) y) z) = eval (Con (Con x z) (Con y z))
eval (Con x y) | (eval x) == x && (eval y) == y = (Con x y) 
               {-- We stop evaluation when it accomplishes nothing. This allows the                computation to terminate (e.g., when x and y are K or S) --}  
               | otherwise = eval (Con (eval x) (eval y))
