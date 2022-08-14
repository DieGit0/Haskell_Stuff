{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module LeMonade where

data Expr = Val Int | Div Expr Expr

--Math          Haskell
-- 1            Val 1
-- 6 / 2        Div 6 2
-- 6 / (3 / 1)  ?

-- Unsafe
eval :: Expr ->  Int
eval (Val n)   = n
eval (Div x y) = div (eval x ) (eval y)
eval (Div a (Div b c)) = div (eval a) (div (eval b) (eval c)) -- 6 / (3 / 1)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                       Nothing -> Nothing
                       Just n -> case eval' y of
                                      Nothing -> Nothing
                                      Just m -> safeDiv n m

--The same program however less verbose
eval'' :: Expr -> Maybe Int
eval'' (Val n)   = return n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safeDiv n m

-- The same using a monadic do notation
eval_ :: Expr -> Maybe Int
eval_ (Val n)   = return n
eval_ (Div x y) = do
                   m <- eval_ x
                   n <- eval_ y
                   safeDiv m n
eval_ (Div a (Div b c)) = do
                           x <- eval_ a
                           y <- eval_ b
                           z <- eval_ c
                           safeDiv x y >>= safeDiv z -- 6 / (3 / 1)


-- Just 50 >>= (\a -> safeDiv a 10 )
-- What's the Point:
-- 1) Same idea works for other effects : I/O, readind from environments, writing a Log; mutable states, non-determinism
-- 2) Support pure programming with effects (deal with fails)
-- 3) Use effects explicity in types
-- 4) Functions that works for any effects

main :: IO ()
main = do
       print $ eval (Val 5)
       print $ eval (Div (Val 5) (Val 2))
       print $ eval (Div (Val 50) (Div (Val 10) (Val 2)))
       putStrLn []
       print $ eval' (Val 10)
       print $ eval' (Div (Val 5) (Val 0))
       putStrLn []
       print $ eval'' (Val 20)
       print $ eval'' (Div (Val 5) (Val 0))
       putStrLn []
       print $ eval_ (Val 30)
       print $ eval_ (Div (Val 5) (Val 0))
       print $ eval_ (Div (Val 50) (Div (Val 10) (Val 2)))            
