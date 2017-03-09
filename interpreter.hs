--Google FlexibleContexts
{-# Language PatternSynonyms, ViewPatterns, StandaloneDeriving, UndecidableInstances, DeriveFunctor, DeriveFoldable #-}

module Interpreter where
-- In Lambda Calculus when you define a lambda you give a name to the argument
-- Rather than using names a simpler way is every time you create a lambda it pushes another
-- argument on the stack, letting you refer to the binders index
-- e.g. 0 is the current lambda, 1 is the lambda before this one, etc
-- this is called a de brujin index

import Data.Foldable

data ExprF r
  = AppF r r
  | AbsF String r
  | VarF String
  | LitIF Int
  | AddF r r
  | MulF r r
  deriving (Show, Functor, Foldable)

newtype Fix f = Fix {unfix :: f (Fix f)}
deriving instance (Show (f (Fix f))) => Show (Fix f)

type Expr = Fix ExprF

pattern App a b = Fix (AppF a b)
pattern Abs a b = Fix (AbsF a b)
pattern Var a = Fix (VarF a)
pattern LitI a = Fix (LitIF a)
pattern Add a b = Fix (AddF a b)
pattern Mul a b = Fix (MulF a b)

add1NoPatternSynonyms :: Expr
add1NoPatternSynonyms = Fix $ AbsF "n" (Fix $ AddF (Fix $ VarF "n") (Fix $ LitIF 1))

add1 :: Expr
add1 = Abs "n" (Add (Var "n") (LitI 1))

add1to4 :: Expr
add1to4 = App add1 (LitI 4)

type Env = [(String, Expr)]

--evalReallyVerbose :: Env -> Expr -> Expr
--evalReallyVerbose e (App a' b') = let Abs i a = evalReallyVerbose a' in let b = evalReallyVerbose b' in ...

--evalVerbose was used on each argument before use
--to google: catamorphism
evalVerbose :: Env -> Expr -> Expr
evalVerbose e (App (evalVerbose e -> Abs i a)(evalVerbose e -> b)) = evalVerbose ((i, b) : e) a
evalVerbose e (Var (flip lookup e -> Just v)) = v
evalVerbose e (Add (evalVerbose e -> LitI a) (evalVerbose e -> LitI b)) = LitI (a + b)
evalVerbose e (Mul (evalVerbose e -> LitI a) (evalVerbose e -> LitI b)) = LitI (a * b)
evalVerbose _ a = a
--evalVerbose e a@(Abs _ _) = a
--evalVerbose _ a@(LitI v) = a


cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

data Value
  = VLitI Int
  | VFunc (Value -> Value)

instance Show Value where
  show (VLitI v) = show v
  show (VFunc _) = "Function"

type VEnv = [(String, Value)]

-- f = ExprF
-- a = (Env -> Expr)
-- so fa e will be evaluated and if it can then match a the = will run
evalAlg :: ExprF (VEnv -> Value) -> VEnv -> Value
evalAlg (AppF fa fb) e | (VFunc a) <- fa e = a (fb e)
evalAlg (AbsF i fb) e = VFunc (\v -> fb ((i, v) : e))
evalAlg (VarF i) e | Just v <- lookup i e = v
evalAlg (LitIF v) _ = VLitI v
evalAlg (AddF fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a + b)
evalAlg (MulF fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a * b)

eval' :: Expr -> Value
eval' x = cata evalAlg x []

litAlg :: ExprF [Int] -> [Int]
litAlg (LitIF v) = [v]
litAlg x = fold x


--cata litAlg add1to4

--catamorphism is a fold: you call the algebra on each one of the type from the inside outwards. With a list you call it on the last element (nil), then the cons before, then the cons before
--catamorphism just means its applied to every child -> 
