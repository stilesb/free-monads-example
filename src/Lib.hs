module Lib where

data Toy b next = Output b next
                | Bell next
                | Done

program0 = Output 'A' Done
-- λ > :t program0
-- example0 :: Toy Char (Toy b next)

program1 = Output 'A' (Bell Done)
-- λ > :t program1
-- example1 :: Toy Char (Toy b1 (Toy b next))

-- How do we keep our program type from constantly changing? Let's define
-- a type a that allows us to create a stream of functions.
data Cheat f = Cheat (f (Cheat f))

-- `Cheat` is actually already defined as `Fix'` which is the fixed point
-- of a functor so we'll use this instead
data Fix' f = Fix' (f (Fix' f))

program2 = Fix' (Output 'A' (Fix' Done))
-- λ > :t program2
-- program2 :: Fix' (Toy Char)

program3 = Fix' (Bell (Fix' (Output 'A' (Fix' Done))))
-- λ > :t program3
-- program3 :: Fix' (Toy Char)

-- Looks good so far, but this only works if we write code that always
-- terminates via `Done`. So how do we get around this functor chaining
-- limitation? One quick might be to throw an exception.
data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done
