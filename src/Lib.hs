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

-- Looks good, but this only works if we write code that always terminates
-- via `Done`. One quick way to get around this functor chaining
-- limitation is to throw an exception.
data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

data IncompleteException = IncompleteException

type Subroutine = FixE (Toy Char) IncompleteException
type Program = FixE (Toy Char) IncompleteException

-- output 'A'
-- throw IncompleteException
subroutine0 :: Subroutine
subroutine0 = Fix (Output 'A' (Throw IncompleteException))

-- try {
--     subroutine0
-- } catch (IncompleteException) {
--     bell
--     done
-- }
program4 :: Program
program4 = subroutine0 `catch` (\_ -> Fix (Bell (Fix Done)) :: FixE (Toy Char) e)

subroutine1 :: Subroutine
subroutine1 = Fix (Output 'A' (Fix (Output 'B' (Fix (Output 'C' (Throw IncompleteException))))))

program5 = subroutine1 `catch` (\_ -> Fix (Bell (Fix Done)) :: FixE (Toy Char) e)

-- Amazing! This works well for us. A few gripes:
--
-- 1. Exceptions are not truly "Exceptional"
-- 2. Some redundant code with `Program` and `Subroutine` being synonyms for the same type
--
-- How do we solve these issues?
--
-- As it turns out, `FixE` is actually a Free Monad. Let's look more
-- closely to see what we can get out of looking around here.
data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Applicative (Free f) where
  pure               = Pure
  Pure a <*> Pure b  = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b      = Free $ (<*> b) <$> ma

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free fa) = Free (fmap f <$> fa)

instance Functor f => Monad (Free f) where
    return         = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
