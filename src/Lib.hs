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

-- How do we keep our program type from constantly changing? Let's define a type a that allows us to create a stream of functions.
data Cheat f = Cheat (f (Cheat f))

-- `Cheat` is actually already defined as `Fix` which is the fixed point of a functor
data Fix f = Fix (f (Fix f))

program2 = Fix (Output 'A' (Fix Done))
-- λ > :t program2
-- program2 :: Fix (Toy Char)

program3 = Fix (Bell (Fix (Output 'A' (Fix Done))))
-- λ > :t program3
-- program3 :: Fix (Toy Char)
