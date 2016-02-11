


------------------------- Arithmetic expressions


data Aexp = Num Integer
           | Aexp :+: Aexp
           | Aexp :*: Aexp
           | Aexp :-: Aexp
             deriving (Eq,Show)


eval :: Aexp -> Integer
eval (Num n)   = n
eval (a :+: b) = eval a + eval b
eval (a :*: b) = eval a * eval b
eval (a :-: b) = eval a - eval b


------------------------- Exercise 1

n1 = Num 4 :*: Num (-6)

n2 = Num 0 :-: (Num 0 :-: Num 10)

n3 = undefined


------------------------- Exercise 2

natural :: Aexp -> Integer
natural (Num n)   = undefined
natural (a :+: b) = undefined
natural (a :*: b) = undefined
natural (a :-: b) = undefined


------------------------- State as a list

type Variable = String

type State = [(Variable,Integer)]


st :: State
st = [("x",3), ("y",5), ("z",0)]


------------------------- Exercise 3

empty :: State
empty = undefined

get :: Variable -> State -> Integer
get v [] = undefined
get v ((x,i):xs) = undefined

remove :: Variable -> State -> State
remove = undefined

set :: Variable -> Integer -> State -> State
set = undefined


------------------------- Exercise 4

-- n4 :: Aexp
-- n4 = (Var "x" :+: Num 6) :*: Var "y"

variables :: Aexp -> [Variable]
variables = undefined

evaluate :: Aexp -> State -> Integer
evaluate = undefined


------------------------- State as a function

type ST = Variable -> Integer


------------------------- Exercise 5

nil :: ST
nil v = undefined

one :: ST
one v | v == "a"  = undefined
      | otherwise = undefined

two :: ST
two v = undefined

putCto3 :: ST -> ST
putCto3 st v = undefined

three :: ST
three = putCto3 two

put :: Variable -> Integer -> ST -> ST
put = undefined

evalST :: Aexp -> ST -> Integer
evalST = undefined


