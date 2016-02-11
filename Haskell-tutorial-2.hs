


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

n3 = Num 3 :*: (Num 4 :+: Num 5)

n4 = Num 3 :-: Num 4


------------------------- Exercise 2

natural :: Aexp -> Integer
natural (Num n)   = if n > 0
	                then n
	                else 0
natural (a :+: b) = eval a + eval b
natural (a :*: b) = eval a * eval b
natural (a :-: b) = if eval a - eval b < 0
	                then 0
	                else eval a - eval b


------------------------- State as a list

type Variable = String

type State = [(Variable,Integer)]


st :: State
st = [("x",3), ("y",5), ("z",0)]


------------------------- Exercise 3

empty :: State
empty = [("",2)]

get :: Variable -> State -> Integer
get v [] = 0
get v ((x,i):xs) = if v == x
	               then i
	               else get v (xs)

remove :: Variable -> State -> State
remove v (x,i) = (v,i) 
remove v ((x,i):xs) = if (get v ((x,i):xs) == 0)
                      then 0
                      else remove v (x,i)

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


