


  ------------------------- Arithmetic expressions


  data Aexp = Num Integer
             | Var Variable
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
  empty = []

  get :: Variable -> State -> Integer
  get _ [] = 0
  get v ((x,i):xs) | x == v  = i
                   | otherwise = get v xs

  remove :: Variable -> State -> State
  remove v [] = []
  remove v (x:xs) | fst x == v = xs
                  | otherwise = x : remove v xs

  set :: Variable -> Integer -> State -> State
  set v x s = (v,x) : remove v s


  ------------------------- Exercise 4

  n4 :: Aexp
  n4 = (Var "x" :+: Num 6) :*: Var "y"

  variables :: Aexp -> [Variable]
  variables (Var n)   = [n]
  variables (Num n)   = []
  variables (a :+: b) = (variables a) ++ (variables b)
  variables (a :*: b) = (variables a) ++ (variables b)
  variables (a :-: b) = (variables a) ++ (variables b)

  evaluate :: Aexp -> State -> Integer
  evaluate (Var n) s   = get n s
  evaluate (Num n) _   = n
  evaluate (a :+: b) s = (evaluate a s) + (evaluate b s)
  evaluate (a :*: b) s = (evaluate a s) * (evaluate b s)
  evaluate (a :-: b) s = (evaluate a s) - (evaluate b s)



  ------------------------- State as a function

  type ST = Variable -> Integer


  ------------------------- Exercise 5

  nil :: ST
  nil v = 0

  one :: ST
  one v | v == "a"  = 1
        | otherwise = nil v

  two :: ST
  two v | v == "b" = 2
        | otherwise = one v 

  putCto3 :: ST -> ST
  putCto3 st v | v == "c" = 3
               | otherwise = st v

  three :: ST
  three = putCto3 two

  put :: Variable -> Integer -> ST -> ST
  put v i s w | v == w = i
              | otherwise = st w

  evalST :: Aexp -> ST -> Integer
  evalST = undefined


