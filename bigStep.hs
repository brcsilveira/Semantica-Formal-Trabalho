
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | Twice C   ---- Executa o comando C 2 vezes
    | RepeatUntil C B --- Repeat C until B: executa C até que B seja verdadeiro
    | ExecN C E      ---- ExecN C n: executa o comando C n vezes
    | Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
    | DoWhile C B  --- DoWhile C B: executa C enquanto B for verdadeiro ------ NÃO ESTAVA AQUI, FOI ADICIONADO POR NÓS PORQUE PEDIA NO PROGRAMA DE EXEMPLO
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

-- exSigma :: Memoria
-- exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------




ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s) = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2, s)
   | ebigStep (e2, s) == 0 = error "Foi detectada uma divisão por zero"
   | otherwise = ebigStep (e1, s) `div` ebigStep (e2, s)



bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 
bbigStep (And b1 b2,s)
   | bbigStep (b1,s) == True && bbigStep (b2,s) == True = True
   | otherwise = False
bbigStep (Or b1 b2,s)  
   | bbigStep (b1,s) == False && bbigStep (b2,s) == False = False
   | otherwise = True
bbigStep (Leq e1 e2,s) 
   | ebigStep (e1,s) <= ebigStep (e2,s) = True
   | otherwise = False
bbigStep (Igual e1 e2,s)  -- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
   | ebigStep (e1,s) == ebigStep (e2,s) = True
   | otherwise = False

   
cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (While b c,s) -- ESSE NÃO ESTAVA COMENTADO, MAS VERIFICAMOS COM BASE NO ESCOPO QUE ELE ESTAVA FALTANDO
   | bbigStep (b,s) == True = let (_,s') = cbigStep (c,s) in cbigStep (While b c,s')
   | otherwise = (Skip,s)
cbigStep (If b c1 c2,s)
   | bbigStep (b,s) == True = cbigStep (c1,s)
   | otherwise = cbigStep (c2,s)  
cbigStep (Seq c1 c2,s)
   | c1 == Skip = cbigStep (c2,s)
   | otherwise = let (_,s') = cbigStep (c1,s) in cbigStep (c2,s') 
cbigStep (Atrib (Var x) e,s)
   | procuraVar s x == ebigStep (e,s) = (Skip,s)
   | otherwise = (Skip,mudaVar s x (ebigStep (e,s))) 
cbigStep (Twice c,s)   ---- Executa o comando C 2 vezes
   | c == Skip = (Skip,s)
   | otherwise = let (_,s') = cbigStep (c,s) in cbigStep (c,s') 
cbigStep (RepeatUntil c b,s)   --- Repeat C until B: executa C até que B seja verdadeiro
   | bbigStep (b,s) == True = (Skip,s)
   | otherwise = let (_,s') = cbigStep (c,s) in cbigStep (RepeatUntil c b,s') 
cbigStep (ExecN c e,s)      ---- ExecN C n: executa o comando C n vezes
   | ebigStep (e,s) == 0 = (Skip,s)
   | otherwise = let (_,s') = cbigStep (c,s) in cbigStep (ExecN c (Num (ebigStep (e,s) - 1)),s') 
cbigStep (Assert b c,s)   --- Assert B C: caso B seja verdadeiro, executa o comando C *** ESSE NÃO ESTAVA COMENTADO, MAS VERIFICAMOS COM BASE NO ESCOPO QUE ELE ESTAVA FALTANDO ***
   | bbigStep (b,s) == True = cbigStep (c,s)
   | otherwise = (Skip,s)
cbigStep (Swap (Var x) (Var y),s) --- recebe duas variáveis e troca o conteúdo delas
   | procuraVar s x == procuraVar s y = (Skip,s)
   | otherwise = (Skip,mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))
cbigStep (DAtrib (Var x) (Var y) e1 e2,s) -- Dupla atribuição: recebe duas variáveis x e y e duas expressões "e1" e "e2". Faz x:=e1 e y:=e2.
   | procuraVar s x == ebigStep (e1,s) && procuraVar s y == ebigStep (e2,s) = (Skip,s)
   | otherwise = (Skip,mudaVar (mudaVar s x (ebigStep (e1,s))) y (ebigStep (e2,s)))
--- DoWhile C B: executa C enquanto B for verdadeiro *** IMPLEMENTAMOS POIS FOI PEDIDO PARA PROGRAMA DE EXEMPLO ***
cbigStep (DoWhile c b, s) = 
   let (_, s') = cbigStep (c, s) 
   in cbigStep (If b (DoWhile c b) Skip, s')

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------


progtest1 :: C
progtest1 = While (Not (Igual (Var "x") (Num 0))) 
                  (Atrib (Var "x") (Sub (Var "x") (Num 1)))


progtest2 :: C -- Esse nós fizemos mais pra exercitar de o comando Atrib estava funcionando corretamente
progtest2 = Seq (Atrib (Var "temp") (Var "x")) 
                (Seq (Atrib (Var "x") (Var "y"))
                     (Atrib (Var "y") (Var "temp")))


progtest3 :: C
progtest3 = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 10))

progtest4 :: C
progtest4 = Seq 
    (DAtrib (Var "y") (Var "temp") (Num 1) (Var "x"))
    (RepeatUntil
        (Seq 
            (DAtrib (Var "y") (Var "temp") (Mult (Var "y") (Var "x")) (Sub (Var "x") (Num 1))) 
            (Atrib (Var "x") (Sub (Var "x") (Num 1))) 
        ) 
        (Igual (Var "x") (Num 1))
    )

-------------------------------------
-- Exemplos de memória para teste


exSigma :: Memoria
exSigma = [("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

exSigma3 :: Memoria
exSigma3 = [("x",5), ("y",0), ("temp",0)]

exSigma4 :: Memoria
exSigma4 = [("x",11)]

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))