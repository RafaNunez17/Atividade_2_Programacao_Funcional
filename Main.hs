-- ALUNO: Rafael Vitagliano Tannenbaum Nuñez

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um
inteiro uma unidade maior que a entrada.
-}

soma1 :: Int -> Int
soma1 x = x + 1



------------------------
{-
2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva
sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
-}

sempre :: Int -> Int
sempre v = 0



------------------------
{-
3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com
precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
-}

treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z



------------------------
{-
4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números
inteiros
-}

resto :: Int -> Int -> Int
resto x y = mod x y



------------------------
{-
5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores
monetários.
-}

precoMaior :: Int -> Int -> Int -> Int -> Int
precoMaior x y z w
  | x > y && x > z && x > w = x
  | y > x && y > z && y > w = y
  | z > x && z > y && z > w = z
  | otherwise = z



------------------------
{-
6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.

Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva Escreva 
uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
-}

impar :: Int -> Int -> Bool
impar x y
  | mod z 2 == 1 = True
  | otherwise = False
  where
    z = x * y

par :: Int -> Int -> Int
par x y = x + y



--------------------------
{-
7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado
da equação 𝑥^2 + 𝑦/2 +𝑧.
-}

conta :: Double -> Double -> Double -> Double
conta x y z = x ^ 2 + y / 2 + z



--------------------------
-- ATIVIDADE 8
{-
Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima  um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br). Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.
-}

diagnostico :: Double -> Double -> String
diagnostico peso altura
  | imc < 17 = "Muito abaixo do peso"
  | imc >= 17 && imc < 18.49 = "Abaixo do peso"
  | imc >= 18.5 && imc < 24.99 = "Peso Normal"
  | imc >= 25 && imc < 29.99 = "Sobrepeso"
  | imc >= 30 && imc < 34.99 = "Obesidade Leve"
  | imc >= 35 && imc < 39.99 = "Obesidade Severa"
  | otherwise = "Obesidade Mórbida"
  where
    imc = peso / altura ^ 2



--------------------------
-- ATIVIDADE 9
{-
Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
-}

bissexto :: Int -> Bool
bissexto ano
  | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
  | otherwise = False



main = do
  putStrLn $ "Func. 1; entrada: 1; resultado = " ++ show (soma1 1)
  putStrLn $ "Func. 2; entrada: 10; resultado = " ++ show (sempre 10)
  putStrLn $ "Func. 3; entrada: 10.6  20.9  30.3; resultado = " ++ show (treco 10.6 20.9 30.3)
  putStrLn $ "Func. 4; entrada: 10  2; resultado = " ++ show (resto 10 2)
  putStrLn $ "Func. 5; entrada: 15  18  9  10; resultado = " ++ show (precoMaior 15 18 9 10)
  putStrLn $ "Func. 6; entrada: 11  3; resultado = " ++ show (impar 11 3)
  putStrLn $ "Func. 7; entrada: 11  3; resultado = " ++ show (par 11 3)
  putStrLn $ "Func. 8; entrada: 11  3  9; resultado = " ++ show (conta 11 3 9)
  putStrLn $ "Func. 9; entrada: 78   1.85; resultado = " ++ show (diagnostico 78.5 1.85)
  putStrLn $ "Func. 10; entrada: 2022; resultado = " ++ show (bissexto 2022)
