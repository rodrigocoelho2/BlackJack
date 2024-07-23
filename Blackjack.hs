-- fc60744,fc59857
module Blackjack (
      Carta,
      Baralho,
      EstadoJogo (..),
      Estrategia (..),
      converte,
      tamanho,
      inicializa,
      ejOrdenado,
      ejInsuficiente,
      ejSimples,
      baralho,
      terminado,
      sempreStand,
      sempreHit,
      hitAte17,
      cartaToString,
      handValue,
      simulaRonda,
      simulaJogo
) where

import BaralhosExemplo

data Carta = Carta Char Char deriving (Show)
data Baralho = Baralho [Carta] deriving (Show)

instance Show EstadoJogo where
      show (EstadoJogo _ c cj cc) = "jogador: " ++ unwords cj ++ "\n" ++
                                    "casa: " ++ unwords cc ++ "\n" ++
                                    "creditos: " ++ show c

converte :: [String] -> Baralho
converte xs = Baralho (map (\x -> Carta (x !! 0) (x !! 1)) xs)

tamanho :: Baralho -> Int
tamanho (Baralho cartas) = length cartas

data EstadoJogo = EstadoJogo { baralhoPrincipal :: Baralho
                             , creditos :: Int
                             , cartasJogador :: [String]
                             , cartasCasa :: [String]
                             }

inicializa :: Baralho -> EstadoJogo
inicializa (Baralho cartas) = EstadoJogo (Baralho cartas) 100 [] []

ejOrdenado :: EstadoJogo
ejOrdenado = inicializa (converte baralhoOrdenado)

ejInsuficiente :: EstadoJogo
ejInsuficiente = inicializa (converte baralhoInsuficiente)

ejSimples :: EstadoJogo
ejSimples = inicializa (converte baralhoSimples)

baralho :: EstadoJogo -> Baralho
baralho (EstadoJogo baralho _ _ _) = baralho

terminado :: EstadoJogo -> Bool
terminado (EstadoJogo baralho creditos _ _)
      | tamanho baralho <= 20 = True
      | creditos == 0 = True
      | otherwise = False

data Estrategia = Estrategia { aposta :: Int
                             , maxPoints :: Int
                             }

sempreStand :: Estrategia
sempreStand = Estrategia 5 0

sempreHit :: Estrategia
sempreHit = Estrategia 5 21

hitAte17 :: Estrategia
hitAte17 = Estrategia 5 17

cartaToString :: Carta -> String
cartaToString (Carta valor naipe) = [valor, naipe]

points :: String -> [Int]
points card
      | head card `elem` "23456789" = [read [head card]]
      | head card `elem` "TJQK" = [10]
      | head card == 'A' = [1, 11]
      | otherwise = [0]

handValue :: [String] -> Int
handValue hand 
      | acc + acesValues <= 21 = acc + acesValues
      | otherwise = acc + nAces
             where 
                  (acc, nAces) = foldr (\x (acc,nAces) -> if x == [1,11] then (acc,nAces+1) else (acc+head x,nAces)) (0,0) (map points hand)
                  acesValues = 11+(nAces-1)

simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda strat@(Estrategia aposta maxPoints) estado@(EstadoJogo (Baralho baralho) creditos cartasJogador cartasMesa)
      | null cartasJogador = simulaRonda strat (startingHands estado)
      | handValue cartasJogador < maxPoints = simulaRonda strat (EstadoJogo (Baralho (drop 1 baralho)) creditos (cartasJogador ++ map cartaToString (take 1 baralho)) cartasMesa)
      | handValue cartasJogador > 21 = EstadoJogo (Baralho baralho) (creditos-aposta) [] []
      | handValue cartasMesa < 17 = simulaRonda strat (EstadoJogo (Baralho (drop 1 baralho)) creditos cartasJogador (cartasMesa ++ map cartaToString (take 1 baralho)))
      | handValue cartasMesa < handValue cartasJogador || handValue cartasMesa > 21 = EstadoJogo (Baralho baralho) (creditos+aposta) [] []
      | handValue cartasMesa == handValue cartasJogador = EstadoJogo (Baralho baralho) creditos [] []
      | otherwise = EstadoJogo (Baralho baralho) (creditos-aposta) [] []

simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo strat baralho = simulaJogoRecursive strat (inicializa baralho) 

simulaJogoRecursive :: Estrategia -> EstadoJogo -> Int
simulaJogoRecursive strat estado@(EstadoJogo (Baralho baralho) creditos cartasJogador cartasMesa)
      | terminado estado = creditos
      | otherwise = simulaJogoRecursive strat (simulaRonda strat estado)

startingHands :: EstadoJogo -> EstadoJogo
startingHands (EstadoJogo (Baralho baralho) creditos cartasJogador cartasMesa) = 
      EstadoJogo (Baralho (drop 4 baralho)) creditos (cartasJogador ++ map cartaToString (take 2 baralho)) (cartasMesa ++ map cartaToString (take 2 (drop 2 baralho)))
