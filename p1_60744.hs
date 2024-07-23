-- A. Baralho
ranks :: [Char]
ranks = "A23456789TJQK"

suits :: [Char]
suits = "SHDC"

baralho :: [String]
baralho = [rank : [suit] | rank <- ranks, suit <- suits]

-- B. Blackjack
points :: String -> [Int]
points card
      | head card `elem` "23456789" = [read [head card]]
      | head card `elem` "TJQK" = [10]
      | head card == 'A' = [1, 11]
      | otherwise = [0]

combinacoesBlackjack :: Int -> [(String, String)]
combinacoesBlackjack x = 
      [(card1, card2) | card1 <- baralho, card2 <- baralho,
            p1 <- points card1, p2 <- points card2, p1 + p2 == x,
            card1 < card2] 

-- C. Poker
fullHouses :: [[String]]
fullHouses = [[c1, c2, c3, c4, c5] | c1 <- baralho,
                                     c2 <- baralho, head c1 == head c2, c1 < c2,
                                     c3 <- baralho, head c1 == head c3, c2 < c3,
                                     c4 <- baralho, head c4 /= head c1,
                                     c5 <- baralho, head c4 == head c5, c4 < c5]