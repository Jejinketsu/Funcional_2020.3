module Cinema where

lucrative(value, current, meta, tickets)
 | current < meta && tickets > 0 = lucrative(value, current + value, meta, tickets - 1)
 | otherwise = current

cinema(ticketPrice, profit, movieCost, sectionCost, ticketPerSection)
 | profit <= movieCost = cinema(ticketPrice, profit + sectionProfit, movieCost + sectionCost, sectionCost, ticketPerSection)
 | otherwise = profit
    where
        sectionProfit = lucrative(ticketPrice, 0, (movieCost + sectionCost) - profit, ticketPerSection)

main = do
    putStr "Digite o valor do ingresso: "
    ticketPrice <- getLine
    putStr "Digite o custo do filme: "
    movieCost <- getLine
    putStr "Digite o custo da secao: "
    sectionCost <- getLine
    putStr "Digite a capacidade da secao: "
    ticketPerSection <- getLine
    putStr "Tickets necessarios: "
    print((cinema(read ticketPrice, 0, read movieCost, read sectionCost, read ticketPerSection) :: Float) / 24) 
    putStr "Secoes necessarios: "
    print(ceiling ((cinema(read ticketPrice, 0, read movieCost, read sectionCost, read ticketPerSection) :: Float) / 12000)) 