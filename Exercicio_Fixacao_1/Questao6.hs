module TheftCalculator where

anual(value) = value * 12

stolenTax(base, aliquot) = base * (aliquot / 100)

restitution(payed, tax) = payed - tax

calcBase(anualSalary) = anualSalary * 0.8

calcTax(salary, incomeTax)
 | base <= 22000 = anual(incomeTax)
 | base > 22000 && base <= 33000 = stolenTax(base, 7.5) - anual(incomeTax)
 | base > 33000 && base <= 45000 = stolenTax(base, 15.0) - anual(incomeTax)
 | base > 45000 && base <= 55000 = stolenTax(base, 22.5) - anual(incomeTax)
 | base > 55000 = stolenTax(base, 27.5) - incomeTax
 | otherwise = error "algo deu errado :("
    where
        base = calcBase(anual(salary))

main = do
    putStr "Digite seu salário: "
    salary <- getLine
    putStr "Digite o imposto mensal: "
    tax <- getLine
    putStr "Sua restituição ´de R$ "
    print(calcTax(read salary,read tax) :: Float)