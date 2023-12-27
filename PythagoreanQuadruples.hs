--
--  zadanie 7 - czworka pitagorejska
--

-- algorytm Euklidesa - NWD a,b jest taki sam jak NWD a,(reszta z dzielenia a/b)
greatestCommonDivisor :: Integral a => a -> a -> a
greatestCommonDivisor a 0 = a -- jesli b==0, to zwracamy a jako wynik funkcji (to jest przypadek bazowy dla rekurencji (koniec rekurencji) )
greatestCommonDivisor a b = greatestCommonDivisor b $ a `mod` b

checkIfNumbersHaveTheSameDivisor :: Integral a => a -> a -> a -> a-> Bool
checkIfNumbersHaveTheSameDivisor a b c d = greatestCommonDivisor a b == 1 && greatestCommonDivisor c d == 1 && greatestCommonDivisor a c == 1

findPythagoreanQuadruple :: Integral a => a -> [(a, (a, a, a, a))]
findPythagoreanQuadruple n = filter (\(n, (a, b, c, d)) -> a*a + b*b + c*c == d*d && checkIfNumbersHaveTheSameDivisor a b c d) allCombinations
    where
        allCombinations = [(n, (a, b, c, d)) | a <- [1..n],
                                               b <- [a..n],
                                               c <- [b..n],
                                               let d = n - a - b - c]

main :: IO ()
main = do
    putStrLn "Podaj liczbę n: "
    n <- readLn
    let pythagoreanQuadruples = concatMap findPythagoreanQuadruple [n,n-1..1] -- liczby od n do 1 (n,n-1,n-2,n-3,...,1), czyli
    -- szukamy czworek pitagorejskich dla wszystkich liczb od n do 1, aby potem wypisac tylko jedna czworke dla n
    -- (lub jesli dla n nie istnieje, to dla liczby mniejszej, najblizszej do n)
    if null pythagoreanQuadruples
        then putStrLn "Nie znaleziono pierwotnej czwórki pitagorejskiej dla żadnej liczby m < n."
        else let (m, quadruple) = head pythagoreanQuadruples -- czyli bierzemy pierwszy element z listy
             in putStrLn $ "Czwórka pitagorejska dla " ++ show m ++ " to: " ++ show quadruple

