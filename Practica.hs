import Data.List

solveRPN :: String -> Float 
solveRPN = head.foldl doo [].words
  where doo (x:y:ys) "+"= (x+y):ys
        doo (x:y:ys) "-"= (y-x):ys
        doo (x:y:ys) "*"= (y*x):ys
        doo (x:y:ys) "/"= (y/x):ys   
        doo (x:xs)  "raiz2"= sqrt x:xs 
        doo  (x:xs) "neg1"= negate x:xs 
        doo (x:xs) "condnumero" = condnumero x:xs
        doo (x:y:ys) "sum" = sum(x:y:ys):ys
        doo (x:y:ys) "productoTotal" = product(x:y:ys):ys
        doo xs "totalPromedio"= (sum xs / fromIntegral (length xs)):xs
        doo xs numberString = read numberString: xs

condnumero::Float->Float
condnumero num 
      | num==3=100
      | num==5=25
      | otherwise =0

    
main = do
 print(solveRPN "2 4 5 6 + +")
 print(solveRPN "10 9 4 5 - -")
 print(solveRPN "23 57 58 89 * *")
 print(solveRPN "25 4 8 9 / /")
 print(solveRPN "10 16 raiz2")
 print(solveRPN "10 16 raiz2 +")
 print(solveRPN "10 67 neg1 +")
 print(solveRPN "10 67 neg1 ")
 print(solveRPN "10 67 neg1 *")
 print(solveRPN "10 3 condnumero")
 print(solveRPN "10 67 15 sum")
 print(solveRPN "10 16 10 productoTotal")
 print(solveRPN "120 20 40 12 totalPromedio")
