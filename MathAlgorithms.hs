--Nathaniel Bates
--COP 4020
--Math Algorithms Assignment
--9/22/17

--Create two functions: one creates a random list of integers, and the other creates a random list of floating point numbers.

--Create the following functions: from a randomized list find the mean, medium, max, min, standard deviation

import System.Random
import Data.List

--Helper Functions to calculate statistical data
--Determines the maximum value in the list.
myMax [] = 0
myMax [x] = x
myMax (x:xs)
     |x > currentMax = x
     |otherwise = currentMax 
     where currentMax = myMax xs

--Determines the minimum value in the list.
myMin [] = 0
myMin [x] = x
myMin (x:xs)
     | x < currentMin = x
     | otherwise = currentMin
     where currentMin = myMin xs

--Determines the length of the list
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

--Determines the sum of all values in the list.
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs

--Sorts the lists so that we can then calculate the median value.
mySort [] = []  
mySort (x:xs) =   
    let smallerSorted = mySort [a | a <- xs, a <= x]  
        biggerSorted = mySort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

--Determines the average, or the mean of the values in the list.
myMean xs = realToFrac(mySum(xs)) / realToFrac(myLength(xs))

--Determines the median of the sets of lists
myMedian xs 
     | ((myLength xs) `mod` 2) == 0 = (realToFrac(((xs!!(((myLength xs) `div` 2) - 1)) + (xs!!((myLength xs) `div` 2))))) / (realToFrac(2.0))
     | otherwise = realToFrac(xs !! ((myLength xs) `div` 2))

--Standard deviation calculation functions.
stdInternalList xs = [(x - myMean xs)^2 | x <- xs]
stdDev xs = sqrt(myMean (stdInternalList xs))

--Used for converting the list of ints to floats for use in the sqrt function 
--when calculating the standard deviation.
convertInts :: [Int] -> [Float]
convertInts xs = [fromIntegral(x) | x <- xs]

--main function will do all the calculations and print out the information.
--The random number generator will generate to lists of 10, one of integers and one of floats.
--All values that need to be calculated along with the lists are printed out.
main = do
   gen <- getStdGen
   let newInts = take 10 $ randomRs(1, 100) gen :: [Int]
   let newFloats = take 10 $ randomRs(1, 100) gen :: [Float]
  
   putStrLn "A random list of 10 integers:"
   print newInts   
   putStrLn "\nA random list of 10 floats:"
   print newFloats
   
   putStrLn "\nInteger Information:\n"
   putStrLn "The mean for the list of integers:"
   print (myMean newInts)
   putStrLn "The median for the list of integers:"
   print (myMedian (mySort newInts))
   putStrLn "The maximum for the list of integers:"
   print (myMax newInts)
   putStrLn "The minimum for the list of integers:"
   print (myMin newInts)
   putStrLn "The standard deviation for the list of integers:"
   print (stdDev (convertInts newInts))
   
   putStrLn "\nFloating Point Information:\n"
   putStrLn "The mean for the list of floats:"
   print (myMean newFloats)  
   putStrLn "The median for the list of floats:"
   print (myMedian (mySort newFloats))   
   putStrLn "The maximum for the list of floats:"
   print (myMax newFloats)   
   putStrLn "The minimum for the list of floats:"
   print (myMin newFloats)  
   putStrLn "The standard deviation for the list of floats:"
   print (stdDev newFloats)