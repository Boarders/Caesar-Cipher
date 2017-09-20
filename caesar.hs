import Data.Char
import Data.List

charNum :: Char -> Int
charNum x
  | isLower x = ord x - ord 'a'
  | isUpper x = ord x - ord 'A'
  | otherwise = 27
    
charCycle :: Int -> Char -> Char
charCycle n x
  | isUpper x = chr $ ((charNum x + n) `mod` 26 ) + ord ('A')
  | isLower x = chr $ ((charNum x + n) `mod` 26) + ord ('a')
  | otherwise = x


caesarEncode :: Int -> String -> String
caesarEncode n st = unwords cycleWds
  where wds = words st
        cycleWds = map (map (charCycle n)) wds

caesarDecode n = caesarEncode (-n)

rot13 = caesarEncode 13

-- This is a small collection of functions to decode a monoalphabetic cipher using a frequency table

letterFrequencyTable :: [Float]
letterFrequencyTable = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094,
             6.966, 0.153, 0.772, 4.025,  2.406, 6.749, 7.507, 1.929,
             0.095, 5.987, 6.327, 9.056,  2.758, 0.978, 2.360, 0.150,
             1.974, 0.074]

letterFrequencyTableLetters = zip frequency ['a' .. 'z']

orderdedFreqOfLetters = map (snd) $ reverse ( sort frequencyLetters)

standardBasisVector :: Int -> [Int]
standardBasisVector n = (take (n-1) $ repeat 0) ++ [1] ++ (repeat 0)

zeroes :: [Int]
zeroes = take 26 $ repeat 0

lowerString :: String -> String
lowerString = map (toLower)

letterFreq :: String -> [Int]
letterFreq st = foldr (\c acc -> zipWith (+) acc (nthOne ( (charNum c) + 1) )) zeroes letters
  where letters = concat $ words $ lowerString st
        
letterFreq' st = zip (letterFreq st) ['a'..'z']

letterFreqList st = map (snd) $ reverse ( sort $ letterFreq' st)

    
