module Trans where

import qualified Data.StringMap
import Data.Char

-- | translation function
translation 
  :: String            -- ^ displayinput: text to be translated
  -> String            -- ^ shortsign: sign used for "short"
  -> String            -- ^ longsign: sign used for "long"
  -> String            -- ^ sepsign: sign used for "separator"
  -> Data.StringMap.StringMap String  -- ^ dict: dictionary file
  -> String            -- ^ label: button label
  -> String
translation displayinput shortsign longsign sepsign dict label  
  | label == "code -> text" = do
      transMorse (words $ codeadjust displayinput shortsign longsign sepsign) dict label sepsign
  | label == "text -> code" = do
      transMorse (words . addSpace . umlaut2 . toUpperString $ spaceReplace displayinput sepsign) dict label sepsign
      -- transMorse (umlaut . words . addSpace . toUpperString $ spaceReplace displayinput sepsign) dict label sepsign
    where
      transMorse :: [String] -> Data.StringMap.StringMap String -> String -> String -> String
      transMorse [] dict label sepsign = []
      transMorse (x:xs) dict label sepsign
        | label == "code -> text" = do
            trans_ct x dict sepsign ++ transMorse xs dict label sepsign
        | label == "text -> code" = do 
            trans_tc x dict sepsign ++ " " ++ transMorse xs dict label sepsign
        where
          trans_ct :: String -> Data.StringMap.StringMap String -> String -> String
          trans_ct x dict sepsign
            | x == sepsign = " "
            | otherwise = unwords (Data.StringMap.lookup x dict)
          trans_tc :: String -> Data.StringMap.StringMap String -> String -> String
          trans_tc x dict sepsign
            | x == sepsign = sepsign
            | otherwise = unwords (Data.StringMap.lookup x dict)

-- | Add space between every char in String 
addSpace :: String -> String
addSpace xs = if length xs <= 1
              then xs
              else take 1 xs ++ " " ++ addSpace (drop 1 xs)

spaceReplace :: String -> String -> String
spaceReplace [] sepsign = []
spaceReplace (x:xs) sepsign = 
  replacecode x sepsign ++ spaceReplace xs sepsign
  where
    replacecode :: Char -> String -> [Char]
    replacecode x sepsign
      | x == ' ' = sepsign
      | otherwise = [x]

-- | Umlaut replacement function
--umlaut :: [String] -> [String]
--umlaut [] = []
--umlaut (x:xs) = concat (map umlautrep (x:xs))
--  where 
--    umlautrep :: String -> [String]
--    umlautrep x 
--      | x == "Ä" || x == "ä" = words (addSpace "AE")
--      | x == "Ö" || x == "ö" = words (addSpace "OE")
--      | x == "Ü" || x == "ü" = words (addSpace "UE")
--      | x == "ß"             = words (addSpace "SS")
--      | otherwise = [x]

umlaut2 :: String -> String
umlaut2 [] = []
umlaut2 xs = concat (map umlautrep2 xs)
  where 
    umlautrep2 :: Char -> [Char]
    umlautrep2 x 
      | x == 'Ä' || x == 'ä' = "AE"
      | x == 'Ö' || x == 'ö' = "OE"
      | x == 'Ü' || x == 'ü' = "UE"
      | x == 'ß'             = "SS"
      | otherwise = [x]


-- | Replace long and short signs with "." and "-"
codeadjust :: String -> String -> String -> String -> String
codeadjust [] shortsign longsign sepsign = []
codeadjust (x:xs) shortsign longsign sepsign = 
  replacecode x shortsign longsign sepsign ++ codeadjust xs shortsign longsign sepsign
  where
    replacecode :: Char -> String -> String -> String -> [Char]
    replacecode x shortsign longsign sepsign
      | x == head shortsign = "."
      | x == head longsign = "-"
      | otherwise = [x]

-- | Maske für map toUpper _ 
toUpperString :: String -> String
toUpperString [] = []
toUpperString xs = map toUpper xs