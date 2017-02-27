module Trans where

import Data.StringMap
import Data.Char

-- | translation function
translation 
  :: String 
  -> String            -- ^ shortsign
  -> String            -- ^ longsign
  -> String            -- ^ sepsign
  -> StringMap String 
  -> String 
  -> String
translation displayinput shortsign longsign sepsign dict label  
  | label == "code -> text" = do
      transMorse (words (codeadjust displayinput shortsign longsign sepsign)) dict label sepsign
  | label == "text -> code" = do
      transMorse (umlaut (words (addSpace (Prelude.map toUpper (spaceReplace displayinput sepsign))))) dict label sepsign
    where
      transMorse :: [String] -> StringMap String -> String -> String -> String
      transMorse [] dict label sepsign = []
      transMorse (x:xs) dict label sepsign
        | label == "code -> text" = do
            trans_ct x dict sepsign ++ transMorse xs dict label sepsign
        | label == "text -> code" = do 
            trans_tc x dict sepsign ++ " " ++ transMorse xs dict label sepsign
        where
          trans_ct :: String -> StringMap String -> String -> String
          trans_ct x dict sepsign
            | x == sepsign = " "
            | otherwise = unwords (Data.StringMap.lookup x dict)
          trans_tc :: String -> StringMap String -> String -> String
          trans_tc x dict sepsign
            | x == sepsign = sepsign
            | otherwise = unwords (Data.StringMap.lookup x dict)

-- | addSpace function
addSpace :: String -> String
addSpace xs = if Prelude.length xs <= 1
              then xs
              else Prelude.take 1 xs ++ " " ++ addSpace (Prelude.drop 1 xs)

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
umlaut :: [String] -> [String]
umlaut [] = []
umlaut (x:xs) = Prelude.concat (Prelude.map umlautrep (x:xs))
  where 
    umlautrep :: String -> [String]
    umlautrep x 
      | x == "Ä" || x == "ä" = words (addSpace "AE")
      | x == "Ö" || x == "ö" = words (addSpace "OE")
      | x == "Ü" || x == "ü" = words (addSpace "UE")
      | x == "ß"             = words (addSpace "SS")
      | otherwise = [x]

-- | code adjust function
codeadjust :: String -> String -> String -> String -> String
codeadjust [] shortsign longsign sepsign = []
codeadjust (x:xs) shortsign longsign sepsign = 
  replacecode x shortsign longsign sepsign ++ codeadjust xs shortsign longsign sepsign
  where
    replacecode :: Char -> String -> String -> String -> [Char]
    replacecode x shortsign longsign sepsign
      | x == Prelude.head shortsign = "."
      | x == Prelude.head longsign = "-"
      | otherwise = [x]