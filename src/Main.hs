{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import System.IO  
import Data.StringMap
import Data.Maybe
import Data.ByteString
import Data.FileEmbed
import Language.Haskell.TH.Ppr
import Data.Char

dicttxt :: ByteString
dicttxt = $(embedFile "data/morsesigns")

main :: IO ()
main = do
  --table <- readFile "../data/morsesigns"
  let split [v1,v2] = (v1,v2)
      splitandrev [v1,v2] = (v2,v1)
      dict_ct = Data.StringMap.fromList (Prelude.map (splitandrev.words) (lines (bytesToString(unpack dicttxt))))
      dict_tc = Data.StringMap.fromList (Prelude.map (split.words) (lines (bytesToString(unpack dicttxt))))
  -- init ui
  st <- newIORef (Value "hununu")
  void initGUI
  -- create window
  window <- windowNew
  set window [ windowTitle         := "Morse Code Translator"
             , windowDefaultWidth  := 700
             , windowDefaultHeight := 500 ]
  -- create code and text fields
  displayinput <- entryNew
  set displayinput [ entryEditable := True
                   , entryText     := ".. -.-. .... -... .. -. . .. -. -... .-. --- -" ]
  displayoutput <- entryNew
  set displayoutput [ entryEditable := False
                    , entryText     := "" ]
  shortsign <- entryNew
  set shortsign [ entryEditable := True
                   , entryText     := "." ]
  longsign <- entryNew
  set longsign [ entryEditable := True
                   , entryText     := "-" ]
  sepsign <- entryNew
  set sepsign [ entryEditable := True
                   , entryText     := "/" ]
  -- create widget grid and add widgets
  grid <- gridNew                  
  gridSetRowHomogeneous grid True  
  gridSetColumnHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st displayoutput displayinput shortsign longsign sepsign
  attach 0 0 5 1 displayinput
  attach 0 1 5 1 displayoutput
  attach 2 2 1 1 shortsign
  attach 3 2 1 1 longsign
  attach 4 2 1 1 sepsign
  mkBtn dict_ct "code -> text" >>= attach 0 2 1 1
  mkBtn dict_tc "text -> code" >>= attach 1 2 1 1
  containerAdd window grid 
  widgetShowAll window
  -- close window
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window
  -- main loop
  mainGUI


-- | 'Value'
data Value = Value String

-- | Create a button 
mkButton
  :: IORef Value       -- ^ 'IORef' to calculator state
  -> Entry             -- ^ input text
  -> Entry             -- ^ Our display to update
  -> Entry            -- ^ shortsign
  -> Entry            -- ^ longsign
  -> Entry            -- ^ sepsign
  -> StringMap String  -- ^ dictionary
  -> String            -- ^ Button label
  -> IO Button         -- ^ Resulting button object
mkButton st displayoutput displayinput shortsign longsign sepsign dict label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    displayinput <- entryGetText displayinput :: IO String
    shortsign <- entryGetText shortsign :: IO String
    longsign <- entryGetText longsign :: IO String
    sepsign <- entryGetText sepsign :: IO String
    updateDisplay displayoutput (Value (translation displayinput shortsign longsign sepsign dict label))
  return btn

-- | Make calculator's display show given 'Value'.
updateDisplay :: Entry -> Value -> IO ()
updateDisplay displayoutput value =
  set displayoutput [ entryText := renderValue value ]

-- | Render given 'Value'.
renderValue :: Value -> String
renderValue (Value x) = x

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
      transMorse (words (codeadjust displayinput shortsign longsign sepsign)) dict label
  | label == "text -> code" = do
      transMorse (umlaut (words (addSpace (Prelude.map toUpper displayinput)))) dict label
    where
      transMorse :: [String] -> StringMap String -> String -> String
      transMorse [] dict label = []
      transMorse (x:xs) dict label 
        | label == "code -> text" = do
            transOne x dict ++ transMorse xs dict label
        | label == "text -> code" = do 
            transOne x dict ++ " " ++ transMorse xs dict label
        | otherwise = transMorse [] dict label
        where
          transOne :: String -> StringMap String -> String
          transOne x dict = unwords (Data.StringMap.lookup x dict)

-- | addSpace function
addSpace :: String -> String
addSpace xs = if Prelude.length xs <= 1
              then xs
              else Prelude.take 1 xs ++ " " ++ addSpace (Prelude.drop 1 xs)

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
      | x == Prelude.head sepsign = "/"
      | otherwise = [x]