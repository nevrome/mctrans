{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import System.IO  
import qualified Data.StringMap --hiding (map)
import Data.Maybe
import qualified Data.ByteString
import Data.FileEmbed
import Language.Haskell.TH.Ppr
import Data.Char
import Trans

-- | Embed dictionary text file via Data.FileEmbed
dicttxt :: Data.ByteString.ByteString
dicttxt = $(embedFile "data/morsesigns")

main :: IO ()
main = do
  -- read and parse dictionary file into Data.Stringmap 
  let split [v1,v2] = (v1,v2)
      splitandrev [v1,v2] = (v2,v1)
      dict_ct = Data.StringMap.fromList (map (splitandrev.words) ((lines . bytesToString . Data.ByteString.unpack) dicttxt))
      dict_tc = Data.StringMap.fromList (map (split.words) ((lines . bytesToString . Data.ByteString.unpack) dicttxt))
  -- init user interface
  st <- newIORef (Value "hununu")
  void initGUI
  -- create window
  window <- windowNew
  set window [ windowTitle         := "Morse Code Translator"
             , windowDefaultWidth  := 700
             , windowDefaultHeight := 500 ]
  -- create code and text fields
  displayinput      <- entryNew
  set displayinput  [ entryEditable := True
                    , entryText     := "zu übersetzenden Text hier eingeben" ]
  displayoutput     <- entryNew
  set displayoutput [ entryEditable := False
                    , entryText     := "hier wird das Ergebnis ausgegeben" ]
  shortsign         <- entryNew
  set shortsign     [ entryEditable := True
                    , entryText     := "." ]
  longsign          <- entryNew
  set longsign      [ entryEditable := True
                    , entryText     := "-" ]
  sepsign           <- entryNew
  set sepsign       [ entryEditable := True
                    , entryText     := "/" ]
  linesepsign       <- entryNew
  set linesepsign   [ entryEditable := True
                    , entryText     := "n" ]
  -- create widget grid and add widgets
  grid <- gridNew                  
  gridSetRowHomogeneous grid True  
  gridSetColumnHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st displayoutput displayinput shortsign longsign sepsign
  attach 0 0 6 1 displayinput
  attach 0 1 6 1 displayoutput
  attach 2 2 1 1 shortsign
  attach 3 2 1 1 longsign
  attach 4 2 1 1 sepsign
  attach 5 2 1 1 linesepsign
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

-- | 'Value' data type
data Value = Value String

-- | Create a button 
mkButton
  :: IORef Value      -- ^ 'IORef' to input state
  -> Entry            -- ^ input text
  -> Entry            -- ^ Our display to update
  -> Entry            -- ^ shortsign
  -> Entry            -- ^ longsign
  -> Entry            -- ^ sepsign
  -> Data.StringMap.StringMap String -- ^ dictionary
  -> String           -- ^ Button label
  -> IO Button        -- ^ Resulting button object
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

-- | Make output display show given 'Value'.
updateDisplay :: Entry -> Value -> IO ()
updateDisplay displayoutput value =
  set displayoutput [ entryText := renderValue value ]

-- | Render given 'Value'.
renderValue :: Value -> String
renderValue (Value x) = x