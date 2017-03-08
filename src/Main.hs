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
  set window [ windowTitle          := "Morse Code Translator"
             , windowDefaultWidth   := 700
             , windowDefaultHeight  := 500 
             , containerBorderWidth := 10]
  -- create labels 
  (shortlabel,shortframe) <- myLabelWithFrameNew
  labelSetText shortlabel "short sign"
  (longlabel,longframe) <- myLabelWithFrameNew
  labelSetText longlabel "long sign"
  (seplabel,sepframe) <- myLabelWithFrameNew
  labelSetText seplabel "word separation sign"
  -- create code and text fields
  displayinput      <- textBufferNew Nothing
  set displayinput  [ textBufferText := "enter text/code to be translated here: Fußpilz or ..-. ..- ... ... .--. .. .-.. --.." ]
  displayinputview  <- textViewNewWithBuffer displayinput
  set displayinputview [textViewWrapMode := WrapWord]
  displayoutput     <- textBufferNew Nothing
  set displayoutput [ textBufferText := "translation" ]
  displayoutputview <- textViewNewWithBuffer displayoutput
  set displayoutputview [textViewWrapMode := WrapWord]
  shortsign         <- entryNew
  set shortsign     [ entryEditable  := True
                    , entryText      := "." ]
  longsign          <- entryNew
  set longsign      [ entryEditable  := True
                    , entryText      := "-" ]
  sepsign           <- entryNew
  set sepsign       [ entryEditable  := True
                    , entryText      := "/" ]
  -- create widget grid and add widgets
  grid <- gridNew                  
  gridSetRowHomogeneous grid True  
  gridSetColumnHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st displayoutput displayinput shortsign longsign sepsign
  attach 0 0 5 4 displayinputview
  attach 0 4 6 4 displayoutputview
  attach 3 8 1 1 shortframe
  attach 4 8 1 1 longframe
  attach 5 8 1 1 sepframe
  mkBtn dict_tc "text -> code" >>= attach 5 0 1 2
  mkBtn dict_ct "code -> text" >>= attach 5 2 1 2
  attach 3 9 1 2 shortsign
  attach 4 9 1 2 longsign
  attach 5 9 1 2 sepsign
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
  -> TextBuffer       -- ^ Our display to update
  -> TextBuffer       -- ^ input text
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
    displayinput <- get displayinput textBufferText
    shortsign <- entryGetText shortsign :: IO String
    longsign <- entryGetText longsign :: IO String
    sepsign <- entryGetText sepsign :: IO String
    updateDisplay displayoutput (Value (concatMap (\x -> (translation x shortsign longsign sepsign dict label) ++ "\n") (lines displayinput)))
  return btn

-- | Make output display show given 'Value'.
updateDisplay :: TextBuffer -> Value -> IO ()
updateDisplay displayoutput value =
  set displayoutput [ textBufferText := renderValue value ]

-- | Render given 'Value'.
renderValue :: Value -> String
renderValue (Value x) = x

-- | Create label 
myLabelWithFrameNew :: IO (Label,Frame)
myLabelWithFrameNew = do
  label <- labelNew (Nothing :: Maybe String)
  frame <- frameNew
  containerAdd frame label
  frameSetShadowType frame ShadowNone
  return (label, frame)