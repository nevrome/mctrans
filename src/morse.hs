module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import System.IO  
import Data.StringMap
import Data.Maybe

main :: IO ()
main = do
  -- build dictionary
  table <- readFile "../data/morsesigns"
  let splitandrev [v1,v2] = (v2,v1)
      pairs = Prelude.map (splitandrev.words) (lines table)
      dict = Data.StringMap.fromList pairs
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
                   , entryText     := ".. -.-. .... -... .. -. . .. -. -... .-. --- - ----. ---.." ]
  displayoutput <- entryNew
  set displayoutput [ entryEditable := False
                    , entryText     := "" ]
  -- create widget grid and add widgets
  grid <- gridNew                  
  gridSetRowHomogeneous grid True  
  gridSetColumnHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st displayoutput displayinput dict
  attach 0 0 5 1 displayinput
  attach 0 1 5 1 displayoutput
  mkBtn "code -> text" >>= attach 0 2 1 1
  mkBtn "text -> code" >>= attach 1 2 1 1
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
  -> StringMap String  -- ^ dictionary
  -> String            -- ^ Button label
  -> IO Button         -- ^ Resulting button object
mkButton st displayoutput displayinput dict label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    test <- entryGetText displayinput :: IO String
    if label == "code -> text" 
      then do 
        updateDisplay displayoutput (Value (translation test dict))
      else do 
        updateDisplay displayoutput (Value (translation test dict))
  return btn

-- | Make calculator's display show given 'Value'.
updateDisplay :: Entry -> Value -> IO ()
updateDisplay displayoutput value =
  set displayoutput [ entryText := renderValue value ]

-- | Render given 'Value'.
renderValue :: Value -> String
renderValue (Value x) = x

-- | translation function
translation :: String -> StringMap String -> String
translation text dict = 
  transMorse textlist dict
    where
      textlist = words text
      transMorse :: [String] -> StringMap String -> String
      transMorse [] dict = []
      transMorse (x:xs) dict = transOne x dict ++ transMorse xs dict
        where
          transOne :: String -> StringMap String -> String
          transOne x dict = unwords (Data.StringMap.lookup x dict)

