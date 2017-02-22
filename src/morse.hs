import System.IO  
import Data.StringMap
import Data.Maybe

main = do
    text <- readFile "../data/morseex"
    let textlist = words text
    table <- readFile "../data/morsesigns"
    let pairs = Prelude.map (splitandrev.words) (lines table)
        dict = Data.StringMap.fromList pairs
    print (transMorse textlist dict)
  where
    splitandrev [v1,v2] = (v2,v1)
    transMorse :: [String] -> StringMap String -> String
    transMorse [] dict = []
    transMorse (x:xs) dict = transOne x dict ++ transMorse xs dict
      where
        transOne :: String -> StringMap String -> String
        transOne x dict = unwords (Data.StringMap.lookup x dict)

