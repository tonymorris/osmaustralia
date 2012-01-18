module Main where

import Geo.Garmin.OsmAustralia
import System.FilePath
import System.Command
import System.Directory
import System.Environment
import Control.Monad

main ::
  IO ()
main =
  do a <- getArgs
     case a of
       [] -> putStrLn "Usage: osmgarmin <output-dir>" >> exitWith (exitCode 107)
       (o:_) -> do e <- gmapsupp [minBound .. maxBound] (\st p -> let o' = show st </> o
                                                                  in do mkdir o'
                                                                        copyFile p (o' </> takeFileName p))
                   isFailure e `when` putStrLn ("Error: exit with failure: " ++ show e)
