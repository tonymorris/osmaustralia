module Geo.Garmin.OsmAustralia where

import System.Unix.Directory 
import System.Directory 
import System.FilePath
import System.Command

mkdir ::
  FilePath
  -> IO ()
mkdir =
  createDirectoryIfMissing True

system' ::
  String
  -> IO ExitCode
system' c =
  putStrLn c >> system c

data State =
  VIC
  | NSW
  | QLD
  | NT
  | TAS
  | WA
  | SA
  deriving (Eq, Show, Enum, Bounded)

description ::
  State 
  -> String
description VIC =
  "Victoria"
description NSW =
  "New South Wales"
description QLD =
  "Queensland"
description NT =
  "Northern Territory"
description TAS =
  "Tasmania"
description WA =
  "Western Australia"
description SA =
  "South Australia"

gmapsupp ::
  [State]
  -> (State -> FilePath -> IO a)
  -> IO ExitCode
gmapsupp s f =
  runExitCodes (map (\t -> withTemporaryDirectory "osmaustralia-" (\d ->
                             let k = show t ++ ".img.zip"
                                 l = d </> k
                             in system' ("wget -O " ++ l ++ " http://www.osmaustralia.org/garmin/AU/" ++ k) ->>
                                system' ("unzip " ++ l ++ " -d " ++ d) ->>
                                system' ("java -jar /opt/mkgmap/mkgmap.jar --gmapsupp --route --transparent --drive-on-left --description=\"" ++ description t ++ "\" --country-name=Australia --country-abbr=AU --output-dir=" ++ d ++ " " ++ (d </> "*.img")) ->->
                                f t (d </> "gmapsupp.img"))) s)

