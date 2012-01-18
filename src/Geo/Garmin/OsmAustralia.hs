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
  withTemporaryDirectory "osmaustralia-" 
    (\d -> runExitCodes (map (\t -> let k = show t ++ ".img.zip"
                                        l = d </> k
                                    in system' ("wget -O " ++ l ++ " http://www.osmaustralia.org/garmin/AU/" ++ k) ->>
                                       system' ("unzip " ++ l ++ " -d " ++ d) ->>
                                       system' ("mkgmap --gmapsupp --route --transparent --drive-on-left --description=\"" ++ description t ++ "\" --country-name=Australia --country-abbr=AU --output-dir=" ++ d ++ " " ++ (d </> "*.img")) ->->
                                       f t (d </> "gmapsupp.img")) s))

