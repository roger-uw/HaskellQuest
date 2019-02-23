module GenJSONInfo (genJSONInfo) where

import Test.QuickCheck

resultToJSON :: Result -> String
resultToJSON Success {output = out} = "{\"stat\": \"AC\", \"info\": \"" ++ out ++ "\"}"
resultToJSON Failure {reason = rs, output = out} = 
  let stat = case rs of
        "Falsifiable" -> "WA"
        "Timeout" -> "TLE"
        "Exception: '<<timeout>>'" -> "TLE"
        _ -> "RE"
  in "{\"stat\": \"" ++ stat ++ "\", \"info\": \"" ++ out ++ "\"}"
resultToJSON r = "{\"stat\": \"UE\", \"info\": \"" ++ output r ++ "\"}"

embedEscape :: Char -> String
embedEscape '\n' = "\\n"
embedEscape '\\' = "\\\\"
embedEscape x = [x]

genJSONInfo :: Result -> String
genJSONInfo = concatMap embedEscape . resultToJSON
