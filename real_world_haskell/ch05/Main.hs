module Main (main) where

import SimpleJSON
import PutJSON (putJValue)

-- main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
