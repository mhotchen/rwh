module Main (main) where

import           JJson
import           PrettyStub

main = print (renderJValue (JBool True))
