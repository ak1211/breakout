module Main where

import Prelude
import Effect (Effect)
import Breakout as Breakout
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Breakout.component unit body
