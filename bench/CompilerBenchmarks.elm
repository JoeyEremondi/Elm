import Graphics.Element exposing (show)
import Signal
import Benchmark
import Task exposing (Task, andThen)
import Text

import LargeDictionary

main =
 Signal.map (Graphics.Element.leftAligned << Text.fromString ) results.signal

mySuite =
  Benchmark.Suite "My Suite" 
  [ Benchmark.bench1 "Make and sum large dictionary" LargeDictionary.addNToDictAndSum 1000
  ]

results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"

port benchResults : (Task Benchmark.Never ())
port benchResults =
  Benchmark.runWithProgress (Just results) mySuite `andThen` \_ -> Task.succeed ()
  -- `andThen` Signal.send results.address
