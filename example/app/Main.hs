module Main where

import           Booking.Config                       (exampleConf)
import           Booking.Interpreter                  (bookingProxy, server)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant                              (serve)

main :: IO ()
main = do
  putStrLn "Server running on localhost:3000"
  run 3000 $ logStdoutDev (serve bookingProxy $ server exampleConf)
