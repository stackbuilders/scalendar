{-# LANGUAGE OverloadedStrings #-}

module Booking.Config where

import           Booking.Types (ConfigDB (..))

exampleConf :: ConfigDB
exampleConf = Config { path = "scalendar.db" }
