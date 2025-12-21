{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CoolHeaded
  ( main
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (nub)
import qualified Data.Text as T
import CoolHeaded.Parser
import Options.Applicative(execParser)


import DBus.Client

import CoolHeaded.Ble

main :: IO ()
main = do
    Option {..} <- execParser options

    client <- connectSystem

    powerOnAdapter client

    putStrLn "Starting discovery for 5 seconds…"

    whileDiscovering client $ threadDelay (5 * 1000000)

    putStrLn "Discovery stopped.\n"

    devices <- getBleDevices client
    let uniqueDevices = nub devices

    putStrLn "Devices found:"
    forM_ uniqueDevices $ \device -> do
        let mac = getMac device
        if optionMac == mac then
          putStrLn $ "found the blessed mac: " <> T.unpack mac
        else
          putStrLn (T.unpack $ getMac device)
