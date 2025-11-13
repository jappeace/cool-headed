{-# LANGUAGE OverloadedStrings #-}
module CoolHeaded
  ( main
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Applicative ((<|>))
import Data.Int (Int16)
import DBus
import DBus.Client

bluezBusName :: BusName
bluezBusName = busName_ "org.bluez"

adapterPath :: ObjectPath
adapterPath = objectPath_ "/org/bluez/hci0"  -- change if not using hci0

main :: IO ()
main = do
    -- Connect to the system bus (BlueZ lives here)
    client <- connectSystem

    -- Make sure adapter is powered
    powerOnAdapter client

    -- 5-second discovery
    putStrLn "Starting discovery for 5 seconds…"
    _ <- call_ client $ (methodCall adapterPath (interfaceName_ "org.bluez.Adapter1") (memberName_ "StartDiscovery"))
        { methodCallDestination = Just bluezBusName
        }

    threadDelay (5 * 1000000)

    _ <- call_ client $ (methodCall adapterPath (interfaceName_ "org.bluez.Adapter1") (memberName_ "StopDiscovery"))
        { methodCallDestination = Just bluezBusName
        }
    putStrLn "Discovery stopped.\n"

    -- Query all managed objects from BlueZ and pick out devices
    devs <- getDevicesWithRssi client

    putStrLn "Devices found (MAC, RSSI):"
    forM_ devs $ \(addr, rssi) ->
        putStrLn $ addr ++ "  RSSI=" ++ show rssi

-- Power on the adapter with org.freedesktop.DBus.Properties.Set
powerOnAdapter :: Client -> IO ()
powerOnAdapter client = do
    -- Get current Powered property
    reply <- call_ client $
        (methodCall adapterPath (interfaceName_ "org.freedesktop.DBus.Properties") (memberName_ "Get"))
            { methodCallDestination = Just bluezBusName
            , methodCallBody =
                [ toVariant ("org.bluez.Adapter1" :: String)
                , toVariant ("Powered" :: String)
                ]
            }

    let body = methodReturnBody reply
        poweredVariant = case body of
            [v] -> v
            _   -> error "Unexpected Get(Powered) reply shape"

        mPowered :: Maybe Bool
        mPowered = fromVariant =<< fromVariant poweredVariant

    case mPowered of
        Just True  -> return ()
        _ -> do
            putStrLn "Powering on adapter…"
            let value = toVariant (True :: Bool) -- inner value for Variant 'v'
            _ <- call_ client $
                (methodCall adapterPath (interfaceName_ "org.freedesktop.DBus.Properties") (memberName_ "Set"))
                    { methodCallDestination = Just bluezBusName
                    , methodCallBody =
                        [ toVariant ("org.bluez.Adapter1" :: String)
                        , toVariant ("Powered" :: String)
                        , value
                        ]
                    }
            return ()

-- Grab all objects from BlueZ and extract (Address, RSSI) from org.bluez.Device1
getDevicesWithRssi :: Client -> IO [(String, Int16)]
getDevicesWithRssi client = do
    reply <- call_ client $
        (methodCall (objectPath_ "/") (interfaceName_ "org.freedesktop.DBus.ObjectManager") (memberName_ "GetManagedObjects"))
            { methodCallDestination = Just bluezBusName
            }

    let body = methodReturnBody reply
        objectsDictVar = case body of
            [v] -> v
            _   -> error "Unexpected GetManagedObjects reply shape"

        -- a{oa{sa{sv}}}
        mTopDict :: Maybe Dictionary
        mTopDict = fromVariant objectsDictVar

    print body
    case mTopDict of
        Nothing   -> return []
        Just dict ->
            let entries = dictionaryItems dict
            in return (concatMap decodeDevice entries)

-- One top-level entry: key = object path, value = dict of interfaces
decodeDevice :: (Variant, Variant) -> [(String, Int16)]
decodeDevice (objPathVar, ifacesVar) =
    case (fromVariant objPathVar :: Maybe ObjectPath,
          fromVariant ifacesVar  :: Maybe Dictionary) of
        (Just _objPath, Just ifacesDict) ->
            let ifaceEntries = dictionaryItems ifacesDict
            in concatMap decodeIfDevice ifaceEntries
        _ -> []

-- Look for org.bluez.Device1 interface
decodeIfDevice :: (Variant, Variant) -> [(String, Int16)]
decodeIfDevice (ifaceNameVar, propsVar) =
    case (fromVariant ifaceNameVar :: Maybe String,
          fromVariant propsVar    :: Maybe Dictionary) of
        (Just "org.bluez.Device1", Just propsDict) ->
            let props = dictionaryItems propsDict
            in maybeToListDevice props
        _ -> []

-- Extract Address (String) and RSSI (Int16) from property dict
maybeToListDevice :: [(Variant, Variant)] -> [(String, Int16)]
maybeToListDevice props =
    case (lookupPropString "Address" props, lookupPropInt16 "RSSI" props) of
        (Just addr, Just rssi) -> [(addr, rssi)]
        _                      -> []

lookupPropString :: String -> [(Variant, Variant)] -> Maybe String
lookupPropString name = go
  where
    go [] = Nothing
    go ((k, v):xs) =
        case fromVariant k :: Maybe String of
            Just n | n == name ->
                -- v is the 'v' (Variant) containing a String
                (fromVariant =<< fromVariant v) <|> go xs
            _ -> go xs

lookupPropInt16 :: String -> [(Variant, Variant)] -> Maybe Int16
lookupPropInt16 name = go
  where
    go [] = Nothing
    go ((k, v):xs) =
        case fromVariant k :: Maybe String of
            Just n | n == name ->
                (fromVariant =<< fromVariant v) <|> go xs
            _ -> go xs
