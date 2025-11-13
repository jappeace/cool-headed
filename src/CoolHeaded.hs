{-# LANGUAGE OverloadedStrings #-}
module CoolHeaded
  ( main
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Int (Int16)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import DBus
import DBus.Client


bluezBusName :: BusName
bluezBusName = busName_ "org.bluez"

adapterPath :: ObjectPath
adapterPath = objectPath_ "/org/bluez/hci0"

data DecodeError
    = TopLevelNotDict Type
    | EntryKeyNotObjectPath Variant
    | EntryValueNotDict ObjectPath Type
    | IfaceNameNotText ObjectPath Variant
    | PropsNotDict ObjectPath Type
    | MissingProp ObjectPath Text
    | BadPropType ObjectPath Text Type
    deriving (Show)

type Device = (Text, Int16)   -- (MAC, RSSI)

main :: IO ()
main = do
    client <- connectSystem

    powerOnAdapter client

    putStrLn "Starting discovery for 5 seconds…"
    _ <- call_ client $
        (methodCall adapterPath "org.bluez.Adapter1" "StartDiscovery")
            { methodCallDestination = Just bluezBusName }

    threadDelay (5 * 1000000)

    _ <- call_ client $
        (methodCall adapterPath "org.bluez.Adapter1" "StopDiscovery")
            { methodCallDestination = Just bluezBusName }

    putStrLn "Discovery stopped.\n"

    (errs, devs) <- getDevicesWithRssi client

    putStrLn "Devices found (MAC, RSSI):"
    forM_ devs $ \(addr, rssi) ->
        putStrLn $ T.unpack addr ++ "  RSSI=" ++ show rssi

    putStrLn ""
    putStrLn "Decode errors:"
    forM_ errs $ \e ->
        putStrLn ("  " ++ show e)

powerOnAdapter :: Client -> IO ()
powerOnAdapter client = do
    reply <- call_ client $
        (methodCall adapterPath "org.freedesktop.DBus.Properties" "Get")
            { methodCallDestination = Just bluezBusName
            , methodCallBody =
                [ toVariant ("org.bluez.Adapter1" :: Text)
                , toVariant ("Powered" :: Text)
                ]
            }

    let body = methodReturnBody reply
        poweredVariant = case body of
            [v] -> v
            _   -> error "Unexpected reply shape from Get(Powered)"

        mPowered :: Maybe Bool
        mPowered = fromVariant =<< fromVariant poweredVariant

    case mPowered of
        Just True -> pure ()
        _ -> do
            putStrLn "Powering on adapter…"
            let value = toVariant (True :: Bool)
            _ <- call_ client $
                (methodCall adapterPath "org.freedesktop.DBus.Properties" "Set")
                    { methodCallDestination = Just bluezBusName
                    , methodCallBody =
                        [ toVariant ("org.bluez.Adapter1" :: Text)
                        , toVariant ("Powered" :: Text)
                        , value
                        ]
                    }
            pure ()

getDevicesWithRssi :: Client -> IO ([DecodeError], [Device])
getDevicesWithRssi client = do
    reply <- call_ client $
        (methodCall "/" "org.freedesktop.DBus.ObjectManager" "GetManagedObjects")
            { methodCallDestination = Just bluezBusName }

    let body = methodReturnBody reply

    case body of
        [objectsVariant] ->
            case fromVariant objectsVariant :: Maybe Dictionary of
                Nothing ->
                    pure ([TopLevelNotDict (variantType objectsVariant)], [])
                Just dict -> do
                    let entries = dictionaryItems dict
                        results = map decodeTopEntry entries
                        (errsLists, devLists) = unzip results
                    pure (concat errsLists, concat devLists)
        _ ->
            pure ([TopLevelNotDict TypeVariant], [])  -- "impossible" shape but tagged

-- Each top-level entry: key = object path, value = dict of interfaces
decodeTopEntry
    :: (Variant, Variant)
    -> ([DecodeError], [Device])
decodeTopEntry (pathVar, ifacesVar) =
    case fromVariant pathVar :: Maybe ObjectPath of
        Nothing ->
            ([EntryKeyNotObjectPath pathVar], [])
        Just path ->
            case fromVariant ifacesVar :: Maybe Dictionary of
                Nothing ->
                    ([EntryValueNotDict path (variantType ifacesVar)], [])
                Just ifacesDict ->
                    let ifaceEntries = dictionaryItems ifacesDict
                        results = map (decodeIface path) ifaceEntries
                        (errsLists, devLists) = unzip results
                    in (concat errsLists, concat devLists)

-- One interface entry on a path: key = interface name, value = dict of properties
decodeIface
    :: ObjectPath
    -> (Variant, Variant)
    -> ([DecodeError], [Device])
decodeIface path (ifaceNameVar, propsVar) =
    case fromVariant ifaceNameVar :: Maybe Text of
        Nothing ->
            ([IfaceNameNotText path ifaceNameVar], [])
        Just ifaceName
            | ifaceName /= "org.bluez.Device1" ->
                ([], [])   -- not a device; ignore
            | otherwise ->
                case fromVariant propsVar :: Maybe Dictionary of
                    Nothing ->
                        ([PropsNotDict path (variantType propsVar)], [])
                    Just propsDict ->
                        decodeDeviceProps path (dictionaryItems propsDict)

-- Properties dict: key = property name (Text), value = Variant (wrapped)
decodeDeviceProps
    :: ObjectPath
    -> [(Variant, Variant)]
    -> ([DecodeError], [Device])
decodeDeviceProps path props =
    let mAddrVar = lookupProp "Address" props
        mRssiVar = lookupProp "RSSI"   props
    in case (mAddrVar, mRssiVar) of
        (Nothing, _) ->
            ([MissingProp path "Address"], [])
        (_, Nothing) ->
            ([MissingProp path "RSSI"], [])
        (Just addrVar, Just rssiVar) ->
            let mAddr :: Maybe Text
                mAddr = fromVariant =<< fromVariant addrVar

                mRssi :: Maybe Int16
                mRssi = fromVariant =<< fromVariant rssiVar
            in case (mAddr, mRssi) of
                (Nothing, _) ->
                    ([BadPropType path "Address" (variantType addrVar)], [])
                (_, Nothing) ->
                    ([BadPropType path "RSSI" (variantType rssiVar)], [])
                (Just addr, Just rssi) ->
                    ([], [(addr, rssi)])

-- Find a property by name in a{sv}
lookupProp
    :: Text
    -> [(Variant, Variant)]
    -> Maybe Variant
lookupProp name =
    fmap snd . listToMaybe . filter matches
  where
    matches (k, _) =
        case fromVariant k :: Maybe Text of
            Just n  -> n == name
            Nothing -> False
