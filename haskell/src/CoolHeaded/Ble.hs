{-# LANGUAGE OverloadedStrings #-}
-- | dbus based ble interaction
module CoolHeaded.Ble
  (
    powerOnAdapter,
    whileDiscovering,
    getBleDevices,
    getMac
  )
where

import Control.Monad
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import UnliftIO.Exception(bracket_)

import DBus
import DBus.Client

newtype BleDevice = BleDevice { bleMac :: Text }
    deriving (Eq, Ord, Show)

getMac :: BleDevice -> Text
getMac = bleMac

bluezBusName :: BusName
bluezBusName = busName_ "org.bluez"

adapterPath :: ObjectPath
adapterPath = objectPath_ "/org/bluez/hci0"


whileDiscovering :: Client -> IO () -> IO ()
whileDiscovering client fun = bracket_ startDiscovery fun  stopDiscovery
  where
    startDiscovery = void $ call_ client $
        (methodCall adapterPath "org.bluez.Adapter1" "StartDiscovery")
            { methodCallDestination = Just bluezBusName }
    stopDiscovery = void $ call_ client $
        (methodCall adapterPath "org.bluez.Adapter1" "StopDiscovery")
            { methodCallDestination = Just bluezBusName }

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

getBleDevices :: Client -> IO [BleDevice]
getBleDevices client = do
    reply <- call_ client $
        (methodCall "/" "org.freedesktop.DBus.ObjectManager" "GetManagedObjects")
            { methodCallDestination = Just bluezBusName }

    let body = methodReturnBody reply
    case body of
        [objectsVariant] ->
            case fromVariant objectsVariant :: Maybe Dictionary of
                Nothing   -> pure []
                Just dict ->
                    let entries = dictionaryItems dict
                    in pure (mapMaybe decodeDevice entries)
        _ -> pure []

decodeDevice :: (Variant, Variant) -> Maybe BleDevice
decodeDevice (pathVar, ifacesVar) = do
    _path <- fromVariant pathVar :: Maybe ObjectPath
    ifacesDict <- fromVariant ifacesVar :: Maybe Dictionary
    let ifaceEntries = dictionaryItems ifacesDict
    propsVar <- lookupIface "org.bluez.Device1" ifaceEntries
    propsDict <- fromVariant propsVar :: Maybe Dictionary
    let props = dictionaryItems propsDict
    addrVar <- lookupProp "Address" props
    addr <- fromVariant =<< fromVariant addrVar :: Maybe Text
    pure (BleDevice addr)

lookupIface :: Text -> [(Variant, Variant)] -> Maybe Variant
lookupIface name =
    fmap snd . listToMaybe . filter matches
  where
    matches (k, _) =
        case fromVariant k :: Maybe Text of
            Just n  -> n == name
            Nothing -> False

lookupProp :: Text -> [(Variant, Variant)] -> Maybe Variant
lookupProp name =
    fmap snd . listToMaybe . filter matches
  where
    matches (k, _) =
        case fromVariant k :: Maybe Text of
            Just n  -> n == name
            Nothing -> False
