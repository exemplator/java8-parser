module Main where

import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ContinueAsync))
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types (JSVal)
import Lib

sayHello :: String -> IO ()
sayHello name = print $ parse name

sayHello' :: JSVal -> IO ()
sayHello' jsval = do
    Just str <- fromJSVal jsval
    sayHello $ unpack str

foreign import javascript unsafe "js_callback_ = $1"
    set_callback :: Callback a -> IO ()

foreign import javascript unsafe "js_callback_($1)" 
    test_callback :: JSString -> IO ()

main = do
    callback <- syncCallback1 ContinueAsync sayHello'
    set_callback callback
