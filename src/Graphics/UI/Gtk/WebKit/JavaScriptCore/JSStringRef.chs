{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad


#include <JavaScriptCore/JSStringRef.h>

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.Util #}

type JSChar = {#type JSChar#}

foreign import ccall unsafe "JSBase.chs.h &JSStringRelease"
  jsStringReleasePtr :: FinalizerPtr JSString
                        
jsStringRelease :: JSString -> IO ()
jsStringRelease = {# call JSStringRelease as ^ #}

makeJSString :: String -> IO JSString
makeJSString string = 
  withCString string $ \chars -> 
    {#call JSStringCreateWithUTF8CString as ^ #} chars 
    
withJSString :: String -> (JSString -> IO c) -> IO c    
withJSString string = bracket (makeJSString string) jsStringRelease


jsStringToString :: JSString -> IO String
jsStringToString string = do
  stringSize <- {#call JSStringGetMaximumUTF8CStringSize as ^ #} string
  allocaArray (fromIntegral stringSize) $ \buffer -> do
   {#call JSStringGetUTF8CString as ^ #} string buffer stringSize
   peekCString buffer
  
withJSStringArrayLen :: [String] -> (Int -> Ptr JSString -> IO c) -> IO c
withJSStringArrayLen strings action = 
  go strings $ \jsStrings ->
  withArrayLen jsStrings action
    where 
      -- maybe we want explicit recursion here
      -- or something different which I can't seem to figure out atm
      go strings action = foldl (\action string jsStrings -> 
                                 withJSString string $ \jsString -> 
                                 action (jsString : jsStrings)) action strings []


  -- JSStringIsEqual