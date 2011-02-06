-- Haskell bindings to the WebKit JavaScriptCore library
--
-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- License: MIT; See LICENSE file
--
-- Description
--
-- Language : Haskell 98
--


{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase where

import Foreign
import Foreign.C

#include <JavaScriptCore/JSBase.h>

{# pointer JSContextGroupRef             as JSContextGroup    foreign newtype #}
{# pointer JSContextRef                  as JSContext         foreign newtype #}
{# pointer JSGlobalContextRef            as JSGlobalContext   foreign newtype #}
{# pointer JSStringRef                   as JSString          foreign newtype #}
{# pointer JSClassRef                    as JSClass           foreign newtype #}
{# pointer JSPropertyNameArrayRef        as JSPropertyNameArray foreign newtype #}
{# pointer JSPropertyNameAccumulatorRef  as JSPropertyNameAccumulator newtype #}
{# pointer JSValueRef                    as JSValue                   newtype #}
{# pointer JSObjectRef                   as JSObject                  newtype #}


deriving instance Storable JSValue

jsEvaluateScript  :: JSContext
     -> JSString
     -> JSObject
     -> JSString
     -> CInt
     -> IO (JSValue, JSValue)
jsEvaluateScript ctx script thisobject sourceURL startingLineNumber = 
  withJSContext ctx        $ \ctx ->
  withJSString  script     $ \script ->
  withJSString  sourceURL  $ \sourceURL ->
  alloca                   $ \exception -> do
    res <- {#call unsafe JSEvaluateScript as ^ #}
             ctx script thisobject sourceURL startingLineNumber exception
    ex <- peek exception
    return (res, ex)
    
jsCheckScriptSyntax ctx script sourceURL startingLineNumber = 
  withJSContext ctx        $ \ctx ->
  withJSString  script     $ \script ->
  withJSString  sourceURL  $ \sourceURL ->
  alloca $ \exception -> do
    res <- jsCheckScriptSyntax_
             ctx script sourceURL startingLineNumber exception
    ex <- peek exception
    return (toBool res, ex)

-- TODO: doctor the C include to make it return an UInt
foreign import ccall unsafe "JSBase.chs.h JSCheckScriptSyntax"
  jsCheckScriptSyntax_ :: ((Ptr JSContext) -> ((Ptr JSString) -> ((Ptr JSString) -> (CInt -> ((Ptr (JSValue)) -> (IO (CUInt)))))))

jsGarbageCollect = {# call unsafe JSGarbageCollect as jsGarbageCollect_ #}

