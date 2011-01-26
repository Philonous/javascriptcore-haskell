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
-- This module provides low level language bindings to the 
-- Jack Audio Connection Kit (http://jackaudio.org ). 
-- It is intended to be a faithfull representation of the C include files.
-- Parameters are converted only where the meaning is obvious. 

{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Gtk.WebKit.JSBase where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types

#include <JavaScriptCore/JSBase.h>

{# pointer JSContextGroupRef		 as JSContextGroup            newtype #}
{# pointer JSContextRef			 as JSContext    	      newtype #}
{# pointer JSGlobalContextRef		 as JSGlobalContext    	      newtype #}
{# pointer JSStringRef			 as JSString     	      newtype #}
{# pointer JSClassRef			 as JSClass     	      newtype #}
{# pointer JSPropertyNameArrayRef        as JSPropertyNameArray       newtype #}
{# pointer JSPropertyNameAccumulatorRef  as JSPropertyNameAccumulator newtype #}
{# pointer JSValueRef                    as JSValue                   newtype #}
{# pointer JSObjectRef                   as JSObject                  newtype #}

deriving instance Storable JSValue

jsEvaluateScript  :: JSContext
     -> JSString
     -> JSObject
     -> JSString
     -> Foreign.C.Types.CInt
     -> IO (JSValue, JSValue)
jsEvaluateScript ctx script thisobject sourceURL startingLineNumber = 
  alloca $ \exception -> do
    res <- {#call unsafe JSEvaluateScript as jsEvaluateScript_#}
             ctx script thisobject sourceURL startingLineNumber exception
    ex <- peek exception
    return (res, ex)
    
jsCheckScriptSyntax ctx script sourceURL startingLineNumber = 
  alloca $ \exception -> do
    res <- jsCheckScriptSyntax_
             ctx script sourceURL startingLineNumber exception
    ex <- peek exception
    return (res, ex)

-- C->HS doesn't seem to like the corresponding C prototype.
foreign import ccall unsafe "JSBase.chs.h JSCheckScriptSyntax"
  jsCheckScriptSyntax_ :: ((JSContext) -> ((JSString) -> ((JSString) -> (CInt -> ((Ptr (JSValue)) -> (IO (JSValue)))))))

jsGarbageCollect = {# call unsafe JSGarbageCollect as jsGarbageCollect_ #}

