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

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.Util #}
{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef #}

-- Type definitions are in <Util>

jsEvaluateScript  :: JSContext
     -> String
     -> JSObject
     -> String
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
    res <- {# call JSCheckScriptSyntax as ^ #}
             ctx script sourceURL startingLineNumber exception
    ex <- peek exception
    return (toBool res, ex)


jsGarbageCollect = {# call unsafe JSGarbageCollect as jsGarbageCollect_ #}

