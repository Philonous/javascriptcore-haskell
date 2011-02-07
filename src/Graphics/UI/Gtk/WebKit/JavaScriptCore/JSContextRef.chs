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

{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef where

import Foreign
import Foreign.C

import Control.Monad 
import Control.Applicative((<$>))
#include <JavaScriptCore/JSContextRef.h>

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.Util #}
--{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase #}
--{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef #}
--{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef #}

foreign import ccall unsafe "JSBase.chs.h &JSContextGroupRelease"
  jsContextGroupReleasePtr :: FinalizerPtr JSContextGroup

toJSContextGroup jsContextGroupPtr = 
  JSContextGroup <$> newForeignPtr jsContextGroupReleasePtr jsContextGroupPtr

jsContextGroupCreate = toJSContextGroup =<< {#call JSContextGroupCreate as ^ #}
jsContextGroupRetain = toJSContextGroup <=< {#call JSContextGroupRetain as ^ #}


foreign import ccall unsafe "JSBase.chs.h &JSGlobalContextRelease"
  jsGlobalContextReleasePtr :: FinalizerPtr JSGlobalContext

toJSGlobalContext :: Ptr JSGlobalContext -> IO JSGlobalContext
toJSGlobalContext jsGlobalContextPtr = 
  JSGlobalContext <$> newForeignPtr jsGlobalContextReleasePtr jsGlobalContextPtr

jsGlobalContextCreate globalObjectClass = 
  withJSClass globalObjectClass $ \globalObjectClass -> 
  toJSGlobalContext =<< {#call JSGlobalContextCreate as ^ #} globalObjectClass

jsGlobalContextCreateInGroup :: JSContextGroup -> JSClass -> IO JSGlobalContext
jsGlobalContextCreateInGroup group globalObjectClass  = 
  withJSContextGroup group $ \group ->
  withJSClass globalObjectClass $ \globalObjectClass -> do
    jsGlobalContextPtr <- {#call JSGlobalContextCreateInGroup as ^ #} 
                            group globalObjectClass
    toJSGlobalContext jsGlobalContextPtr

jsGlobalContextRetain ctx = 
  withJSGlobalContext ctx $ \ctx -> 
  toJSGlobalContext =<< {#call JSGlobalContextRetain as ^ #} ctx

jsContextGetGlobalObject ctx =
  withJSContext ctx $ \ctx -> 
  {#call JSContextGetGlobalObject as ^ #} ctx
  
jsContextGetGroup  ctx = 
  withJSContext ctx $ \ctx -> 
  {#call JSContextGetGroup as ^ #} ctx


