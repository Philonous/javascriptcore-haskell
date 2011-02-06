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

#include <JavaScriptCore/JSContextRef.h>

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase #}
{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef #}
{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef #}