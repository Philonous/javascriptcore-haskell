{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String

#include <JavaScriptCore/JSBase.h>


{#enum JSType {upcaseFirstLetter} deriving(Eq, Bounded)#}

