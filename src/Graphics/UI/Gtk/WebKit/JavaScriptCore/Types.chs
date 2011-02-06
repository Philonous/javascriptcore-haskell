module Graphics.UI.Gtk.WebKit.JavaScriptCore.Types where

import Foreign.Ptr
import Foreign.C.Types

import System.Glib.Flags

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase#}

#include <JavaScriptCore/JSObjectRef.h>
#include <JSObjectRef.wrapper.h>

{# enum JSPropertyAttribute {upcaseFirstLetter} deriving(Eq, Bounded)#}
instance Flags JSPropertyAttribute

{# enum JSClassAttribute {upcaseFirstLetter} deriving(Eq, Bounded)#}
instance Flags JSClassAttribute

type JSObjectInitializeCallbackPtr     = {#type JSObjectInitializeCallback       #}
type JSObjectFinalizeCallbackPtr       = {#type JSObjectFinalizeCallback         #}
type JSObjectHasPropertyCallbackPtr    = {#type JSObjectHasPropertyCallback      #}
type JSObjectGetPropertyCallbackPtr    = {#type JSObjectGetPropertyCallback      #}
type JSObjectSetPropertyCallbackPtr    = {#type JSObjectSetPropertyCallback      #}
type JSObjectDeletePropertyCallbackPtr = {#type JSObjectDeletePropertyCallback   #}
type JSObjectGetPropertyNamesCallbackPtr= {#type JSObjectGetPropertyNamesCallback #}
type JSObjectCallAsFunctionCallbackPtr = {#type JSObjectCallAsFunctionCallback   #}
type JSObjectCallAsConstructorCallbackPtr = {#type JSObjectCallAsConstructorCallback#}
type JSObjectHasInstanceCallbackPtr    = {#type JSObjectHasInstanceCallback      #}
type JSObjectConvertToTypeCallbackPtr  = {#type JSObjectConvertToTypeCallback    #}

{# pointer * JSStaticValue     as JSStaticValue     #}
