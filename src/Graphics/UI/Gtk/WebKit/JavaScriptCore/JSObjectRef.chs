{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef where

import Control.Monad (ap)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String

import System.Glib.Flags

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase#}

#include <JavaScriptCore/JSObjectRef.h>
#include <JSObjectRef.wrapper.h>
-- #include <alignment.h>

#include <stddef.h>

#c
#define ALIGNOF(type) offsetof (struct { char c; type member; }, member)
#endc


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

data JSStaticValue = JSStaticValue 
                       CString 
                       JSObjectGetPropertyCallbackPtr
                       JSObjectSetPropertyCallbackPtr
                       CUInt

#c
int JSStaticValueAlign     () { return (ALIGNOF(JSStaticValue     ));}
#endc

instance Storable JSStaticValue where
  sizeOf _ = {# sizeof JSStaticValue #}
  alignment _ = fromIntegral {# call pure unsafe JSStaticValueAlign as jssva #}
  peek struct = JSStaticValue
    `fmap` {# get JSStaticValue.name        #} struct
    `ap`   {# get JSStaticValue.getProperty #} struct
    `ap`   {# get JSStaticValue.setProperty #} struct
    `ap`   {# get JSStaticValue.attributes  #} struct
  poke ptr  = undefined
    


{# pointer * JSStaticFunction  as JSStaticFunction  #}

#c
int JSStaticFunctionAlign  () { return (ALIGNOF(JSStaticFunction  ));}
#endc

withJSStaticFunction name callAsFunction attribute action = 
  alloca $ \struct -> do 
    {#set JSStaticFunction.name           #} struct name
    {#set JSStaticFunction.callAsFunction #} struct callAsFunction
    {#set JSStaticFunction.attributes     #} struct attribute
    action struct

{# pointer * JSClassDefinition as JSClassDefinition #}

#c
int JSClassDefinitionAlign () { return (ALIGNOF(JSClassDefinition ));}
#endc