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
  poke struct (JSStaticValue name getProperty setProperty attributes) = do
    {# set JSStaticValue.name        #} struct name       
    {# set JSStaticValue.getProperty #} struct getProperty
    {# set JSStaticValue.setProperty #} struct setProperty
    {# set JSStaticValue.attributes  #} struct attributes 
                                                                   
data JSStaticFunction = JSStaticFunction
                          CString
                          JSObjectCallAsFunctionCallbackPtr
                          CUInt
                          
#c
int JSStaticFunctionAlign  () { return (ALIGNOF(JSStaticFunction  ));}
#endc

instance Storable JSStaticFunction where
  sizeOf _ = {# sizeof JSStaticFunction #}
  alignment _ = fromIntegral {# call pure unsafe JSStaticFunctionAlign as jSStaticFunctionAlign #}
  peek struct = JSStaticFunction
    `fmap` {# get JSStaticFunction.name           #} struct
    `ap`   {# get JSStaticFunction.callAsFunction #} struct
    `ap`   {# get JSStaticFunction.attributes     #} struct
  poke struct (JSStaticFunction name callAsFunction attributes) = do
    {# set JSStaticFunction.name           #} struct name       
    {# set JSStaticFunction.callAsFunction #} struct callAsFunction
    {# set JSStaticFunction.attributes     #} struct attributes 


data JSClassDefinition = JSClassDefinition 
                           CInt 
                           CUInt
                           CString
                           (Ptr ())
                           (Ptr ())
                           (Ptr ())
                           JSObjectInitializeCallbackPtr 
                           JSObjectFinalizeCallbackPtr 
                           JSObjectHasPropertyCallbackPtr 
                           JSObjectGetPropertyCallbackPtr 
                           JSObjectSetPropertyCallbackPtr 
                           JSObjectDeletePropertyCallbackPtr 
                           JSObjectGetPropertyNamesCallbackPtr 
                           JSObjectCallAsFunctionCallbackPtr 
                           JSObjectCallAsConstructorCallbackPtr 
                           JSObjectHasInstanceCallbackPtr 
                           JSObjectConvertToTypeCallbackPtr 

#c
int JSClassDefinitionAlign () { return (ALIGNOF(JSClassDefinition ));}
#endc

instance Storable JSClassDefinition where
  sizeOf _ = {# sizeof JSClassDefinition #}
  alignment _ = fromIntegral {# call pure unsafe JSClassDefinitionAlign as jSClassDefinitionAlign #}
  peek struct = JSClassDefinition
    `fmap` {# get JSClassDefinition.version           #} struct
    `ap`   {# get JSClassDefinition.attributes	      #} struct
    `ap`   {# get JSClassDefinition.className         #} struct
    `ap`   {# get JSClassDefinition.parentClass       #} struct
    `ap`   {# get JSClassDefinition.staticValues      #} struct  
    `ap`   {# get JSClassDefinition.staticFunctions   #} struct
    `ap`   {# get JSClassDefinition.initialize	      #} struct
    `ap`   {# get JSClassDefinition.finalize	      #} struct
    `ap`   {# get JSClassDefinition.hasProperty       #} struct  
    `ap`   {# get JSClassDefinition.getProperty       #} struct  
    `ap`   {# get JSClassDefinition.setProperty       #} struct  
    `ap`   {# get JSClassDefinition.deleteProperty    #} struct  
    `ap`   {# get JSClassDefinition.getPropertyNames  #} struct
    `ap`   {# get JSClassDefinition.callAsFunction    #} struct  
    `ap`   {# get JSClassDefinition.callAsConstructor #} struct
    `ap`   {# get JSClassDefinition.hasInstance       #} struct  
    `ap`   {# get JSClassDefinition.convertToType     #} struct  
  poke struct (JSClassDefinition 
                version          
                attributes	     
                className        
                parentClass      
                staticValues     
                staticFunctions  
                initialize	     
                finalize	     
                hasProperty      
                getProperty      
                setProperty      
                deleteProperty   
                getPropertyNames 
                callAsFunction   
                callAsConstructor
                hasInstance      
                convertToType    
                ) = do
    {# set JSClassDefinition.version           #} struct version          
    {# set JSClassDefinition.attributes	       #} struct attributes	      
    {# set JSClassDefinition.className         #} struct className        
    {# set JSClassDefinition.parentClass       #} struct parentClass      
    {# set JSClassDefinition.staticValues      #} struct staticValues     
    {# set JSClassDefinition.staticFunctions   #} struct staticFunctions  
    {# set JSClassDefinition.initialize	       #} struct initialize	      
    {# set JSClassDefinition.finalize	       #} struct finalize	      
    {# set JSClassDefinition.hasProperty       #} struct hasProperty      
    {# set JSClassDefinition.getProperty       #} struct getProperty      
    {# set JSClassDefinition.setProperty       #} struct setProperty      
    {# set JSClassDefinition.deleteProperty    #} struct deleteProperty   
    {# set JSClassDefinition.getPropertyNames  #} struct getPropertyNames 
    {# set JSClassDefinition.callAsFunction    #} struct callAsFunction   
    {# set JSClassDefinition.callAsConstructor #} struct callAsConstructor
    {# set JSClassDefinition.hasInstance       #} struct hasInstance      
    {# set JSClassDefinition.convertToType     #} struct convertToType    

