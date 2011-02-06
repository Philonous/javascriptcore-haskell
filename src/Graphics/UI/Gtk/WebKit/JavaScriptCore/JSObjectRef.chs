{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef where

import Control.Monad (ap)
import Control.Applicative ((<$>))
import Foreign
import Foreign.C

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

type JSObjectInitializeCallbackPtr     
  = {# type JSObjectInitializeCallback       #}
type JSObjectFinalizeCallbackPtr       
  = {# type JSObjectFinalizeCallback         #}
type JSObjectHasPropertyCallbackPtr    
  = {# type JSObjectHasPropertyCallback      #}
type JSObjectGetPropertyCallbackPtr    
  = {# type JSObjectGetPropertyCallback      #}
type JSObjectSetPropertyCallbackPtr    
  = {# type JSObjectSetPropertyCallback      #}
type JSObjectDeletePropertyCallbackPtr 
  = {# type JSObjectDeletePropertyCallback   #}
type JSObjectGetPropertyNamesCallbackPtr
  = {# type JSObjectGetPropertyNamesCallback #}
type JSObjectCallAsFunctionCallbackPtr 
  = {# type JSObjectCallAsFunctionCallback   #}
type JSObjectCallAsConstructorCallbackPtr 
  = {# type JSObjectCallAsConstructorCallback#}
type JSObjectHasInstanceCallbackPtr    
  = {# type JSObjectHasInstanceCallback      #}
type JSObjectConvertToTypeCallbackPtr  
  = {# type JSObjectConvertToTypeCallback    #}

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
  alignment _ = fromIntegral {# call pure unsafe JSStaticValueAlign as ^ #}
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
  alignment _ = fromIntegral {# call pure unsafe JSStaticFunctionAlign as ^ #}
  peek struct = JSStaticFunction
    `fmap` {# get JSStaticFunction.name           #} struct
    `ap`   {# get JSStaticFunction.callAsFunction #} struct
    `ap`   {# get JSStaticFunction.attributes     #} struct
  poke struct (JSStaticFunction name callAsFunction attributes) = do
    {# set JSStaticFunction.name           #} struct name       
    {# set JSStaticFunction.callAsFunction #} struct callAsFunction
    {# set JSStaticFunction.attributes     #} struct attributes 


data JSClassDefinition = JSClassDefinition 
  { jsClassDefinitionVersion           :: CInt 
  , jsClassDefinitionAttributes        :: CUInt
  , jsClassDefinitionClassName         :: CString
  , jsClassDefinitionParentClass       :: (Ptr ())
  , jsClassDefinitionStaticValues      :: (Ptr ())
  , jsClassDefinitionStaticFunctions   :: (Ptr ())
  , jsClassDefinitionInitialize        :: JSObjectInitializeCallbackPtr 
  , jsClassDefinitionFinalize          :: JSObjectFinalizeCallbackPtr 
  , jsClassDefinitionHasProperty       :: JSObjectHasPropertyCallbackPtr 
  , jsClassDefinitionGetProperty       :: JSObjectGetPropertyCallbackPtr 
  , jsClassDefinitionSetProperty       :: JSObjectSetPropertyCallbackPtr 
  , jsClassDefinitionDeleteProperty    :: JSObjectDeletePropertyCallbackPtr 
  , jsClassDefinitionGetPropertyNames  :: JSObjectGetPropertyNamesCallbackPtr 
  , jsClassDefinitionCallAsFunction    :: JSObjectCallAsFunctionCallbackPtr 
  , jsClassDefinitionCallAsConstructor :: JSObjectCallAsConstructorCallbackPtr 
  , jsClassDefinitionHasInstance       :: JSObjectHasInstanceCallbackPtr 
  , jsClassDefinitionConvertToType     :: JSObjectConvertToTypeCallbackPtr 
  }

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


foreign import ccall unsafe "JSBase.chs.h &JSClassRelease"
  jsClassReleasePtr :: FinalizerPtr JSClass
                       
toJSClass jsClassPtr = JSClass <$> newForeignPtr jsClassReleasePtr jsClassPtr

jsClassCreate :: JSClassDefinition -> IO JSClass
jsClassCreate definition = do 
  jsClassRefPtr <- with definition $ {#call JSClassCreate as ^ #} . castPtr
  toJSClass jsClassRefPtr

jsClassRetain jsClass = do
  jsRetained <- withJSClass jsClass {# call JSClassRetain as ^ #}
  toJSClass jsRetained
  
jsObjectMake ctx jsClass data_ = 
    withJSContext ctx     $ \ctx'     ->
    withJSClass   jsClass $ \jsClass' ->
      {# call JSObjectMake as ^ #} ctx' jsClass' data_

jsObjectMakeFunctionWithCallback ctx name callAsFunction = 
  withJSContext ctx  $ \ctx  -> 
  withJSString  name $ \name -> 
    {#call JSObjectMakeFunctionWithCallback as ^ #} ctx name callAsFunction

jsObjectMakeConstructor ctx jsClass callAsConstructor = 
  withJSContext ctx     $ \ctx     -> 
  withJSClass   jsClass $ \jsClass ->
  {# call JSObjectMakeConstructor as ^ #} ctx jsClass callAsConstructor

withCtxArgsEx action ctx arguments =
  withJSContext ctx      $ \ctx                     ->
  withArrayLen arguments $ \argumentCount arguments ->
  alloca                 $ \exception               -> do
    ret <- action ctx (fromIntegral argumentCount) arguments exception
    ex <- peek exception
    return (ret, ex)

jsObjectMakeArray  = withCtxArgsEx {#call JSObjectMakeArray  as ^ #}
jsObjectMakeDate   = withCtxArgsEx {#call JSObjectMakeDate   as ^ #}
jsObjectMakeError  = withCtxArgsEx {#call JSObjectMakeError  as ^ #}
jsObjectMakeRegExp = withCtxArgsEx {#call JSObjectMakeRegExp as ^ #}
    
jsObjectMakeFunction ctx name parameterNames body sourceURL startingLineNumber =
  withJSContext ctx $ \ctx ->
  withJSString name $ \name ->
  withArrayLen parameterNames $ \parameterCount parameterNames ->
  withJSString body $ \body ->
  withJSString sourceURL $ \sourceURL ->
  alloca $ \exception  -> do 
    ret <- {#call JSObjectMakeFunction as ^  #}
      ctx name (fromIntegral parameterCount) parameterNames body sourceURL 
      (fromIntegral startingLineNumber) exception
    ex <- peek exception
    return (ret, ex)

jsObjectGetPrototype ctx object = 
  withJSContext ctx $ \ctx ->
  {# call JSObjectGetPrototype as ^ #} ctx object

jsObjectSetPrototype ctx object value =
  withJSContext ctx $ \ctx ->
  {# call JSObjectSetPrototype as ^ #} ctx object value
  
jsObjectHasProperty ctx object propertyName =
  withJSContext ctx $ \ctx ->
  withJSString propertyName $ \propertyName ->
  toBool <$> {# call JSObjectHasProperty as ^ #} ctx object propertyName
  
jsObjectGetProperty ctx object propertyName =
  withJSContext ctx $ \ctx ->
  withJSString propertyName $ \propertyName ->
  alloca $ \exception -> do
    ret <- {#call JSObjectGetProperty as ^  #}
        ctx object propertyName exception
    ex <- peek exception
    return (ret, ex)

jsObjectSetProperty ctx object propertyName value attributes=
  withJSContext ctx $ \ctx ->
  withJSString propertyName $ \propertyName ->
  alloca $ \exception -> do
    ret <- {#call JSObjectSetProperty as ^ #}
             ctx object propertyName value 
             (fromIntegral . fromFlags $ attributes) exception
    ex <- peek exception
    return (ret, ex)

jsObjectDeleteProperty ctx object propertyName =
  withJSContext ctx $ \ctx ->
  withJSString propertyName $ \propertyName ->
  alloca $ \exception -> do
    ret <- {#call JSObjectDeleteProperty as ^ #}
             ctx object propertyName exception
    ex <- peek exception
    return (ret, ex)

jsObjectGetPropertyAtIndex ctx object propertyIndex =
  withJSContext ctx $ \ctx ->
  alloca $ \exception -> do
    ret <- {#call JSObjectGetPropertyAtIndex as ^ #}
             ctx object (fromIntegral propertyIndex) exception
    ex <- peek exception
    return (ret, ex)

jsObjectSetPropertyAtIndex ctx object propertyIndex value =
  withJSContext ctx $ \ctx ->
  alloca $ \exception -> do
    ret <- {#call JSObjectSetPropertyAtIndex as ^ #}
             ctx object (fromIntegral propertyIndex) value 
             exception
    ex <- peek exception
    return (ret, ex)
    
jsObjectGetPrivate = {# call JSObjectGetPrivate as ^ #}
jsObjectSetPrivate = {# call JSObjectSetPrivate as ^ #}

jsObjectIsFunction ctx object = 
  withJSContext ctx $ \ctx ->
  toBool <$> {#call JSObjectIsFunction as ^ #} ctx object

jsObjectCallAsFunction ctx object thisObject arguments = 
  withJSContext ctx $ \ctx ->
  withArrayLen arguments $ \argumentCount arguments ->
  alloca $ \exception -> do
    ret <- {#call JSObjectCallAsFunction as ^ #}
             ctx object thisObject (fromIntegral argumentCount) arguments exception
    ex <- peek exception
    return (ret, ex)

jsObjectIsConstructor ctx object =
  withJSContext ctx $ \ctx ->
  toBool <$> {#call JSObjectIsConstructor as ^ #} ctx object

jsObjectCallAsContructor ctx object arguments = 
  withJSContext ctx $ \ctx ->
  withArrayLen arguments $ \argumentCount arguments ->
  alloca $ \exception -> do
    ret <- {#call JSObjectCallAsConstructor as ^ #}
             ctx object (fromIntegral argumentCount) arguments exception
    ex <- peek exception
    return (ret, ex)

foreign import ccall unsafe "JSBase.chs.h &JSPropertyNameArrayRelease"
  jsPropertyNameArrayReleasePtr :: FinalizerPtr JSPropertyNameArray
                                   
toJSPropertyNameArray jsPropertyNameArrayPtr = 
  JSPropertyNameArray <$> newForeignPtr jsPropertyNameArrayReleasePtr
    jsPropertyNameArrayPtr

jsObjectCopyPropertyNames ctx object =
  withJSContext ctx $ \ctx -> {# call JSObjectCopyPropertyNames as ^ #} ctx object
                               >>= toJSPropertyNameArray

jsPropertyNameArrayRetain array = 
  withJSPropertyNameArray array $ \array ->
  {# call JSPropertyNameArrayRetain as ^ #} array 
   >>= toJSPropertyNameArray
   
jsPropertyNameArrayGetCount array = 
  withJSPropertyNameArray array $ \array ->
  fromIntegral <$> {# call JSPropertyNameArrayGetCount  as ^ #} array
  
jsPropertyNameArrayGetNameAtIndex array index = 
  withJSPropertyNameArray array $ \array ->
  {# call JSPropertyNameArrayGetNameAtIndex as ^ #} array index

jsPropertyNameAccumulatorAddName accumulator propertyName = 
  withJSString propertyName $ \propertyName -> 
  {# call JSPropertyNameAccumulatorAddName as ^ #} accumulator propertyName
  

