{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef where

import Foreign
import Foreign.C


import Data.Bool
import Control.Applicative ((<$>))

#include <JavaScriptCore/JSValueRef.h>

{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.Util #}
{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase #}
{# import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef #}


{#enum JSType {upcaseFirstLetter} deriving(Eq, Bounded)#}

jsValueGetType :: JSContext -> JSValue -> IO JSType
jsValueGetType ctx value = 
  withJSContext ctx $ \ctx -> 
  toEnum . fromIntegral <$> {#call JSValueGetType as ^ #} ctx value

-- JSValueIs<Type> seems pointless to bind

jsValueIsObjectOfClass ctx value jsClass = 
  withJSContext ctx $ \ctx -> 
  withJSClass   jsClass $ \jsClass -> 
  toBool <$> {# call JSValueIsObjectOfClass as ^ #} ctx value jsClass
  
jsValueIsEqual ctx a b = 
  withJSContext ctx $ \ctx -> 
  withException $ \exception -> do 
    toBool <$> {# call JSValueIsEqual as ^ #} ctx a b exception


jsValueIsStrictEqual ctx a b = 
  withJSContext ctx $ \ctx -> 
     toBool <$> {# call JSValueIsStrictEqual as ^ #} ctx a b 

jsValueIsInstanceOfConstructor ctx value constructor =
  withJSContext ctx $ \ctx -> 
  withException $ \exception ->
    toBool <$> {# call JSValueIsInstanceOfConstructor as ^ #} 
           ctx value constructor exception


jsValueMakeUndefined ctx =
  withJSContext ctx $ \ctx -> {# call JSValueMakeUndefined as ^ #} ctx
jsValueMakeNull      ctx =
  withJSContext ctx $ \ctx -> {# call JSValueMakeNull     as ^ #} ctx
jsValueMakeBoolean ctx boolean =
  withJSContext ctx $ \ctx -> {# call JSValueMakeBoolean  as ^ #} ctx (fromBool boolean)
jsValueMakeNumber    ctx number =
  withJSContext ctx $ \ctx -> {# call JSValueMakeNumber   as ^ #} ctx (realToFrac number)
jsValueMakeString    ctx string=
  withJSString string $ \string -> 
  withJSContext ctx $ \ctx -> {# call JSValueMakeString   as ^ #} ctx string
                              
jsValueToBoolean    ctx value = 
  withJSContext ctx $ \ctx -> 
    toBool <$> {# call JSValueToBoolean as ^ #} ctx value

jsValueToNumber     ctx value =
  withJSContext ctx $ \ctx -> 
  withException $ \exception ->
    realToFrac <$> {# call JSValueToNumber as ^ #} ctx value exception


jsValueToString ctx value = 
  withJSContext ctx $ \ctx -> 
  withException $ \exception -> do
    jsString <- {# call JSValueToStringCopy as ^ #} ctx value exception
    string <- jsStringToString jsString
    jsStringRelease jsString
    return string


jsValueToObject     ctx value = 
  withJSContext ctx $ \ctx -> 
  withException $ \exception ->
    {# call JSValueToObject as ^ #} ctx value exception

  
jsValueProtect ctx value =
  withJSContext ctx $ \ctx -> 
  {# call JSValueProtect as ^ #} ctx value
                              
jsValueUnprotect ctx value =
  withJSContext ctx $ \ctx -> 
  {# call JSValueUnprotect as ^ #} ctx value

data JSValue' = JSUndefinedVal
              | JSNullVal
              | JSBooleanVal Bool
              | JSNumberVal Double
              | JSStringVal String
              | JSObjectVal JSObject

fromJSValue ctx jsValue = do
  valType <- jsValueGetType ctx jsValue
  case valType of 
    KJSTypeUndefined -> return $ Right JSUndefinedVal
    KJSTypeNull      -> return $ Right JSNullVal
    KJSTypeBoolean   -> Right . JSBooleanVal <$> jsValueToBoolean ctx jsValue
    KJSTypeNumber    -> fmap JSNumberVal     <$> jsValueToNumber  ctx jsValue
    KJSTypeString    -> fmap JSStringVal     <$> jsValueToString  ctx jsValue
    KJSTypeObject    -> fmap JSObjectVal     <$> jsValueToObject  ctx jsValue

toJSValue ctx value = 
  case value of 
    JSUndefinedVal          -> jsValueMakeUndefined ctx
    JSNullVal               -> jsValueMakeNull      ctx
    JSBooleanVal   bool     -> jsValueMakeBoolean   ctx bool
    JSNumberVal    double   -> jsValueMakeNumber    ctx double
    JSStringVal    string   -> jsValueMakeString    ctx string
    JSObjectVal    jsObject -> error "toJSValue on Object"
