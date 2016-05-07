{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Cosmostrix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Cosmostrix <cosmos@lunakit.org>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Datatypes corresponding to vk.xml
----------------------------------------------------------------------------
module Convertor (mapToHask) where
import Data.List (elemIndices)
import VkRegistry

haskType :: [String] -> [String]
haskType = map $ \case
  "*" -> "Ptr"
  "void" -> "()"
  "char" -> "CChar"
  "float" -> "Float"
  "uint8_t" -> "Word8"
  "uint32_t" -> "Word32"
  "uint64_t" -> "Word64"
  "int32_t" -> "Int32"
  "size_t" -> "CSize"
  "wl_display" -> "WlDisplay"
  "wl_surface" -> "WlSurface"
  "xcb_connection_t" -> "XcbConnection"
  "xcb_visualid_t" -> "XcbVisualId"
  "xcb_window_t" -> "XcbWindow"
  x -> x

haskType' :: [String] -> [String]
haskType' ["void"] = ["Ptr", "()"]
haskType' xs = haskType xs

mapToHask :: Registry -> Registry
mapToHask (Registry {..}) = Registry
  { registryTypes = flip map registryTypes $ \case
      x@(Struct {..}) -> x { typeMembers = map mappingMember typeMembers }
      x@(Union {..}) -> x { typeMembers = map mappingMember typeMembers }
      x@(RequireType {..}) -> x { typeName = head $ haskType [typeName] }
      x@(Basetype {..}) -> x { typeType = head $ haskType [typeType] }
      x@(Handle { typeType = "VK_DEFINE_HANDLE", ..}) ->
        x { typeType = "Ptr ()" }
      x@(Handle { typeType = "VK_DEFINE_NON_DISPATCHABLE_HANDLE", ..}) ->
        x { typeType = "Int64" }
      x@(Funcpointer {..}) ->
        x { typeArgs = map haskType' typeArgs ++ ["IO" : funcpointerReturns typeName] }
      rest -> rest
  , registryEnums = registryEnums
  , registryCommands = flip map registryCommands $ \x@(Command {..}) ->
      x { commandReturn = haskType commandReturn
        , commandParameters = map mappingMember commandParameters
        }
  , registryFeatures = flip map registryFeatures $ \x@(Feature {..}) ->
      if featureName == "VK_VERSION_1_0"
      then x { featureRequires = drop 3 featureRequires }
      else x
  , registryExtensions = registryExtensions
  }

mappingMember :: Member -> Member
-- avoid haskell keywords
mappingMember p@(Member { memberName = "instance", .. }) =
  mappingMember p { memberName = "vulkan" }
mappingMember p@(Member { memberName = "type", .. }) =
  mappingMember p { memberName = "imageType" }
mappingMember p@(Member { memberName = "data", .. }) =
  mappingMember p { memberName = "word" }
mappingMember p@(Member { memberName = "module", .. }) =
  mappingMember p { memberName = "shaderModule" }
-- fix type
--mappingMember m@(Member { memberType = ["char"], .. }) =
--  m { memberType = case length . elemIndices ',' <$> memberLen of
--                        Nothing -> ["CString"] -- *char[enum]
--                        Just 0 -> ["CString"]
--                        Just 1 -> ["Ptr", "CString"]
--    }
mappingMember m@(Member { memberEnum = Just enum, .. }) = case enum of
  "[2]" -> m { memberType = "V2" : haskType memberType, memberEnum = Nothing }
  "[3]" -> m { memberType = "V3" : haskType memberType, memberEnum = Nothing }
  "[4]" -> m { memberType = "V4" : haskType memberType, memberEnum = Nothing }
  enum -> m { memberType = "Ptr" : haskType memberType } -- !!!!
mappingMember m@(Member {..}) = m { memberType = haskType memberType }

funcpointerReturns :: String -> [String]
funcpointerReturns "PFN_vkInternalAllocationNotification" = ["()"]
funcpointerReturns "PFN_vkInternalFreeNotification" = ["()"]
funcpointerReturns "PFN_vkReallocationFunction" = ["Ptr", "()"]
funcpointerReturns "PFN_vkAllocationFunction" = ["Ptr", "()"]
funcpointerReturns "PFN_vkFreeFunction" = ["()"]
funcpointerReturns "PFN_vkVoidFunction" = ["FunPtr", "()"]
funcpointerReturns "PFN_vkDebugReportCallbackEXT" = ["VkBool32"]

