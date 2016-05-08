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
        , commandParameters =
            specialFix commandName $ map mappingMember commandParameters
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
mappingMember m@(Member { memberEnum = Just enum, .. }) = case enum of
  "[2]" -> m { memberType = "V2" : haskType memberType }
  "[3]" -> m { memberType = "V3" : haskType memberType }
  "[4]" -> m { memberType = "V4" : haskType memberType }
  "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE" -> m { memberType = "V 256" : haskType memberType }
  "VK_UUID_SIZE" -> m { memberType = "V 16" : haskType memberType }
  "VK_MAX_EXTENSION_NAME_SIZE" -> m { memberType = "V 256" : haskType memberType }
  "VK_MAX_DESCRIPTION_SIZE" -> m { memberType = "V 256" : haskType memberType }
  "VK_MAX_MEMORY_TYPES" -> m { memberType = "V 32" : haskType memberType }
  "VK_MAX_MEMORY_HEAPS" -> m { memberType = "V 16" : haskType memberType }
  enum -> error $ show m { memberType = haskType memberType }
mappingMember m@(Member {..}) = m { memberType = haskType memberType }

specialFix :: String -> [Member] -> [Member]
specialFix "vkCmdSetBlendConstants" (m1 : _) =
  m1 : [f "blendConstant1", f "blendConstant2", f "blendConstant3", f "blendConstant4"]
  where f x = Member x ["Float"] False Nothing "" "" Nothing False
specialFix _ p = p

funcpointerReturns :: String -> [String]
funcpointerReturns "PFN_vkInternalAllocationNotification" = ["()"]
funcpointerReturns "PFN_vkInternalFreeNotification" = ["()"]
funcpointerReturns "PFN_vkReallocationFunction" = ["Ptr", "()"]
funcpointerReturns "PFN_vkAllocationFunction" = ["Ptr", "()"]
funcpointerReturns "PFN_vkFreeFunction" = ["()"]
funcpointerReturns "PFN_vkVoidFunction" = ["()"]
funcpointerReturns "PFN_vkDebugReportCallbackEXT" = ["VkBool32"]

