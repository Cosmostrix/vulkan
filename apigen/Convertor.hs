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

haskType :: String -> String
haskType = \case
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

haskType' :: String -> String
haskType' "void" = "Ptr ()" -- for *pNext
haskType' rest = haskType rest

mapToHask :: Registry -> Registry
mapToHask (Registry {..}) = Registry
  { registryTypes = flip map registryTypes $ \case
      x@(Struct { typeName = "VkImageBlit", ..}) ->
        x { typeMembers =
              [ Member "srcSubresource" "VkImageSubresourceLayers" Nothing False Nothing False
              , Member "srcOffset0" "VkOffset3D" Nothing False Nothing False
              , Member "srcOffset1" "VkOffset3D" Nothing False Nothing False
              , Member "dstSubresource" "VkImageSubresourceLayers" Nothing False Nothing False
              , Member "dstOffset0" "VkOffset3D" Nothing False Nothing False
              , Member "dstOffset1" "VkOffset3D" Nothing False Nothing False ] }
      x@(Struct {..}) -> x { typeMembers = map mappingMember typeMembers }
      x@(Union {..}) -> x { typeMembers = map mappingMember typeMembers }
      x@(Basetype {..}) -> x { typeType = haskType typeType }
      x@(Handle { typeType = "VK_DEFINE_HANDLE", ..}) ->
        x { typeType = "Ptr ()" }
      x@(Handle { typeType = "VK_DEFINE_NON_DISPATCHABLE_HANDLE", ..}) ->
        x { typeType = "Int64" }
      x@(Funcpointer {..}) ->
        x { typeArgs = map haskType' typeArgs ++ [funcpointerReturns typeName] }
      rest -> rest
  , registryEnums = registryEnums
  , registryCommands = flip map registryCommands $ \x@(Command {..}) ->
      x { commandReturn = haskType commandReturn
        , commandParameters = map mappingParameter commandParameters
        }
  , registryFeatures = registryFeatures
  , registryExtensions = registryExtensions
  }

mappingMember :: Member -> Member
mappingMember m@(Member { memberEnum = Just enum, .. }) =
  m { memberType = "Ptr " ++ haskType' memberType }
mappingMember m@(Member { memberType = "char", .. }) =
  m { memberType = case length . elemIndices ',' <$> memberLen of
                     Just 0 -> "CString"
                     Just 1 -> "Ptr CString"
    }
mappingMember m@(Member { memberType = "void", .. }) =
  m { memberType = "Ptr ()" } -- for VkSpecializationInfo_pData
mappingMember m@(Member {..}) =
  m { memberType = case length . elemIndices ',' <$> memberLen of
                     Nothing -> haskType' memberType
                     Just 0 -> "Ptr " ++ haskType' memberType
    }

mappingParameter :: Parameter -> Parameter
-- avoid haskell keywords
mappingParameter p@(Parameter { parameterName = "instance", .. }) =
  mappingParameter p { parameterName = "vulkan" }
mappingParameter p@(Parameter { parameterName = "type", .. }) =
  mappingParameter p { parameterName = "imageType" }
mappingParameter p@(Parameter { parameterName = "data", .. }) =
  mappingParameter p { parameterName = "word" }
-- fix type
mappingParameter p@(Parameter { parameterType = "char", .. }) =
  p { parameterType = case length . elemIndices ',' <$> parameterLen of
                        Nothing -> "CString" -- fix accident in xml
                        Just 0 -> "CString"
                        Just 1 -> "Ptr CString"
    }
mappingParameter p@(Parameter {..}) =
  p { parameterType = haskType' parameterType }

funcpointerReturns :: String -> String
funcpointerReturns "PFN_vkInternalAllocationNotification" = "IO ()"
funcpointerReturns "PFN_vkInternalFreeNotification" = "IO ()"
funcpointerReturns "PFN_vkReallocationFunction" = "IO (Ptr ())"
funcpointerReturns "PFN_vkAllocationFunction" = "IO (Ptr ())"
funcpointerReturns "PFN_vkFreeFunction" = "IO ()"
funcpointerReturns "PFN_vkVoidFunction" = "FunPtr ()"
funcpointerReturns "PFN_vkDebugReportCallbackEXT" = "IO VkBool32"

