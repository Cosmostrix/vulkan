{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad (forM_)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.IO as L
import System.Environment
import Text.Shakespeare.Text
import Data.List
import Data.Monoid
import Data.String
import Data.Char (toLower, toUpper)
import Data.Bits

import VkRegistry
import VkParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [vk_xml, destdir] -> main' vk_xml destdir
    _ -> putStrLn "Usage: vulkan-apigen vk.xml autogen"

main' :: String -> String -> IO ()
main' src destdir = do
  Registry {..} <- mapToHask <$> parseVkXml src
  L.putStrLn [lt|{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- # LANGUAGE Strict #-}
{- # LANGUAGE StrictData #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Cosmostrix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Cosmostrix <cosmos@lunakit.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Complete Vulkan raw API bindings.
----------------------------------------------------------------------------
module Graphics.Vulkan.Bindings where
import Foreign
import Foreign.C
import Data.Bits
import Data.Int
import Data.Word
import Numeric.Half
import Numeric.Fixed
import Control.Monad.IO.Class

type VkDataPtr = Ptr ()

-- X11/Xlib.h
data Display
data VisualID
data Window

-- android/native_window.h
data ANativeWindow

-- mir_toolkit/client_types.h
data MirConnection
data MirSurface

-- wayland-client.h
data WlDisplay -- ^ wl_display
data WlSurface -- ^ wl_surface

-- windows.h
data HINSTANCE
data HWND

-- xcb/xcb.h
data XcbConnection -- ^ xcb_connection_t
data XcbVisualId -- ^ xcb_visualid_t
data XcbWindow -- ^ xcb_window_t
|]
  forM_ registryTypes $ \case
    Struct {..} -> L.putStrLn [lt|
-- |
-- #{showUsage typeValidity}
data #{typeName} = #{typeName}#{members} --deriving (Eq, Show)
  -- ReturnedOnly? = #{show typeReturnedOnly}

instance Storable #{typeName} where
  alignment _ = 0
  sizeOf _ = 0
  peek _ = return undefined
  poke _ _ = return ()
|] where members = intercalate "\n" . braced $
                     map (showMember typeName) typeMembers
    Union {..} -> do
      let showCon (Member {..}) = unpack
            [st|#{typeName}#{upperCamel memberName} #{memberType}|]
      let constructors = intercalate " | " $ map showCon typeMembers
      L.putStrLn [lt|data #{typeName} = #{constructors} --deriving (Eq, Show)|]
    Basetype {..} ->
      L.putStrLn [lt|type #{typeName} = #{typeType}|]
    Bitmask {..} ->
      L.putStrLn [lt|-- | Requires: #{typeRequires}
type #{typeName} = #{typeType}|]
    Handle {..} ->
      L.putStrLn [lt|-- | Parent: #{show typeParent}
type #{typeName} = #{typeType}|]
    Funcpointer {..} ->
      L.putStrLn [lt|type #{typeName} = #{intercalate " -> " typeArgs}|]
    _ -> return ()
  forM_ registryEnums $ \(Enumeratees {..}) -> do
    let derivings = case enumsType of
                      "enum" -> "(Eq, Ord, Show)" :: Text
                      "bitmask" -> "(Eq, Ord, Bits, FiniteBits, Show)" -- allow binary ops
    L.putStrLn [lt|-- | #{enumsComment}
newtype #{enumsName} = #{enumsName} Int deriving #{derivings}

#{mconcat $ map (showEnum enumsName) enumsList}
|]
  forM_ registryCommands $ \(Command {..}) ->
    let arguments = intercalate " " $ map parameterName commandParameters in
    let commentedTypes = [ (parameterType, unpack [st|#{parameterName}. Optional:#{parameterOptional} ExternSync:#{parameterExternsync} Len:#{show parameterLen} Noautovalidity:#{show parameterNoautovalidity}|]) | Parameter {..} <- commandParameters ] ++ [("m " ++ commandReturn, "")] in
    L.putStrLn [lt|
-- | @#{commandName} #{arguments}@
-- #{showUsage commandValidity}
-- 
-- s:#{commandSuccessCodes} e:#{commandErrorCodes} q:#{commandQueues} rp:#{commandRenderpass} cbl:#{commandCmdBufferLevel} iesp:#{show commandImplicitExternSyncParams}
#{commandName} :: MonadIO m
  => #{showCommentedTypes commentedTypes}
#{commandName} #{arguments} =
  liftIO undefined
|]

showUsage :: [String] -> String
showUsage xs = mconcat [ "\n-- * " ++ x | x <- xs ]

braced :: (Monoid a, IsString a) => [a] -> [a]
braced [] = []
braced [x] = ["", "  { " <> x <> " }"]
braced (x : xs)  = ["", "  { " <> x] ++ ["  , " <> y | y <- xs] ++ ["  }"]

lowerCamel :: String -> String
lowerCamel [] = []
lowerCamel (x:xs) = toLower x : xs

upperCamel :: String -> String
upperCamel [] = []
upperCamel (x:xs) = toUpper x : xs

showMember :: String -> Member -> String
showMember typeName (Member {..}) =
  unpack [st|#{lowerCamel typeName}_#{memberName} :: #{memberType}#{comment}|]
  where comment = if opt <> enum <> len <> nav /= ""
                  then [st|
  -- ^#{opt}#{enum}#{len}#{nav}|]
                  else ""
        opt = if memberOptional then " Optional." else ""
        enum = maybe "" (\enum -> [st| Max: #{enum}.|]) memberEnum
        len = maybe "" (\len -> [st| Length: #{len}.|]) memberLen
        nav = if memberNoautovalidity then " Noautovalidity." else ""

showEnum :: String -> Enumeratee -> Text
showEnum enumsName (EnumValue {..}) =
  [st|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{value}
|] where value = if head enumValue == '-' then "(" ++ enumValue ++ ")" else enumValue
showEnum enumsName (EnumBitpos {..}) =
  [st|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{value}
|] where value = bit enumBitpos :: Int

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

showCommentedTypes :: [(String, String)] -> String
showCommentedTypes [(ret, comment)] =
  ret ++ if comment /= "" then " -- ^ " ++ comment else ""
showCommentedTypes xs = intercalate "\n  -> " $ map (showCommentedTypes.(:[])) xs

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

