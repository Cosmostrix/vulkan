{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Arrow
import Control.Monad (forM_)
import Data.Text.Lazy (Text, pack, unpack, intercalate)
import qualified Data.Text.Lazy.IO as L
import System.Environment
import Text.Shakespeare.Text
import Data.Monoid
import Data.Char (toLower, toUpper)
import Data.Bits
import Foreign
import Foreign.C.Types
import Data.Text.Internal.Builder
import System.IO (openFile, IOMode(WriteMode))

import VkRegistry
import VkParser
import Convertor

main :: IO ()
main = do
  args <- getArgs
  case args of
    [vk_xml, destdir] -> main' vk_xml destdir
    _ -> putStrLn "Usage: vulkan-apigen vk.xml autogen"

main' :: String -> String -> IO ()
main' src destdir = do
  registry@(Registry {..}) <- mapToHask <$> parseVkXml src
  let gd = gatherCommands registry
  let sd = mkSizeDict registry
  yield <- case destdir of
    "" -> return L.putStrLn
    "-" -> return L.putStrLn
    dir -> openFile (dir ++ "/Bindings.hs") WriteMode >>= return . L.hPutStrLn
  yield [lt|{-# LANGUAGE CPP #-}
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
module Graphics.Vulkan.Bindings (
  -- Vk1
    VkSampleMask
  , VkBool32
  , VkFlags
  , VkDeviceSize|]
  forM_ registryFeatures $ \(Feature {..}) -> do
    yield [lt|-- * #{featureName}|]
    forM_ featureRequires $ \(Require {..}) -> do
      let types = concatMap ("\n  , " ++) requireTypes
      -- let deps = concatMap ("\n  , " ++) 
      let enums = concatMap ("\n  , pattern " ++) requireEnums
      let commands = concatMap ("\n  , " ++) requireCommands
      yield [lt|-- ** #{requireComment}#{types}#{enums}#{commands}
|]
  yield "-- * Extensions"
  forM_ registryExtensions $ \(Extension {..}) -> do
    yield [lt|-- ** #{extensionName}
-- Author: #{show extensionAuthor}
-- Contact: #{show extensionContact}
-- Support: #{extensionSupport}
-- Protect: #{show extensionProtect}|]
    forM_ extensionRequires $ \(RequireExt {..}) -> do
      let types = concatMap ("\n  , " ++) requireExtTypes
      let enums = concatMap (("\n  , pattern " ++) . enumEName) requireExtEnums
      let commands = concatMap ("\n  , " ++) requireExtCommands
      yield [lt|-- #{types}#{enums}#{commands}
|]
  yield [lt|) where
import Foreign
import Foreign.C
import Data.Bits
import Data.Int
import Data.Word
import Linear
import Numeric.Half
import Numeric.Fixed
import Control.Monad.IO.Class

type Invoker a = FunPtr a -> a
foreign import ccall unsafe "&vkGetInstanceProcAddr" fp_vkGetInstanceProcAddr :: FunPtr (VkInstance -> Ptr CChar -> IO PFN_vkVoidFunction)

pattern VK_HEADER_VERSION = 6
--    <type name="VK_API_VERSION"/>
--    <type name="VK_API_VERSION_1_0"/>
--    <type name="VK_VERSION_MAJOR"/>
--    <type name="VK_VERSION_MINOR"/>
--    <type name="VK_VERSION_PATCH"/>
--    <type name="VK_HEADER_VERSION"/>
--    <enum name="VK_LOD_CLAMP_NONE"/>
--    <enum name="VK_REMAINING_MIP_LEVELS"/>
--    <enum name="VK_REMAINING_ARRAY_LAYERS"/>
--    <enum name="VK_WHOLE_SIZE"/>
--    <enum name="VK_ATTACHMENT_UNUSED"/>
--    <enum name="VK_TRUE"/>
--    <enum name="VK_FALSE"/>
--    <type name="VK_NULL_HANDLE"/>
--    <enum name="VK_QUEUE_FAMILY_IGNORED"/>
--    <enum name="VK_SUBPASS_EXTERNAL"/>
--    <type name="VkPipelineCacheHeaderVersion"/>

-- X11/Xlib.h
type Display = Ptr ()
type VisualID = Ptr ()
type Window = Ptr ()

-- android/native_window.h
type ANativeWindow = Ptr ()

-- mir_toolkit/client_types.h
type MirConnection = Ptr ()
type MirSurface = Ptr ()

-- wayland-client.h
type WlDisplay = Ptr () -- ^ wl_display
type WlSurface = Ptr () -- ^ wl_surface

-- windows.h
type HINSTANCE = Ptr ()
type HWND = Ptr ()

-- xcb/xcb.h
type XcbConnection = Ptr () -- ^ xcb_connection_t
type XcbVisualId = Ptr () -- ^ xcb_visualid_t
type XcbWindow = Ptr () -- ^ xcb_window_t
|]
  let caps = makeCapability registry
  forM_ caps $ \cap -> do
    yield (renderCapabilityData cap)
    yield (renderCapabilityTest cap)
  --forM_ registryFeatures $ \feature ->
  --  yield $ renderFeatureData registryCommands feature
  --forM_ registryExtensions $ \extension ->
  --  yield $ renderExtensionData registryCommands extension
  forM_ registryTypes $ \case
    struct@(Struct {..}) -> do
      yield (renderData struct)
      yield (renderStorable sd struct)
    union@(Union {..}) -> do
      putStrLn (show union)
      yield (renderData union)
      yield (renderStorable sd union)
    Basetype {..} ->
      yield [lt|type #{typeName} = #{typeType}|]
    Bitmask {..} ->
      yield [lt|-- | Requires: #{typeRequires}
type #{typeName} = #{typeType}|]
    Handle {..} ->
      yield [lt|-- | Parent: #{show typeParent}
type #{typeName} = #{typeType}|]
    Funcpointer {..} ->
      yield [lt|type #{typeName} = #{intercalate " -> " (map showType typeArgs)}|]
    _ -> return ()
  forM_ registryEnums $ \(Enumeratees {..}) -> do
    let derivings = case enumsType of
                      "enum" -> "(Eq, Ord, Show, Storable)" :: Text
                      "bitmask" -> "(Eq, Ord, Bits, FiniteBits, Show, Storable)" -- allow binary ops
    yield [lt|-- | #{enumsComment}
newtype #{enumsName} = #{enumsName} Int deriving #{derivings}

#{mconcat $ map (showEnum enumsName) enumsList}
|]
  forM_ registryCommands $ \(Command {..}) -> do
    let arguments = intercalate " " $ map (pack.memberName) commandParameters
    let types = intercalate " -> " $
          [ [lt|#{memberType m}|] | m <- commandParameters ] ++ [[lt|IO #{commandReturn}|]]
    let commentedTypes = intercalate "\n  -> " $
          map showParameter commandParameters ++ [[lt|m #{commandReturn}|]]
    yield [lt|
-- | @#{commandName} #{arguments}@
-- #{showUsage commandValidity}
-- 
-- s:#{commandSuccessCodes} e:#{commandErrorCodes} q:#{commandQueues} rp:#{commandRenderpass} cbl:#{commandCmdBufferLevel} iesp:#{show commandImplicitExternSyncParams}
#{commandName}
  :: MonadIO m => Vk1
  -> #{commentedTypes}
#{commandName} vk #{arguments} =
  liftIO (ffi_#{commandName} (fp_#{commandName} vk) #{arguments})

foreign import ccall unsafe "dynamic" ffi_#{commandName} :: FunPtr (#{types}) -> (#{types})
|]

showUsage :: [String] -> String
showUsage xs = mconcat [ "\n-- * " ++ x | x <- xs ]

braced :: [Text] -> [Text]
braced [] = []
braced [x] = ["", "  { " <> x <> " }"]
braced (x : xs)  = ["", "  { " <> x] ++ ["  , " <> y | y <- xs] ++ ["  }"]

lowerCamel :: String -> String
lowerCamel [] = []
lowerCamel (x:xs) = toLower x : xs

upperCamel :: String -> String
upperCamel [] = []
upperCamel (x:xs) = toUpper x : xs

showMember :: String -> Member -> Text
showMember typeName m@(Member {..}) =
  [lt|#{lowerCamel typeName}_#{memberName} :: #{memberType}#{comment m}|]

renderData :: Type -> Text
renderData (Struct {..}) = [lt|
-- |
-- #{showUsage typeValidity}
data #{typeName} = #{typeName}#{render typeMembers} --deriving (Eq, Show)
|] where render = intercalate "\n" . braced . map (showMember typeName)
renderData (Union {..}) =
  [lt|data #{typeName} = #{constructors} --deriving (Eq, Show)|]
  where constructors = intercalate " | " $ map showCon typeMembers
        showCon (Member {..}) =
          [lt|#{typeName}#{upperCamel memberName} #{memberType}|]

structLayout :: SizeDict -> [Member] -> [(String, (Int, Int))]
structLayout sd members = uncurry zip $ (map memberName &&& layout) members
  where offsets (x : xs) = x : map (addp x) (offsets xs)
        layout :: [Member] -> [(Int, Int)]
        layout xs = offsets $ (0, 0) : map (sizeOfType sd . memberType) xs

renderStorable :: SizeDict -> Type -> Text
renderStorable sd (Struct {..}) =
  let from = structLayout sd &&& concatMap ((' ':) . memberName)
      (layout, args) = from typeMembers
      impl f = (intercalate "\n    " <<< map f) layout
      peek (name, pos) = [lt|#{name} <- peek (plusPtr ptr (#{showSize pos}))|]
      poke (name, pos) = [lt|poke (plusPtr ptr (#{showSize pos})) #{name}|]
  in [lt|-- ReturnedOnly = #{show typeReturnedOnly}
instance Storable #{typeName} where
  alignment = alignment (undefined :: CSize)
  sizeOf _ = #{showSize $ sizeOfType sd [typeName]}
  peek ptr = do
    #{impl peek}
    return $ #{typeName}#{args}
  poke ptr (#{typeName}#{args}) = do
    #{impl poke}
|]
renderStorable sd (Union {..}) = [lt|-- ReturnedOnly = False
instance Storable #{typeName} where
  alignment = alignment (undefined :: CSize)
  sizeOf _ = #{showSize $ sizeOfType sd [typeName]}
  peek = error "cannot peek C union"
  #{impl' poke'}
|] where impl' f = (intercalate "\n  " <<< map f) typeMembers
         poke' (Member {..}) =
           [lt|poke ptr (#{typeName}#{upperCamel memberName} a) = poke ptr a|]

showParameter :: Member -> Text
showParameter m@(Member {..}) = [lt|#{memberType}#{comment m}|]

comment :: Member -> Text
comment (Member {..}) = if body /= "" then "\n  -- ^ " <> body else "" 
  where
  body = intercalate " " $ filter (/= "") [opt, sync, cons, enum, len, nav]
  opt = if memberOptional == "" then "" else [lt|Optional=#{memberOptional}.|]
  sync = if parameterExternsync == "" then "" else [lt|ExternSync=#{parameterExternsync}.|]
  cons = if memberConst then "Const." else ""
  enum = maybe "" (\enum -> [lt|Max: #{enum}.|]) memberEnum
  len = maybe "" (\len -> [lt|Length: #{len}.|]) memberLen
  nav = if memberNoautovalidity then "Noautovalidity." else ""

showEnum :: String -> Enumeratee -> Text
showEnum enumsName (EnumValue {..}) =
  [lt|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{value}
|] where value = if head enumValue == '-' then "(" ++ enumValue ++ ")" else enumValue
showEnum enumsName (EnumBitpos {..}) =
  [lt|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{value}
|] where value = bit enumBitpos :: Int

type SizeDict = [(String, (Int, Int))] -- (name, (bytes, csize))

initSizeDict :: SizeDict
initSizeDict =
  [ ("Float", (4,0)), ("Word8", (1,0)), ("Word32", (4,0)), ("Word64", (8,0))
  , ("Int32", (4,0)), ("Int64", (8,0)), ("CSize", (0,1)), ("CString", (0,1))
  , ("Ptr ()", (0,1))
  ]

showSize :: (Int, Int) -> Text
showSize (bytes, pointers) =
  [lt|#{bytes} + sizeOf (undefined :: CSize) * #{pointers}|]

sizeOfType :: SizeDict -> [String] -> (Int, Int)
sizeOfType dict [name] = maybe (-100000, -100000) id $ lookup name dict
sizeOfType _ ("Ptr" : _) = (0, 1)
sizeOfType d ("V2" : name) = mulp (2, 2) $ sizeOfType d name
sizeOfType d ("V3" : name) = mulp (3, 3) $ sizeOfType d name
sizeOfType d ("V4" : name) = mulp (4, 4) $ sizeOfType d name

addp, mulp, maxp :: (Num a, Ord a, Show a) => (a, a) -> (a, a) -> (a, a)
addp (x, y) (z, w) = (x + z, y + w)
mulp (x, y) (z, w) = (x * z, y * w)
maxp (x, 0) (z, 0) = (max x z, 0)

mkSizeDict :: Registry -> [(String, (Int, Int))]
mkSizeDict (Registry {..}) =
  foldl go (foldl go initSizeDict registryTypes) registryTypes
  where
    go dict = \case
      Struct {..} -> (typeName, sum' $ map (sizeOfMember dict) typeMembers) : dict
      Union {..} -> (typeName, max' $ map (sizeOfMember dict) typeMembers) : dict
      RequireType { typeRequires = "vk_platform", ..} -> dict
      RequireType {..} -> (typeName, (0, 1)) : dict
      Basetype {..} -> (typeName, sizeOfType dict [typeType]) : dict
      Bitmask {..} -> (typeName, sizeOfType dict [typeType]) : dict
      Handle {..} -> (typeName, sizeOfType dict [typeType]) : dict
      EnumType {..} -> (typeName, (0, 1)) : dict
      Funcpointer {..} -> (typeName, (0, 1)) : dict
    sum' = foldl addp (0, 0)
    max' = foldl maxp (0, 0)
    sizeOfMember dict m@(Member {..}) =
      times (len memberEnum) $ sizeOfType dict memberType
    times n (x, y) = (n * x, n * y)
    len = \case
        Nothing -> 1
        -- API constants.
        Just "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE" -> 256
        Just "VK_UUID_SIZE" -> 16
        Just "VK_MAX_EXTENSION_NAME_SIZE" -> 256
        Just "VK_MAX_DESCRIPTION_SIZE" -> 256
        Just "VK_MAX_MEMORY_TYPES" -> 32
        Just "VK_MAX_MEMORY_HEAPS" -> 16
        Just x -> error x

instance ToText [String] where
  toText = fromLazyText . showType

showType :: [String] -> Text
showType [x] = pack x
showType [p, a] = [lt|#{p} #{a}|]
showType [p, q, a] = [lt|#{p} (#{q} #{a})|]

snakeToCamel :: String -> String
snakeToCamel = id

renderFeatureData :: [Command] -> Feature -> Text
renderFeatureData commands (Feature {..}) =
  [lt|data #{snakeToCamel featureName} = #{snakeToCamel featureName}
  { #{intercalate "\n  , " fields}
  } deriving (Eq, Show)
|] where
  fields = [ showFpField command
           | name <- concatMap requireCommands featureRequires
           , command@(Command {..}) <- commands
           , name == commandName ]

renderExtensionData :: [Command] -> Extension -> Text
renderExtensionData commands (Extension {..}) =
  [lt|data #{snakeToCamel extensionName} = #{snakeToCamel extensionName}
  { #{intercalate "\n  , " fields}
  } deriving (Eq, Show)
|] where
  fields = [ showFpField command
           | name <- concatMap requireExtCommands extensionRequires
           , command@(Command {..}) <- commands
           , name == commandName ]

gatherCommands :: Registry -> [(String, String)]
gatherCommands (Registry {..}) = feat ++ exts
  where feat = [ (command, featureName)
               | Feature {..} <- registryFeatures
               , Require {..} <- featureRequires
               , command <- requireCommands ]
        exts = [ (command, extensionName)
               | Extension {..} <- registryExtensions
               , RequireExt {..} <- extensionRequires
               , command <- requireExtCommands ]
--vkGetInstanceProcAddr x
-- vkEnumerateInstanceExtensionProperties NULL
-- vkEnumerateInstanceLayerProperties NULL
-- vkCreateInstance NULL
--vkGetDeviceProcAddr
-- VkDevice
-- VkQueue
-- VkCommandBuffer

--data Vulkan10Setup = {}
--data Vulkan10 = {}

data Capability = Capability
  { capabilityName :: String
  , capabilityRequirements :: [(String, String)]
  , capabilityTester :: String
  , capabilityPrelude :: String
  , capabilityGetter :: String
  , capabilityCommands :: [Command]
  } deriving Show

findCapability :: [Capability] -> String -> String
findCapability caps func = head
  [ capabilityName
  | Capability {..} <- caps
  , Command {..} <- capabilityCommands
  , commandName == func ]

makeCapability :: Registry -> [Capability]
makeCapability (Registry {..}) =
  concatMap fromFeature registryFeatures -- ++ map fromExtension registryExtensions
  where
    fromFeature (Feature {..}) =
      let commands = [ command
           | name <- concatMap requireCommands featureRequires
           , command@(Command {..}) <- registryCommands
           , name `notElem` preInstanceCommands
           , name == commandName ]
      in
      [ Capability "VulkanSetup"
        [("vulkan", "VkInstance")]
        "return (fp_vkGetInstanceProcAddr /= nullFunPtr)"
        "let vkGetInstanceProcAddr = ffi_vkGetInstanceProcAddr fp_vkGetInstanceProcAddr"
        "vkGetInstanceProcAddr vulkan"
        (filter (not . isDispatchableCommand) commands)
      , Capability "Vulkan10"
        [("vks", "VulkanSetup"), ("device", "VkDevice")]
        "return (fp_vkGetDeviceProcAddr vks /= nullFunPtr)"
        "let vkGetDeviceProcAddr = ffi_vkGetDeviceProcAddr (fp_vkGetDeviceProcAddr vks)"
        "vkGetDeviceProcAddr device"
        (filter isDispatchableCommand commands)
      ]
    fromExtension (Extension {..}) =
      [ ]

preInstanceCommands :: [String]
preInstanceCommands = ["vkGetInstanceProcAddr", "vkEnumerateInstanceExtensionProperties", "vkEnumerateInstanceLayerProperties", "vkCreateInstance"]

isDispatchableCommand :: Command -> Bool
isDispatchableCommand c | commandName c == "vkGetDeviceProcAddr" = False
isDispatchableCommand c = (`elem` ["VkDevice", "VkQueue", "VkCommandBuffer"])
   . head . memberType . head . commandParameters $ c

renderCapabilityData :: Capability -> Text
renderCapabilityData (Capability {..}) =
  [lt|data #{capabilityName} = #{capabilityName}
  { #{intercalate "\n  , " (map showFpField capabilityCommands)}
  } deriving (Eq, Show)
|]

showFpField :: Command -> Text
showFpField (Command {..}) =
  [lt|fp_#{commandName} :: FunPtr (#{commandType})|]
  where commandType = intercalate " -> " $
          map (\x -> [lt|#{memberType x}|]) commandParameters
          ++ [[lt|#{"IO" : commandReturn}|]]

renderCapabilityTest :: Capability -> Text
renderCapabilityTest (Capability {..}) =
  [lt|get#{capabilityName} :: MonadIO m => #{reqTypes}m (Maybe #{capabilityName})
get#{capabilityName} #{reqArgs}= liftIO $ do
  result <- #{capabilityTester}
  #{capabilityPrelude}
  if result then return . Just . #{capabilityName}
    #{intercalate "\n    " capabilityGetters}
  else return Nothing
|] where reqTypes = concatMap (++ " -> ") (map snd capabilityRequirements)
         reqArgs = concatMap (++ " ") (map fst capabilityRequirements)
         capabilityGetters = [ [lt|<*> #{capabilityGetter} "#{commandName}"|]
                             | Command {..} <- capabilityCommands ]


