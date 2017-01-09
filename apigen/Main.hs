{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}
import Control.Arrow
import Control.Monad (forM_)
import Data.Text.Lazy (Text, pack, unpack, intercalate, splitOn)
import qualified Data.Text.Lazy.IO as L
import System.Environment
import Text.Shakespeare.Text
import Data.Monoid
import Data.Char (toLower, toUpper, isUpper)
import Data.Bits
import Data.List (nub)
import Foreign
import Foreign.C.Types
import Data.Text.Internal.Builder
import System.IO (openFile, IOMode(WriteMode), hClose)

import VkRegistry
import VkParser
import Convertor
import VkManual

main :: IO ()
main = do
  args <- getArgs
  case args of
    vk_xml : destdir : man -> main' vk_xml destdir man
    _ -> putStrLn "Usage: vulkan-apigen vk.xml autogen man/*"

main' :: String -> String -> [String] -> IO ()
main' src destdir manfiles = do
  registry@(Registry {..}) <- mapToHask <$> parseVkXml src
  let sd = mkSizeDict registry
  (yield, close) <- case destdir of
    "" -> return (L.putStrLn, return ())
    "-" -> return (L.putStrLn, return ())
    dir -> do
      h <- openFile (dir ++ "/Bindings.hs") WriteMode
      return (L.hPutStrLn h, hClose h)
  Right man <- readVkMan manfiles
  yield [lt|{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE DuplicateRecordFields #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Cosmostrix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Cosmostrix <cosmos@lunakit.org>
-- Stability   :  experimental
-- Portability :  DataKinds, PatternSynonyms, GeneralizedNewtypeDeriving
-- 
-- <<Vulkan_170px_Mar15.png>>
-- 
-- Complete <https://www.khronos.org/vulkan/ Vulkan> raw API bindings.
-- 
-- * <https://www.khronos.org/assets/uploads/developers/library/overview/vulkan-overview.pdf Overview presentation>
-- * <https://www.khronos.org/registry/vulkan Vulkan Registry>
-- * <https://www.khronos.org/registry/vulkan/specs/1.0/refguide/Vulkan-1.0-web.pdf Online Quick Reference>
-- * <https://github.com/KhronosGroup/Khronosdotorg/blob/master/api/vulkan/resources.md Vulkan Resources on Github>
-- * <https://www.khronos.org/registry/vulkan/specs/1.0-wsi_extensions/pdf/vkspec.pdf Vulkan 1.0 Core API + WSI Extensions (PDF)>
-- 
-- Vulkan and the Vulkan logo are trademarks of the Khronos Group Inc.
----------------------------------------------------------------------------
module Graphics.Vulkan.Bindings (
  -- * Initialization
    castToFixedString
  , getVulkanSetup
  , getVulkan
  , VulkanSetup(..)
  , Vulkan(..)
  -- * Misc
  , pattern VK_HEADER_VERSION
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_WHOLE_SIZE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_TRUE
  , pattern VK_FALSE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_SUBPASS_EXTERNAL
  , VkFlags
  --, Display
  --, VisualID
  --, Window
  --, ANativeWindow
  --, MirConnection
  --, MirSurface
  --, WlDisplay
  --, WlSurface
  --, HINSTANCE
  --, HWND
  --, XcbConnection
  --, XcbVisualId
  --, XcbWindow|]
  let listF = flip concatMap registryFeatures $ \(Feature {..}) ->
        [lt|-- * #{featureName}|] :
        (flip concatMap featureRequires $ \(Require {..}) ->
        let types = map (", " <>) (renderExports registry requireTypes)
            enums = map ((", pattern " <>) . pack) requireEnums
            commands = map (", " <>) $
                         renderExportCommands registry requireCommands
        in [lt|-- ** #{requireComment}|] : (types ++ enums ++ commands) )
  let listE = "-- * Extensions" : (flip concatMap registryExtensions $ \(Extension {..}) ->
        [lt|
  -- ** #{extensionName}
  -- Author: #{show extensionAuthor}
  -- Contact: #{show extensionContact}
  -- Support: #{extensionSupport}
  -- Protect: #{show extensionProtect}
|] :
        (flip concatMap extensionRequires $ \(RequireExt {..}) ->
          let types = map (", " <>) (renderExports registry requireExtTypes)
              enums = map ((", pattern " <>) . pack . enumEName) requireExtEnums
              commands = map (", " <>) $
                           renderExportCommands registry requireExtCommands
          in types ++ enums ++ commands))
  yield [lt|  #{intercalate "\n  " (nub $ listF ++ listE)}|]
  yield [lt|) where
import Foreign
import Foreign.C
import Data.Bits
import Data.Int
import Data.Word
import Data.Maybe (fromJust)
import Data.Vector (fromListN)
import Linear
import Linear.V
import Numeric.Half
import Numeric.Fixed
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafePerformIO)

-- | This function is only safe on the first 256 characters.
castToFixedString :: Dim n => String -> V n CChar
castToFixedString x = result
  where d = dim result
        result = fromJust . fromVector . fromListN d $
                   map castCharToCChar (x ++ replicate (d - length x) '\0')

foreign import ccall unsafe "&vkGetInstanceProcAddr" fp_vkGetInstanceProcAddr :: FunPtr (VkInstance -> Ptr CChar -> IO (FunPtr a))

fp_vkCreateInstance :: FunPtr (Ptr VkInstanceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult)
fp_vkCreateInstance = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkCreateInstance")

fp_vkEnumerateInstanceLayerProperties :: FunPtr (Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult)
fp_vkEnumerateInstanceLayerProperties = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkEnumerateInstanceLayerProperties")

fp_vkEnumerateInstanceExtensionProperties :: FunPtr (Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult)
fp_vkEnumerateInstanceExtensionProperties = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkEnumerateInstanceExtensionProperties")

getInstanceProcAddr :: VkInstance -> String -> IO (FunPtr a)
getInstanceProcAddr vulkan proc =
        return . castFunPtr =<< withCAString proc (vkGetInstanceProcAddr vulkan)

pattern VK_HEADER_VERSION = 12
pattern VK_LOD_CLAMP_NONE = 1000 :: Float
pattern VK_REMAINING_MIP_LEVELS = 0xffffffff :: Word32
pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff :: Word32
pattern VK_WHOLE_SIZE = 0xffffffffffffffff :: Word64
pattern VK_ATTACHMENT_UNUSED = 0xffffffff :: Word32
pattern VK_TRUE = 1 :: VkBool32
pattern VK_FALSE = 0 :: VkBool32
pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff :: Word32
pattern VK_SUBPASS_EXTERNAL = 0xffffffff :: Word32

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
  let postInitCommands =
        filter ((`notElem` preInitCommands) . commandName) registryCommands
  let instanceCommands = filter (not . isDispatchableCommand) postInitCommands
  let deviceCommands = filter isDispatchableCommand postInitCommands
  yield [lt|
data VulkanSetup = VulkanSetup
  { #{intercalate "\n  , " (map showFpField instanceCommands)}
  }

getVulkanSetup :: MonadIO m => VkInstance -> m VulkanSetup
getVulkanSetup vulkan = liftIO $ return VulkanSetup
  #{intercalate "\n    " (renderApp "getInstanceProcAddr vulkan" instanceCommands)}

data Vulkan = Vulkan
  { #{intercalate "\n  , " (map showFpField deviceCommands)}
  }

getVulkan :: MonadIO m => VulkanSetup -> VkDevice -> m Vulkan
getVulkan vks device = liftIO $ do
  let getDeviceProcAddr proc =
        return . castFunPtr =<< withCAString proc (vkGetDeviceProcAddr vks device)
  return Vulkan
    #{intercalate "\n    " (renderApp "getDeviceProcAddr" deviceCommands)}
|]

  forM_ registryTypes $ \case
    struct@(Struct {..}) -> do
      yield (renderData (lookup man typeName) struct)
      yield (renderStorable sd struct)
    union@(Union {..}) -> do
      yield (renderData (lookup man typeName) union)
      yield (renderStorable sd union)
    Basetype {..} ->
      yield [lt|type #{typeName} = #{typeType}|]
    Bitmask {..} ->
      yield [lt|-- | Requires: #{typeRequires}
type #{typeName} = #{typeType}|]
    Handle {..} ->
      yield [lt|-- | Parent: #{show typeParent}
type #{typeName} = #{typeType}|]
    Funcpointer {..} -> do
      let sig = intercalate " -> " (map showType typeArgs)
      yield [lt|type #{typeName} = FunPtr (#{sig})|]
      yield [lt|foreign import ccall unsafe "wrapper" ffi_#{typeName} :: (#{sig}) -> IO (FunPtr (#{sig}))|]
    _ -> return ()
  forM_ registryEnums $ \(Enumeratees {..}) -> do
    let derivings = case enumsType of
                      "enum" -> "(Eq, Ord, Show, Storable)" :: Text
                      "bitmask" -> "(Eq, Ord, Bits, FiniteBits, Show, Storable)" -- allow binary ops
    yield [lt|-- | #{enumsComment}
newtype #{enumsName} = #{enumsName} Int deriving #{derivings}

#{mconcat $ map (showEnum enumsName) enumsList}
|]
  forM_ registryCommands $ \command@(Command {..}) -> do
    let isPreInitCommand = commandName `elem` preInitCommands
    let arguments = intercalate " " $ map (pack.memberName) commandParameters
    let types = intercalate " -> " $
          [ [lt|#{memberType m}|] | m <- commandParameters ] ++ [[lt|IO #{commandReturn}|]]
    let fpdata = if isDispatchableCommand command
                 then "Vulkan" else "VulkanSetup" :: String
    let commentedTypes = intercalate "\n  -> " $
          map showParameter commandParameters ++ [[lt|m #{commandReturn}|]]
    yield [lt|
foreign import ccall unsafe "dynamic" ffi_#{commandName} :: FunPtr (#{types}) -> (#{types})

-- | @#{commandName} #{arguments}@
-- #{showValidaty commandValidity}
-- 
-- s:#{commandSuccessCodes} e:#{commandErrorCodes} q:#{commandQueues} rp:#{commandRenderpass} cbl:#{commandCmdBufferLevel} iesp:#{show commandImplicitExternSyncParams}
#{commandName}|]
    if isPreInitCommand
    then yield [lt|  :: MonadIO m
  => #{commentedTypes}
#{commandName} #{arguments} =
  liftIO (ffi_#{commandName} fp_#{commandName} #{arguments})
|]  else yield [lt|  :: MonadIO m => #{fpdata}
  -> #{commentedTypes}
#{commandName} vk #{arguments} =
  liftIO (ffi_#{commandName} (fp_#{commandName} vk) #{arguments})
|]
  forM_ registryExtensions $ \(Extension {..}) -> do
    forM_ extensionRequires $ \(RequireExt {..}) -> do
      forM_ requireExtEnums (yield . renderEnumExt extensionNumber)
  yield "-- End of File"
  close

renderEnumExt :: Int -> EnumExt -> Text
renderEnumExt extensionNumber = \case
  NewEnum {..} -> [lt|-- | #{enumEComment}
pattern #{enumEName} = #{enumEValue}|]
  ExtendValue {..} -> [lt|-- | #{enumEComment}
pattern #{enumEName} = #{enumEExtend} #{enumEValue}|]
  ExtendOffset {..} -> [lt|-- | #{enumEComment}
pattern #{enumEName} = #{enumEExtend} #{value'}|]
    where value' = if enumENegative then [lt|(-#{show value})|]
                                    else [lt|#{show value}|]
          value = 1000000000 + (extensionNumber - 1) * 1000 + enumEOffset
  ExtendBitpos {..} -> [lt|-- | #{enumEComment}
pattern #{enumEName} = #{enumEExtend} #{value}|]
    where value = bit enumEBitpos :: Int

showValidaty :: [String] -> Text
showValidaty [] = ""
showValidaty xs = "\n-- == Validaty" <> showUsage xs

showUsage :: [String] -> Text
showUsage xs = mconcat [ "\n-- * " <> formatUsage x | x <- xs ]

formatUsage :: String -> Text
formatUsage = intercalate "\n-- " . splitOn "\n" . pack

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

abbr :: String -> String
abbr = map toLower . filter (\x -> isUpper x || x `elem` ['0'..'9'])

conv "VkInstanceCreateInfo" = "VULKAN"
-- conv "VkImageCreateInfo" = "VkImageCreateInfo"
-- conv "VkComputePipelineCreateInfo" = "VkComputePipelineCreateInfo"
conv "VkCommandPoolCreateInfo" = "VkCommandPOOLCreateInfo"
conv "VkSamplerCreateInfo" = "VkSAMPLERCreateInfo"
conv "VkSemaphoreCreateInfo" = "VkSEMAPHORECreateInfo"
conv "VkFramebufferCreateInfo" = "VkFrameBufferCreateInfo"
-- conv "VkFenceCreateInfo" = "VkFenceCreateInfo"
conv "VkXlibSurfaceCreateInfoKHR" = "VkXLIBSurfaceCreateInfo"
conv "VkXcbSurfaceCreateInfoKHR" = "VkXCBSurfaceCreateInfo"
conv x = x

showMember :: String -> Member -> Text
showMember typeName m@(Member {..}) =
  [lt|#{abbr (conv typeName)}_#{memberName} :: #{"!" : memberType}#{comment m}|]

renderData :: Man -> Type -> Text
renderData man (Struct {..}) = [lt|
-- | #{showMan typeName man}
-- #{showValidaty typeValidity}
data #{typeName} = #{typeName}#{render typeMembers} --deriving (Eq, Show)
|] where render = intercalate "\n" . braced . map (showMember typeName)
renderData man (Union {..}) =
  [lt|data #{typeName} = #{constructors} --deriving (Eq, Show)|]
  where constructors = intercalate " | " $ map showCon typeMembers
        showCon (Member {..}) =
          [lt|#{typeName ++ upperCamel memberName} #{"!" : memberType}|]

structLayout :: SizeDict -> [Member] -> [(String, (Int, Int))]
structLayout sd members = uncurry zip $ (map memberName &&& layout) members
  where offsets (x : xs) = x : map (addp x) (offsets xs)
        layout :: [Member] -> [(Int, Int)]
        layout xs = offsets $ (0, 0) : map (sizeOfMember sd) xs

renderStorable :: SizeDict -> Type -> Text
renderStorable sd (Struct {..}) =
  let from = structLayout sd &&& concatMap ((' ':) . memberName)
      (layout, args) = from typeMembers
      impl f = (intercalate "\n    " <<< map f) layout
      peek (name, pos) = [lt|#{name} <- peek (plusPtr ptr (#{showSize pos}))|]
      poke (name, pos) = [lt|poke (plusPtr ptr (#{showSize pos})) #{name}|]
  in [lt|-- ReturnedOnly = #{show typeReturnedOnly}
instance Storable #{typeName} where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = #{showSize $ sizeOfType sd [typeName]}
  peek ptr = do
    #{impl peek}
    return $ #{typeName}#{args}
  poke ptr (#{typeName}#{args}) = do
    #{impl poke}
|]
renderStorable sd (Union {..}) = [lt|-- ReturnedOnly = False
instance Storable #{typeName} where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = #{showSize $ sizeOfType sd [typeName]}
  peek = error "cannot peek C union"
  #{impl' poke'}
|] where impl' f = (intercalate "\n  " <<< map f) typeMembers
         poke' (Member {..}) =
           [lt|poke ptr (#{typeName}#{upperCamel memberName} a) = poke (castPtr ptr) a|]

showParameter :: Member -> Text
showParameter m@(Member {..}) = [lt|#{memberType}#{comment m}|]

comment :: Member -> Text
comment (Member {..}) = if body /= "" then "\n  -- ^ " <> body else "" 
  where
  body = intercalate " " $ filter (/= "") [name, opt, sync, cons, enum, len, nav]
  name = [lt|`#{memberName}` |]
  opt = case memberOptional of
          "" -> ""
          "true" -> "Can be `nullPtr`."
          _ -> [lt|Optional=#{memberOptional}.|]
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
  , ("Int32", (4,0)), ("Int64", (8,0)), ("CSize", (0,1)), ("CChar", (1,0))
  , ("Ptr ()", (0,1)), ("CString", (0,1))
  ]

showSize :: (Int, Int) -> Text
showSize (bytes, pointers) =
  [lt|#{bytes} + sizeOf (undefined :: CSize) * #{pointers}|]

sizeOfType :: SizeDict -> [String] -> (Int, Int)
sizeOfType dict [name] = maybe (-100000, -100000) id $ lookup name dict
sizeOfType _ ("Ptr" : _) = (0, 1)
sizeOfType d ("V2" : name) = times 2 $ sizeOfType d name
sizeOfType d ("V3" : name) = times 3 $ sizeOfType d name
sizeOfType d ("V4" : name) = times 4 $ sizeOfType d name
sizeOfType d (('V':' ':n) : name) = times (read n) $ sizeOfType d name

addp, maxp :: (Num a, Ord a, Show a) => (a, a) -> (a, a) -> (a, a)
addp (x, y) (z, w) = (x + z, y + w)
maxp (x, 0) (z, 0) = (max x z, 0)

times :: Int -> (Int, Int) -> (Int, Int)
times n (x, y) = (n * x, n * y)

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

sizeOfMember :: SizeDict -> Member -> (Int, Int)  
sizeOfMember dict = sizeOfType dict . memberType

instance ToText [String] where
  toText = fromLazyText . showType

showType :: [String] -> Text
showType [x] = pack x
showType ["!", a] = [lt|!#{a}|]
showType [p, a] = [lt|#{p} #{a}|]
showType ["!", p, a] = [lt|!(#{p} #{a})|]
showType [p, q, a] = [lt|#{p} (#{q} #{a})|]
showType ["!", p, q, a] = [lt|!(#{p} (#{q} #{a}))|]

snakeToCamel :: String -> String
snakeToCamel = id

renderApp :: String -> [Command] -> [Text]
renderApp f xs = [ [lt|<*> #{f} "#{commandName}"|] | Command {..} <- xs ]

preInitCommands :: [String]
preInitCommands = ["vkGetInstanceProcAddr", "vkEnumerateInstanceExtensionProperties", "vkEnumerateInstanceLayerProperties", "vkCreateInstance"]

isDispatchableCommand :: Command -> Bool
isDispatchableCommand c | commandName c == "vkGetDeviceProcAddr" = False
isDispatchableCommand c = (`elem` ["VkDevice", "VkQueue", "VkCommandBuffer"])
   . head . memberType . head . commandParameters $ c

showFpField :: Command -> Text
showFpField (Command {..}) =
  [lt|fp_#{commandName} :: !(FunPtr (#{commandType}))|]
  where commandType = intercalate " -> " $
          map (\x -> [lt|#{memberType x}|]) commandParameters
          ++ [[lt|#{"IO" : commandReturn}|]]

renderExports :: Registry -> [String] -> [Text]
renderExports r@(Registry {..}) typeNames = flip concatMap typeNames $ \n ->
  case findType n of
    Struct {..} -> pack typeName <> "(..)" :
                     renderExports r (vkTypesInMembers typeMembers)
    Union {..} -> pack typeName <> "(..)" :
                    renderExports r (vkTypesInMembers typeMembers)
    EnumType name -> pack name :
                       map (pack . ("pattern " ++) . enumName) (findEnum n)
    x -> [pack (typeName x)]
  where findType n = head [ x | x <- registryTypes, typeName x == n]
        findEnum n = head [enumsList e | e <- registryEnums, enumsName e == n]

renderExportCommands :: Registry -> [String] -> [Text]
renderExportCommands r@(Registry {..}) = concatMap $ \c ->
   let (Command {..}) = findCommand c
   in pack commandName : renderExports r (vkTypesInMembers commandParameters)
   where findCommand n = head [ x | x <- registryCommands, commandName x == n]

vkTypesInMembers :: [Member] -> [String]
vkTypesInMembers = concatMap (concatMap findVk . memberType)
  where findVk xs = case xs of
                      'V':'k':_ -> [xs]
                      _ -> []

showMan :: String -> Man -> Text
showMan title man | lookup title man == Nothing = ""
showMan title (lookup title -> Just (VkMan {..})) =
  [lt|#{manSynopsis}
-- 
-- #{intercalate "\n--\n-- " $ map showManDesc manDescription}|]
-- 
-- Notes: #{show manNotes}
-- 
-- Seealso: #{show manSeealso}
showManDesc :: String -> Text
showManDesc = intercalate "\n-- " . splitOn "\n" . pack

