{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE Arrows, RecordWildCards #-}
import Control.Monad (forM_)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.IO as L
import System.Environment
import Text.Shakespeare.Text
import Text.XML.HXT.Core
import Data.List
import Data.Monoid
import Data.String
import Data.Char (toLower, toUpper)
import Data.Bits

main :: IO ()
main = do
  args <- getArgs
  case args of
    [vk_xml, destdir] -> main' vk_xml destdir
    _ -> putStrLn "Usage: vulkan-apigen vk.xml autogen"

main' :: String -> String -> IO ()
main' src destdir = do
  L.putStrLn [lt|−− | Complete Vulkan raw API bindings.
module Graphics.Vulkan.Bindings where
import Foreign
import Foreign.C
import Data.Bits
import Data.Int
import Data.Word
import Numeric.Half
import Numeric.Fixed
|]
  [Registry {..}] <- runX (readDocument [withRemoveWS yes] src >>> parse)
  forM_ registryTypes $ \case
    Struct {..} -> L.putStrLn [lt|
-- |
-- #{showUsage typeValidity}
data #{typeName} = #{typeName}#{members} deriving (Eq, Show)
  -- ReturnedOnly? = #{show typeReturnedOnly}

instance Storable #{typeName} where
  sizeOf _ = 0
  peek = return ()
  poke = return ()
|] where members = intercalate "\n" . braced $
                     map (showMember typeName) typeMembers
    Basetype {..} ->
      L.putStrLn [lt|type #{typeName} = #{haskType' typeType}|]
    _ -> return ()
  forM_ registryEnums $ \(Enumeratees {..}) -> do
    let derivings = case enumsType of
                      "enum" -> "(Eq, Ord, Show)" :: Text
                      "bitmask" -> "(Eq, Ord, Bits, FiniteBits, Show)" -- allow binary ops
    L.putStrLn [lt|-- | #{enumsComment}
newtype #{enumsName} = #{enumsName} deriving #{derivings}

#{mconcat $ map (showEnum enumsName) enumsList}
|]
  forM_ registryCommands $ \(Command {..}) ->
    L.putStrLn [lt|
-- | @#{commandName} {params}@
-- #{showUsage commandValidity}
-- 
-- s:#{commandSuccessCodes} e:#{commandErrorCodes} q:#{commandQueues} rp:#{commandRenderpass} cbl:#{commandCmdBufferLevel} iesp:#{show commandImplicitExternSyncParams}
#{commandName} :: -> IO #{commandReturn}
#{commandName} {params} =
  #{mconcat $ map showParameter commandParameters}
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
  unpack [st|#{lowerCamel typeName}_#{memberName} :: #{wrapMaybe $ wrapPtrs memberLen $ haskType' memberType}#{comment}|]
  where wrapMaybe x = if memberOptional then "Maybe (" ++ x ++ ")" else x :: String
        comment = if enum <> len <> nav /= ""
                  then [st|
  -- ^#{enum}#{len}#{nav}|]
                  else ""
        enum = maybe "" (\enum -> [st| Max: #{enum}.|]) memberEnum
        len = maybe "" (\len -> [st| Length: #{len}.|]) memberLen
        nav = if not memberNoautovalidity then "" else " Noautovalidity."
        wrapPtrs l x = maybe x (\len ->
                         case length (elemIndices ',' len) of
                           0 -> "Ptr " ++ x
                           1 -> "Ptr (Ptr " ++ x ++ ")" :: String) l

showEnum :: String -> Enumeratee -> Text
showEnum enumsName (EnumValue {..}) =
  [st|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{enumValue}
|]
showEnum enumsName (EnumBitpos {..}) =
  [st|-- | #{enumComment}
pattern #{enumName} = #{enumsName} #{value}
|] where value = bit enumBitpos :: Int

showParameter :: Parameter -> Text
showParameter (Parameter {..}) = [st|param #{parameterName} #{haskType parameterType} opt:#{parameterOptional} es:#{parameterExternsync} len:#{show parameterLen} nav:#{show parameterNoautovalidity}
|]

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
  x -> x

haskType' :: String -> String
haskType' "void" = "Ptr ()" -- for *pNext
haskType' rest = haskType rest

data Registry = Registry
  { registryTypes :: [Type]
  , registryEnums :: [Enumeratees]
  , registryCommands :: [Command]
  , registryFeatures :: [Feature]
  , registryExtensions :: [Extension]
  } deriving (Eq, Show)

data Type =
    Struct
    { typeName :: String
    , typeMembers :: [Member]
    , typeValidity :: [String]
    , typeReturnedOnly :: Bool
    }
  | Union
    { typeName :: String
    , typeComment :: String
    , typeMembers :: [Member]
    }
  | Include
    { typeName :: String }
  | RequireOther
    { typeName :: String
    , typeRequires :: String
    }
  | Define
    { typeName :: String
    , typeDecl :: String
    }
  | Basetype
    { typeName :: String
    , typeType :: String
    }
  | Bitmask
    { typeName :: String
    , typeType :: String
    , typeRequires :: String
    }
  | Handle
    { typeName :: String
    , typeType :: String
    , typeParent :: Maybe String
    }
  | EnumType
    { typeName :: String }
  | Funcpointer
    { typeName :: String
    , typeArgs :: [String]
    }
  deriving (Eq, Show)

data Member = Member
  { memberName :: String
  , memberType :: String
  , memberEnum :: Maybe String
  , memberOptional :: Bool
  , memberLen :: Maybe String
  , memberNoautovalidity :: Bool
  } deriving (Eq, Show)

data Enumeratees = Enumeratees
  { enumsName :: String
  , enumsType :: String
  , enumsExpand :: Maybe String
  , enumsComment :: String
  , enumsList :: [Enumeratee]
  } deriving (Eq, Show)

data Enumeratee =
    EnumValue
    { enumName :: String
    , enumValue :: String
    , enumComment :: String
    }
  | EnumBitpos
    { enumName :: String
    , enumBitpos :: Int
    , enumComment :: String
    }
  deriving (Eq, Show)

data EnumExt =
    NewEnum
    { enumEName :: String
    , enumEValue :: String
    , enumEComment :: String
    }
  | ExtendValue
    { enumEName :: String
    , enumEValue :: String
    , enumEExtend :: String
    , enumEComment :: String
    }
  | ExtendOffset
    { enumEName :: String
    , enumEOffset :: Int
    , enumEExtend :: String
    , enumEComment :: String
    }
  | ExtendBitpos
    { enumEName :: String
    , enumEBitpos :: Int
    , enumEExtend :: String
    , enumEComment :: String
    }
  deriving (Eq, Show)

data Command = Command
  { commandName :: String
  , commandReturn :: String
  , commandSuccessCodes :: String
  , commandErrorCodes :: String
  , commandQueues :: String
  , commandRenderpass :: String
  , commandCmdBufferLevel :: String
  , commandParameters :: [Parameter]
  , commandImplicitExternSyncParams :: [String]
  , commandValidity :: [String]
  } deriving (Eq, Show)

data Parameter = Parameter
  { parameterName :: String
  , parameterType :: String
  , parameterOptional :: String -- "true", "false,true"
  , parameterExternsync :: String -- "true", ...
  , parameterLen :: Maybe String
  , parameterNoautovalidity :: Bool
  } deriving (Eq, Show)

data Feature = Feature
  { featureName :: String
  , featureRequires :: [Require]
  , featureRemoves :: [Remove]
  } deriving (Eq, Show)

data Require = Require
  { requireComment :: String
  , requireTypes :: [String]
  , requireEnums :: [String]
  , requireCommands :: [String]
  } deriving (Eq, Show)

data Remove = Remove deriving (Eq, Show)

data Extension = Extension
  { extensionName :: String
  , extensionAuthor :: Maybe String
  , extensionContact :: Maybe String
  , extensionSupport :: String
  , extensionProtect :: Maybe String
  , extensionRequires :: [RequireExt]
  } deriving (Eq, Show)

data RequireExt = RequireExt
  { requireExtTypes :: [String]
  , requireExtEnums :: [EnumExt]
  , requireExtCommands :: [String]
  , requireExtUsage :: [(String, String)]
  } deriving (Eq, Show)

to :: ArrowXml a => String -> a XmlTree XmlTree
to name = hasName name <<< isElem <<< getChildren

perhaps :: ArrowIf a => a b c -> a b (Maybe c)
perhaps x = (arr Just <<< x) `orElse` constA Nothing

boolean :: ArrowIf a => a b String -> a b Bool
boolean x = (arr (\x -> if x == "true" then True else error x) <<< x)
            `orElse` constA False

getContent :: ArrowXml a => a XmlTree String
getContent = getText <<< getChildren

parse :: IOSLA (XIOState ()) XmlTree Registry
parse = proc x -> do
  registry <- to "registry" -< x
  types <- listA $ parseTypes <<< to "types" -< registry
  enums <- listA $ parseEnums <<< to "enums" -< registry
  commands <- listA $ parseCommand <<< to "commands" -< registry
  features <- listA $ parseFeature <<< to "feature" -< registry
  extensions <- listA $ parseExtension <<< to "extensions" -< registry
  returnA -< Registry
    { registryTypes = types
    , registryEnums = enums
    , registryCommands = commands
    , registryFeatures = features
    , registryExtensions = extensions
    }

parseTypes :: (ArrowXml a, ArrowChoice a) => a XmlTree Type
parseTypes = proc x -> do
  t <- to "type" -< x
  category <- perhaps (getAttrValue0 "category") -< t
  case category of
    Just "struct" -> do
      name <- getAttrValue0 "name" -< t
      members <- listA $ parseMembers <<< to "member" -< t
      validity <- listA $ getContent <<< to "usage" <<< to "validity" -< t
      retonly <- boolean (getAttrValue0 "returnedonly") -< t
      returnA -< Struct name members validity retonly
    Just "union" -> do
      name <- getAttrValue0 "name" -< t
      comment <- getAttrValue "comment" -< t
      members <- listA $ parseMembers <<< to "member" -< t
      returnA -< Union name comment members
    Just "basetype" -> do
      name <- getContent <<< to "name" -< t
      typ <- getContent <<< to "type" -< t
      returnA -< Basetype name typ
    Just "bitmask" -> do
      name <- getContent <<< to "name" -< t
      typ <- getContent <<< to "type" -< t
      requires <- getAttrValue "requires" -< t
      returnA -< Bitmask name typ requires
    Just "enum" -> do
      name <- getAttrValue0 "name" -< t
      returnA -< EnumType name
    _ -> zeroArrow -< t

parseMembers :: ArrowXml a => a XmlTree Member
parseMembers = proc x -> do
  name <- getContent <<< to "name" -< x
  typ <- getContent <<< to "type" -< x
  enum <- perhaps (getContent <<< to "enum") -< x
  optional <- boolean (getAttrValue0 "optional") -< x
  len <- perhaps (getAttrValue0 "len") -< x
  nav <- boolean (getAttrValue0 "noautovalidity") -< x
  returnA -< Member name typ enum optional len nav

parseEnums :: ArrowXml a => a XmlTree Enumeratees
parseEnums = proc x -> do
  name <- getAttrValue0 "name" -< x
  typ <- getAttrValue0 "type" -< x
  expand <- perhaps (getAttrValue0 "expand") -< x
  comment <- getAttrValue "comment" -< x
  enums <- listA $ parseEnum -< x
  returnA -< Enumeratees name typ expand comment enums

parseEnum :: ArrowXml a => a XmlTree Enumeratee
parseEnum = proc x -> do
  enum <- to "enum" -< x
  name <- getAttrValue0 "name" -< enum
  mvalue <- perhaps (getAttrValue0 "value") -< enum
  mbitpos <- perhaps (getAttrValue0 "bitpos") -< enum
  comment <- getAttrValue "comment" -< enum
  returnA -< case (mvalue, mbitpos) of
    (Just value, Nothing) ->
      EnumValue name value comment
    (Nothing, Just bitpos) ->
      EnumBitpos name (read bitpos) comment
    unexpected -> error (show unexpected)

parseCommand :: ArrowXml a => a XmlTree Command
parseCommand = proc x -> do
  command <- to "command" -< x
  name <- getContent <<< to "name" <<< to "proto" -< command
  ret <- getContent <<< to "type" <<< to "proto" -< command
  successcodes <- getAttrValue "successcodes" -< command
  errorcodes <- getAttrValue "errorcodes" -< command
  queue <- getAttrValue "queue" -< command
  renderpass <- getAttrValue "renderpass" -< command
  buflevel <- getAttrValue "cmdbufferlevel" -< command
  params <- listA $ parseParameters <<< to "param" -< command
  syncparams <- listA $ getContent <<< to "param"
                          <<< to "implicitecommandternsyncparams" -< command
  validity <- listA $ getContent <<< to "usage" <<< to "validity" -< command
  returnA -< Command name ret successcodes errorcodes queue renderpass
                     buflevel params syncparams validity

parseParameters :: ArrowXml a => a XmlTree Parameter
parseParameters = proc x -> do
  name <- getContent <<< to "name" -< x
  typ <- getContent <<< to "type" -< x
  optional <- getAttrValue "optional" -< x
  externalsync <- getAttrValue "externsync" -< x
  len <- perhaps (getAttrValue0 "len") -< x
  noautovalidity <- boolean (getAttrValue0 "noautovalidity") -< x
  returnA -< Parameter name typ optional externalsync len noautovalidity

parseFeature :: ArrowXml a => a XmlTree Feature
parseFeature = proc x -> do
  name <- getAttrValue0 "name" -< x
  require <- listA $ parseRequire <<< to "require" -< x
  remove <- listA $ parseRemove <<< to "remove" -< x
  returnA -< Feature name require remove

parseRequire :: ArrowXml a => a XmlTree Require
parseRequire = proc x -> do
  comment <- getAttrValue "comment" -< x
  types <- listA $ getAttrValue0 "name" <<< to "type" -< x
  enums <- listA $ getAttrValue0 "name" <<< to "enum" -< x
  commands <- listA $ getAttrValue0 "name" <<< to "command" -< x
  returnA -< Require comment types enums commands

parseRemove :: ArrowXml a => a XmlTree Remove
parseRemove = zeroArrow

parseExtension :: ArrowXml a => a XmlTree Extension
parseExtension = proc x -> do
  extension <- to "extension" -< x
  name <- getAttrValue0 "name" -< extension
  author <- perhaps (getAttrValue0 "author") -< extension
  contact <- perhaps (getAttrValue0 "contact") -< extension
  supported <- getAttrValue0 "supported" -< extension
  protect <- perhaps (getAttrValue0 "protect") -< extension
  require <- listA $ parseRequireExt <<< to "require" -< extension
  returnA -< Extension name author contact supported protect require

parseRequireExt :: ArrowXml a => a XmlTree RequireExt
parseRequireExt = proc x -> do
  types <- listA $ getAttrValue0 "name" <<< to "type" -< x
  enums <- listA $ parseEnumExt -< x
  commands <- listA $ getAttrValue0 "name" <<< to "command" -< x
  usages <- listA $ parseUsage <<< to "usage" -< x
  returnA -< RequireExt types enums commands usages

parseEnumExt :: ArrowXml a => a XmlTree EnumExt
parseEnumExt = proc x -> do
  enum <- to "enum" -< x
  name <- getAttrValue0 "name" -< enum
  mvalue <- perhaps (getAttrValue0 "value") -< enum
  moffset <- perhaps (getAttrValue0 "offset") -< enum
  mbitpos <- perhaps (getAttrValue0 "bitpos") -< enum
  mextend <- perhaps (getAttrValue0 "extends") -< enum
  comment <- getAttrValue "comment" -< enum
  returnA -< case (mvalue, moffset, mbitpos, mextend) of
    (Just value, Nothing, Nothing, Nothing) ->
      NewEnum name value comment
    (Just value, Nothing, Nothing, Just extend) ->
      ExtendValue name value extend comment
    (Nothing, Just offset, Nothing, Just extend) ->
      ExtendOffset name (read offset) extend comment
    (Nothing, Nothing, Just bitpos, Just extend) ->
      ExtendBitpos name (read bitpos) extend comment
    unexpected -> error (show unexpected)

parseUsage :: ArrowXml a => a XmlTree (String, String)
parseUsage = proc x -> do
  command <- getAttrValue0 "command" -< x
  text <- getContent -< x
  returnA -< (command, text)
