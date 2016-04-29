-- | Datatypes corresponding to vk.xml
module VkRegistry where

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

