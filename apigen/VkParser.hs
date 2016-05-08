{-# LANGUAGE Arrows #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Cosmostrix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Cosmostrix <cosmos@lunakit.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse vk.xml
----------------------------------------------------------------------------
module VkParser (parseVkXml) where
import Control.Applicative ((<|>))
import Text.XML.HXT.Core
import VkRegistry

parseVkXml :: String -> IO Registry
parseVkXml src = head <$> runX (readDocument [withRemoveWS yes] src >>> parse)

to :: ArrowXml a => String -> a XmlTree XmlTree
to name = hasName name <<< isElem <<< getChildren

perhaps :: ArrowIf a => a b c -> a b (Maybe c)
perhaps x = (arr Just <<< x) `orElse` constA Nothing

match :: (ArrowIf a) => String -> a b String -> a b Bool
match str x = (constA True <<< isA ((== str) . filter (/= ' ')) <<< x)
              `orElse` constA False

boolean :: ArrowIf a => a b String -> a b Bool
boolean = match "true"

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
    Just "handle" -> do
      name <- getContent <<< to "name" -< t
      typ <- getContent <<< to "type" -< t
      parent <- perhaps (getAttrValue0 "parent") -< t
      returnA -< Handle name typ parent
    Just "enum" -> do
      name <- getAttrValue0 "name" -< t
      returnA -< EnumType name
    Just "funcpointer" -> do
      name <- getContent <<< to "name" -< t
      args <- listA $ getContent <<< to "type" -< t
      returnA -< Funcpointer name (map (:[]) args)
    Nothing -> do
      name <- getAttrValue0 "name" -< t
      requires <- getAttrValue0 "requires" -< t
      returnA -< RequireType name requires
    _ -> zeroArrow -< t

parseMembers :: ArrowXml a => a XmlTree Member
parseMembers = proc x -> do
  name <- getContent <<< to "name" -< x
  typ <- getContent <<< to "type" -< x
  star1 <- match "*" getContent -< x
  star2 <- match "**" getContent -< x
  star2' <- match "*const*" getContent -< x
  let typ' = if star1 then ["*", typ]
        else if star2 || star2' then ["*", "*", typ]
        else [typ]
  const <- match "const" getContent -< x
  enum <- perhaps (getContent <<< to "enum") -< x
  items <- perhaps (isA (`elem` ["[2]","[3]","[4]"]) <<< getContent) -< x
  optional <- getAttrValue "optional" -< x
  externsync <- getAttrValue "externsync" -< x -- <param> only
  len <- perhaps (getAttrValue0 "len") -< x
  nav <- boolean (getAttrValue0 "noautovalidity") -< x
  returnA -< Member name typ' const (enum <|> items) optional externsync len nav

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
  params <- listA $ parseMembers <<< to "param" -< command
  syncparams <- listA $ getContent <<< to "param"
                          <<< to "implicitecommandternsyncparams" -< command
  validity <- listA $ getContent <<< to "usage" <<< to "validity" -< command
  returnA -< Command name [ret] successcodes errorcodes queue renderpass
                     buflevel params syncparams validity

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
  number <- getAttrValue0 "number" -< extension
  require <- listA $ parseRequireExt <<< to "require" -< extension
  returnA -< Extension name author contact supported protect (read number) require

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
  mdir <- perhaps (getAttrValue0 "dir") -< enum
  mbitpos <- perhaps (getAttrValue0 "bitpos") -< enum
  mextend <- perhaps (getAttrValue0 "extends") -< enum
  comment <- getAttrValue "comment" -< enum
  returnA -< case (mvalue, moffset, mdir, mbitpos, mextend) of
    (Just value, Nothing, _, Nothing, Nothing) ->
      NewEnum name value comment
    (Just value, Nothing, _, Nothing, Just extend) ->
      ExtendValue name value extend comment
    (Nothing, Just offset, Nothing, Nothing, Just extend) ->
      ExtendOffset name (read offset) False extend comment
    (Nothing, Just offset, Just "-", Nothing, Just extend) ->
      ExtendOffset name (read offset) True extend comment
    (Nothing, Nothing, _, Just bitpos, Just extend) ->
      ExtendBitpos name (read bitpos) extend comment
    unexpected -> error (show unexpected)

parseUsage :: ArrowXml a => a XmlTree (String, String)
parseUsage = proc x -> do
  command <- getAttrValue0 "command" -< x
  text <- getContent -< x
  returnA -< (command, text)

