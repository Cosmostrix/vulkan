module VkManual (readVkMan, Man, VkMan(..)) where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Default
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Walk

--import System.Environment
--main = getArgs >>= readVkMan >>= print

type Man = [(String, VkMan)]

readVkMan :: [String] -> IO (Either PandocError Man)
readVkMan pathes = forM pathes readFile >>=
  return . fmap concat . traverse (fmap extractMan . parseWithPandoc)
  
parseWithPandoc :: String -> Either PandocError Pandoc
parseWithPandoc = readMarkdown def { readerExtensions = githubMarkdownExtensions }
                . filter (/= '\r')

data VkMan = VkMan
  { manName :: String
  , manSynopsis :: String
  , manDescription :: [String]
  , manMembers :: Maybe [String]
  , manNotes :: Maybe [String]
  , manSeealso :: Maybe [String]
  } deriving (Show, Eq)

extractMan :: Pandoc -> [(String, VkMan)]
extractMan (Pandoc _ xs) = let c = classify xs in
  case (extractName c, extractDesc c, extractMembers c,
        extractNotes c, extractSeealso c) of
    (Just (_, "Stub page (not yet written)"), _, _, _, _) -> []
    (Just (name, syn), Just desc, xs, notes, sa) ->
      [(name, VkMan name syn desc xs notes sa)]
    (a,b,c,d,e) -> error . show $ (a,b,c,d,e)

classify :: [Block] -> [(String, [[Inline]])]
classify [] = []
classify (Header _  (name, _, _) _ : xs) =
  case span isNotHeader xs of (ps, rest) -> (name, collapse ps) : classify rest
classify x = error (show x)

isNotHeader (Header _ _ _) = False
isNotHeader _ = True

collapse :: [Block] -> [[Inline]]
collapse = concatMap $ \x -> case x of
  Para x -> [x]
  _      -> []

render :: [Inline] -> String
render = query renderInline

renderInline :: Inline -> String
renderInline (Str x) = x
renderInline Space = " "
renderInline LineBreak = "\n"
renderInline (Emph xs) = '_' : (concatMap renderInline xs ++ "_")
renderInline (Code ("", _ ,_) t) = '@' : (t ++ "@")
renderInline x = error (show x)

extractSection title xs = map render <$> lookup title xs

noInclude = filter $ not . isPrefixOf "include::"

extractName :: [(String, [[Inline]])] -> Maybe (String, String)
extractName xs =
  (renderInline . head &&& render . drop 4) . head <$> lookup "name" xs

extractDesc, extractMembers, extractNotes, extractSeealso
         :: [(String, [[Inline]])] -> Maybe [String]
extractDesc xs = extractSection "description" xs

extractMembers xs =
  map (render . drop 2) <$>
    (lookup "fields" xs <|> lookup "parameters" xs <|> lookup "constants" xs)

extractNotes xs = extractSection "notes" xs

extractSeealso xs = noInclude <$> extractSection "see-also" xs

