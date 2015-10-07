module Main where

import Text.Mustache
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Char 
import Data.Monoid
import Data.List

import qualified Text.Mustache.Types as Mustache
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Vector as V
import System.IO.Unsafe
import qualified Data.Yaml as Yaml
import Control.Lens

main :: IO ()
main = do
  txt <- Text.readFile "resources/opaleye-template.hs.mustache"
  template <- case compileTemplate "persistent_opaleye" txt of
    Left e -> fail (show e)
    Right a -> return a
  config <- Text.readFile "config/models"
  let tokenized = splitConfigFile config
      models = map parseConfigSingle tokenized
  Text.putStrLn $ substituteValue template $ object
    [ "models" ~> models
    ]

parseConfigSingle :: (Text,[[Text]]) -> Model
parseConfigSingle (tableName,attrs) = Model (Text.words tableName !! 0) hasCustomPrimaryKey fields
  where
  fields = map parseField columns
  columns = filter (\attr -> attr !! 0 /= "deriving" 
                          && attr !! 0 /= "Primary" 
                          && attr !! 0 /= "Foreign"
                          && Text.take 6 (attr !! 0) /= "Unique") attrs 
  hasCustomPrimaryKey = anyOf (traverse . ix 0) (=="Primary") attrs

parseField :: [Text] -> Field
parseField ts = Field (ts !! 0) (applyMaybe $ ts !! 1) True
  where 
  applyMaybe t = if ts ^? ix 3 == Just "Maybe" then "Maybe " <> t else t

splitConfigFile :: Text -> [(Text,[[Text]])]
splitConfigFile t = groupTexts groupedLines
  where 
  groupedLines = groupBy (\a b -> (fst <$> Text.uncons a) == (fst <$> Text.uncons b)) goodLines
  goodLines = filter (\l -> 
    let tnoSpace = Text.stripStart l
        a = not . Text.null $ tnoSpace
        b = (fst <$> Text.uncons tnoSpace) /= Just '-'
    in a && b
    ) $ Text.lines t

groupTexts :: [[Text]] -> [(Text,[[Text]])]
groupTexts ([tableName] : columns : xs) = (tableName,map Text.words columns) : groupTexts xs
groupTexts [] = []
groupTexts [_] = error "The grouping failed because an uneven number was found"
groupTexts _ = error "The grouping phase failed"

testModels :: [Model]
testModels = 
  [ Model "Car" False
    [ Field "speed" "Int" True
    , Field "legal" "Bool" True
    ]
  , Model "Person" False
    [ Field "name" "Text" True
    , Field "age" "Int" True
    , Field "height" "Int" True
    ]
  ]

data Model = Model 
  { modelName :: Text
  , modelCustomPrimaryKey :: Bool
  , modelFields :: [Field]
  } deriving Show

instance ToMustache Model where
  toMustache m = object
    [ "model_name" ~> CasedText (modelName m)
    , "fields" ~> listValuesWithExtra (possiblyAddKey $ map fieldToObject (modelFields m))
    , "custom_pk" ~> modelCustomPrimaryKey m
    ]
    where 
    possiblyAddKey :: [Mustache.Object] -> [Mustache.Object]
    possiblyAddKey fs = if modelCustomPrimaryKey m 
      then fs
      else HM.fromList 
        [ "field_name" ~> CasedText "id"
        , "type"       ~> (modelName m <> "Id")
        , "pgtype"     ~> (modelName m <> "IdColumn")
        , "required"   ~> True
        ] : fs

data Field = Field
  { fieldName  :: Text 
  , fieldType  :: Text
  , fieldRequired :: Bool
  } deriving (Show)

-- instance ToMustache Field where
--   toMustache f = object
--     [ "field_name" ~> CasedText (fieldName f)
--     , "type" ~> fieldType f
--     ]

fieldToObject :: Field -> Mustache.Object
fieldToObject f = HM.fromList
  [ "field_name" ~> CasedText (fieldName f)
  , "type" ~> fieldType f
  , "pgtype" ~> getPgType (fieldType f)
  , "required" ~> fieldRequired f
  ]

getPgType :: Text -> Text
getPgType t = if Text.reverse (Text.take 2 (Text.reverse t)) == "Id"
  then (Text.dropEnd 2 t) <> "IdColumn"
  else case Map.lookup t pgTypeMap of
    Just a -> a
    Nothing -> error $ "Couldn't find a postgres type that matched with " <> Text.unpack t

pgTypeMap :: Map Text Text
pgTypeMap = case unsafePerformIO (Yaml.decodeFile "config/pgmap.yml") of
  Just a -> a
  Nothing -> error "pgmap.yaml has something wrong with it"

listValuesWithExtra :: [Mustache.Object] -> Mustache.Value
listValuesWithExtra as = Mustache.Array $ V.fromList $ flip map (zip [0..] as) 
  $ \(i,a) -> Mustache.Object $ mconcat
  [ a
  , HM.singleton "first" (toMustache $ isFirst i)
  , HM.singleton "last"  (toMustache $ isLast i)
  , HM.singleton "index" $ object
    [ "letter" ~> chr (ord 'a' + i)
    , "number" ~> Text.pack (show i)
    ]
  ]
  where 
  isFirst i = i == 0
  isLast i = i == length as

newtype CasedText = CasedText Text

instance ToMustache CasedText where
  toMustache (CasedText t) = object
    [ "title" ~> firstCharUpper t
    , "camel" ~> firstCharLower t
    , "snake" ~> makeSnake (firstCharLower t)
    ]

makeSnake :: Text -> Text
makeSnake = appEndo (mconcat (map makeLowerWithUnderscore uppers))
  where 
  uppers = ['A'..'Z']
  makeLowerWithUnderscore c = Endo $ Text.replace (Text.singleton c) ("_" <> Text.singleton (toLower c))

firstCharUpper :: Text -> Text 
firstCharUpper t = case Text.uncons t of
  Nothing -> ""
  Just (c,cs) -> Text.cons (toUpper c) cs

firstCharLower :: Text -> Text
firstCharLower t = case Text.uncons t of
  Nothing -> ""
  Just (c,cs) -> Text.cons (toLower c) cs

