{-# LANGUAGE DeriveAnyClass #-}

module Data.Cloudinary where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Text             (toLower)

import           System.Envy
import           Web.HttpApiData

-------------------------------------------------------------------------------

-- Cloudinary Types

data Resource = Resource
  { public_id     :: Text
  , format        :: Maybe Text
  , version       :: Int
  , resource_type :: ResourceType
  , created_at    :: Text -- TODO: Use date
  , bytes         :: Int
  , width         :: Maybe Int
  , height        :: Maybe Int
  , url           :: Text
  , secure_url    :: Text
  , tags          :: [ Text ]
  } deriving ( Show, Eq, Generic, FromJSON, ToJSON )


data ResourceType = Image
                  | Video
                  | Raw
                  deriving ( Show, Eq, Generic )

instance ToHttpApiData ResourceType where
  toUrlPiece = toLower . show


instance ToJSON ResourceType where
  toJSON = String . toLower . show

instance FromJSON ResourceType where
  parseJSON ( String "image" ) = pure Image
  parseJSON ( String "video" ) = pure Video
  parseJSON ( String "raw"   ) = pure Raw
  parseJSON invalid            = typeMismatch "ResourceType" invalid

data StorageType = Upload
                 | Private
                 | Authenticated
                 | Facebook
                 | Twitter
                 | Gplus
                 | Instagram_name
                 | Gravatar
                 | Youtube
                 | Hulu
                 | Vimeo
                 | Animoto
                 | Worldstarhiphop
                 | Dailymotion
                 deriving ( Show, Eq )

data ResourceBatch = ResourceBatch
  { resources  :: [ Resource ]
  , nextCursor :: Maybe Text
  } deriving ( Show, Eq, Generic, FromJSON, ToJSON )

data Folder = Folder
  { name :: Text
  , path :: Text
  } deriving ( Show, Eq, Generic, FromJSON, ToJSON )

data Folders = Folders
  { folders :: [ Folder ]
  } deriving ( Show, Eq, Generic, FromJSON, ToJSON )


-------------------------------------------------------------------------------

-- * Configuration

data CloudinaryConfig = CloudinaryConfig
  { cloudinaryProjectName :: Text
  , cloudinaryApiKey      :: Text
  , cloudinaryApiSecret   :: Text
  } deriving ( Show, Eq, Generic, FromJSON, ToJSON )

instance DefConfig CloudinaryConfig where defConfig = CloudinaryConfig "" "" ""
instance FromEnv CloudinaryConfig

type HasCloudinaryConfig r = HasType CloudinaryConfig r

-------------------------------------------------------------------------------

-- * Errors

data CloudinaryError
  = BadRequest
  | AuthRequired
  | NotAllowed
  | NotFound
  | AlreadyExists
  | RateLimited
  | ServantError Text
  deriving ( Show, Eq, Generic )

type AsCloudinaryError e = AsType CloudinaryError e

