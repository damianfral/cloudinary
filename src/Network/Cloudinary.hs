{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Cloudinary where

import           Protolude

import           Control.Monad.Base
import           Control.Monad.Trans.Control

import           Data.Cloudinary
import           Data.Generics.Labels        ()
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Text

import           Control.Lens                hiding (Strict)

import           Servant.API
import           Servant.Client

import           Network.HTTP.Client.TLS     (newTlsManager)

-------------------------------------------------------------------------------

type WithAuth a = BasicAuth "cloudinary-realm" (Text, Text) :> a

type BrowseAPI
  =    "resources"
       :> Capture "resource_type" ResourceType
       :> QueryParam "tags" Bool
       :> WithAuth ( Get '[JSON] ResourceBatch )
  :<|> "resources"
       :> Capture "resource_type" ResourceType
       :> "upload"
       :> QueryParam "prefix" Text
       :> QueryParam "tags" Bool
       :> QueryParam "max_results" Int
       :> WithAuth ( Get '[JSON] ResourceBatch )

type FoldersAPI
  =    "folders" :> WithAuth ( Get '[JSON] Folders )
  :<|> "folders" :> CaptureAll "root_folder" Text :> WithAuth ( Get '[JSON] Folders )


type CloudinaryAPI
  = BrowseAPI :<|> FoldersAPI

cloudinaryAPI :: Proxy CloudinaryAPI
cloudinaryAPI = Proxy

-------------------------------------------------------------------------------

data CloudinaryIOEnv = CloudinaryIOEnv
  { clientEnv :: ClientEnv, config :: CloudinaryConfig
  } deriving stock ( Generic)

getBaseUrl :: CloudinaryConfig -> BaseUrl
getBaseUrl cc = BaseUrl Https "api.cloudinary.com" 443
              $ unpack $ mconcat [ "v1_1/" , cc ^. #cloudinaryProjectName ]

injectError :: (MonadError parent m, AsType child parent) => child -> m a
injectError = throwError . injectTyped

initialize :: CloudinaryConfig -> IO CloudinaryIOEnv
initialize cc = do
  mgr  <- newTlsManager
  let clientEnv = mkClientEnv mgr bUrl
  pure $ CloudinaryIOEnv clientEnv cc
  where bUrl = getBaseUrl cc

-- finalize :: MonadIO m => ClientEnv -> m ()--
-- finalize ( ClientEnv mgr _ _ ) = liftIO $ closeManager manager--

getResources'         :: ResourceType -> Maybe Bool -> BasicAuthData -> ClientM ResourceBatch
getResourcesByFolder' :: ResourceType -> Maybe Text -> Maybe Bool -> Maybe Int -> BasicAuthData
                      -> ClientM ResourceBatch
getRootFolders'       :: BasicAuthData -> ClientM Folders
getFolders'           :: [ Text ] -> BasicAuthData -> ClientM Folders

( getResources' :<|> getResourcesByFolder' )
  :<|> ( getRootFolders' :<|> getFolders' )
  = client cloudinaryAPI


cloudinaryConfigToBasicAuth :: CloudinaryConfig -> BasicAuthData
cloudinaryConfigToBasicAuth cc = BasicAuthData u p
  where
    u = encodeUtf8 $ cc ^. #cloudinaryApiKey
    p = encodeUtf8 $ cc ^. #cloudinaryApiSecret

getStatusCode :: Response -> Int
getStatusCode ( Response status  _ _ _ ) = fromEnum status

toCloudinaryError
  :: ( AsCloudinaryError e
     , MonadError e m )
  => ClientError -> m a
toCloudinaryError f@( FailureResponse _ r ) = injectError $
  case getStatusCode r of
    400 -> BadRequest
    401 -> AuthRequired
    403 -> NotAllowed
    404 -> NotFound
    409 -> AlreadyExists
    420 -> RateLimited
    _   -> ServantError ( show f )
toCloudinaryError r = injectError $ ServantError $ show r

wrap
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasType CloudinaryIOEnv r
     , AsCloudinaryError e
     , Show a )
  => ( BasicAuthData -> ClientM a ) -> m a

wrap authClientM = do

  cEnv :: CloudinaryIOEnv <- view typed
  let bAuth = cloudinaryConfigToBasicAuth $ cEnv ^. #config
  res       <- liftIO $ runClientM ( authClientM bAuth ) ( cEnv ^. #clientEnv )
  case res of
    Left  e -> toCloudinaryError e
    Right r -> return r


instance
  ( MonadIO m
  , MonadReader r m
  , HasType CloudinaryIOEnv r
  , MonadError e m
  , AsCloudinaryError e
  ) => Cloudinary e ( CloudinaryIO m ) where

  getRootFolders
    = wrap $ getRootFolders'

  getFolders folder
    = wrap $ getFolders' $ splitOn "/" folder

  getResources resourceType
    = wrap $ fmap resources . ( getResources' resourceType ( Just True ) )

  getResourcesByFolder resourceType folder
    = wrap $ fmap resources
           . getResourcesByFolder' resourceType ( Just folder ) ( Just True ) (Just 500)

-------------------------------------------------------------------------------

class ( MonadError e m , AsCloudinaryError e ) => Cloudinary e m where
  getResources         :: ResourceType -> m [ Resource ]
  getResourcesByFolder :: ResourceType -> Text -> m [ Resource ]
  getRootFolders       :: m Folders
  getFolders           :: Text -> m Folders


newtype CloudinaryIO m a = CloudinaryIO ( m a )
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadIO
    , MonadError e
    )

deriving instance ( Applicative m, Monad m, MonadIO m) => MonadBase IO (CloudinaryIO m)
deriving instance ( Applicative m, Monad m, MonadIO m) => MonadBaseControl IO (CloudinaryIO m)

