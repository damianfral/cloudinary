{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI where

import Control.Monad.Trans.Resource
import Data.Cloudinary
import Network.Cloudinary
import Protolude
import System.Envy

newtype AppM r e a = AppM {runAppM :: ExceptT e (ResourceT (ReaderT r IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadError e
    )

newtype AppEnv = AppEnv {cloudinaryEnv :: CloudinaryIOEnv} deriving (Generic)

newtype AppError = CloudinaryAppError CloudinaryError deriving (Generic, Show)

type App = AppM AppEnv AppError

deriving via CloudinaryIO App instance Cloudinary AppError App

runApp :: App a -> CloudinaryConfig -> IO (Either AppError a)
runApp app r =
  bracket (initialize r) (const $ pure ()) $
    runReaderT (runResourceT $ runExceptT $ runAppM app) . AppEnv

main :: IO ()
main = do
  Just cc <- decode
  print cc
  folders' <- runApp getRootFolders cc
  print folders'
