module Data.Cloudinary.Aux where

import           Data.Aeson.TH

unPrefixLabels :: Options
unPrefixLabels = defaultOptions { fieldLabelModifier = go }
  where
    go str = case str of
               '_':xs -> go xs
               _      -> str


