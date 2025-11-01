module Model.Options (Options (..)) where

import Optics.Label ()

data Options = Options
    { pollInterval :: Int
    , theme :: Maybe FilePath
    , tailTemplate :: Text
    }
    deriving (Show, Generic)
