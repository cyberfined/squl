module Database.Squl.Builder.Lazy
    ( buildQuery
    ) where

import Data.ByteString.Builder
import Database.Squl.Builder.Common (Buildable)
import Data.ByteString.Lazy         (ByteString)

import qualified Database.Squl.Builder.Common as Builder

buildQuery :: Buildable q => q -> ByteString
buildQuery query = toLazyByteString $ Builder.buildQuery query
