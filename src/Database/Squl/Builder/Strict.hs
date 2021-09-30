module Database.Squl.Builder.Strict
    ( buildQuery
    ) where

import Data.ByteString              (ByteString)
import Data.ByteString.Builder
import Database.Squl.Builder.Common (Buildable)
import Data.ByteString.Lazy.Char8   (toStrict)

import qualified Database.Squl.Builder.Common as Builder

buildQuery :: Buildable q => q -> ByteString
buildQuery query = toStrict $ toLazyByteString $ Builder.buildQuery query
