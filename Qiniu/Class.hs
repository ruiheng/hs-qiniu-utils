module Qiniu.Class where

-- {{{1
import ClassyPrelude

import Qiniu.Types
-- }}}1


class HasBucket a where
  getBucket :: a -> Bucket

class HasResourceKey a where
  getResourceKey :: a -> ResourceKey

class HasEtagHash a where
  getEtagHash :: a -> EtagHash

class HasMimeType a where
  getMimeType :: a -> Text

class HasFileSize a where
  getFileSize :: a -> Int64

getEntry :: (HasBucket a, HasResourceKey a) => a -> Entry
getEntry = getBucket &&& getResourceKey


instance HasBucket Bucket where getBucket = id

instance HasResourceKey ResourceKey where getResourceKey = id

instance HasEtagHash EtagHash where getEtagHash = id

instance HasBucket Entry where getBucket = fst

instance HasResourceKey Entry where getResourceKey = snd

-- vim: set foldmethod=marker:
