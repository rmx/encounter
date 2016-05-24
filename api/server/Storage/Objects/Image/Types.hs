{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Storage.Objects.Image.Types where


import Avers
import Avers.TH



-----------------------------------------------------------------------------
-- | Image

data Image = Image
    { imageBlobId    :: BlobId
      -- ^ The original image blob.

    , imageRelatedId :: Maybe ObjId
      -- ^ An object to which the image is in a relation. This
      -- can be for example the encounter for which this screenshot
      -- was created.
    } deriving (Show)





$(deriveEncoding (deriveJSONOptions "image") ''Image)
