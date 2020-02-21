module Network.AWS.DynamoDB.Class
    ( class DynamoItem
    , class MonadGetItem
    , class MonadPutItem
    , class MonadRemoveItem
    , class MonadScanItems
    , getItem
    , putItem
    , removeItem
    , scanItems
    ) where

import Data.Maybe (Maybe)
import Data.Unit  (Unit)


-- |The `DynamoItem` type class relates a name of table and a set of a key and an item.
-- |A table name must be unique to keys and items.
class DynamoItem (n :: Symbol) (k :: Type) (i :: Type) | n -> k, n -> i

-- |Gets an item with a key.
class MonadGetItem k i m | m -> k, m -> i where
    getItem :: k -> m (Maybe i)

-- |Puts an item.
class MonadPutItem i m | m -> i where
    putItem :: i -> m Unit

-- |Removes an item with a key.
class MonadRemoveItem k m | m -> k where
    removeItem :: k -> m Unit

-- |Scans all items.
class MonadScanItems i m | m -> i where
    scanItems :: m (Array i)
