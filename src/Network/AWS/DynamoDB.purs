module Network.AWS.DynamoDB
    ( DynamoDB
    , documentClient
    , runDynamoDB
    ) where

import Prelude

import Control.Alt                   (class Alt)
import Control.Monad.Reader.Class    (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans    (ReaderT (..), runReaderT)
import Control.Monad.Error.Class     (class MonadThrow, class MonadError)
import Control.Monad.Rec.Class       (class MonadRec)
import Control.Plus                  (class Plus)
import Data.Newtype                  (class Newtype, unwrap)
import Data.Symbol                   (class IsSymbol, SProxy(..), reflectSymbol)
import Effect                        (Effect)
import Effect.Aff                    (Aff, Error)
import Effect.Class                  (class MonadEffect)
import Foreign.Generic.Class         (class Decode)
import Network.AWS.DynamoDB.Class    (class DynamoItem, class MonadGetItem,
                                      class MonadPutItem, class MonadRemoveItem,
                                      class MonadScanItems)
import Network.AWS.DynamoDB.Plumbing (delete, get, put, scan)
import Network.AWS.DynamoDB.Types    (DocumentClient)
import Type.Proxy                    (Proxy)

foreign import _documentClient :: forall r a. { | r } -> Effect (DocumentClient a)

-- |Creates a new `DocumentClient` object from the AWS object.
documentClient :: forall r a. Proxy a -> { | r } -> Effect (DocumentClient a)
documentClient = const _documentClient

newtype DynamoDB (n :: Symbol) (k :: Type) (i :: Type) a =
    DynamoDB (ReaderT (DocumentClient i) Aff a)

runDynamoDB :: forall n k i a. DynamoDB n k i a -> DocumentClient i -> Aff a
runDynamoDB = runReaderT <<< unwrap

derive         instance newtypeDynamoDB     :: Newtype (DynamoDB n k i a) _
derive newtype instance functorDynamoDB     :: Functor (DynamoDB n k i)
derive newtype instance applyDynamoDB       :: Apply (DynamoDB n k i)
derive newtype instance applicativeDynamoDB :: Applicative (DynamoDB n k i)
derive newtype instance altDynamoDB         :: Alt (DynamoDB n k i)
derive newtype instance plusDynamoDB        :: Plus (DynamoDB n k i)
derive newtype instance bindDynamoDB        :: Bind (DynamoDB n k i)
derive newtype instance monadDynamoDB       :: Monad (DynamoDB n k i)
derive newtype instance semigroupDynamoDB   :: Semigroup a => Semigroup (DynamoDB n k i a)
derive newtype instance monoidDynamoDB      :: Monoid a => Monoid (DynamoDB n k i a)
derive newtype instance monadEffectDynamoDB :: MonadEffect (DynamoDB n k i)
derive newtype instance monadThrowDynamoDB  :: MonadThrow Error (DynamoDB n k i)
derive newtype instance monadErrorDynamoDB  :: MonadError Error (DynamoDB n k i)
derive newtype instance monadAskDynamoDB    :: MonadAsk (DocumentClient i) (DynamoDB n k i)
derive newtype instance monadReaderDynamoDB :: MonadReader (DocumentClient i) (DynamoDB n k i)
derive newtype instance monadRecDynamoDB    :: MonadRec (DynamoDB n k i)


instance monadGetItemDynamoDB :: (IsSymbol n, Decode i, DynamoItem n k i) => MonadGetItem k i (DynamoDB n k i) where
    getItem key = DynamoDB $ ReaderT $ flip get
        { "TableName": reflectSymbol (SProxy :: SProxy n)
        , "Key": key
        }

instance moandPutItemDynamoDB :: (IsSymbol n, DynamoItem n k i) => MonadPutItem i (DynamoDB n k i) where
    putItem item = DynamoDB $ ReaderT $ flip put
        { "TableName": reflectSymbol (SProxy :: SProxy n)
        , "Item": item
        }

instance moandRemoveItemDynamoDB :: (IsSymbol n, DynamoItem n k i) => MonadRemoveItem k (DynamoDB n k i) where
    removeItem key = DynamoDB $ ReaderT $ flip delete
        { "TableName": reflectSymbol (SProxy :: SProxy n)
        , "Key": key
        }

instance moandScanItemsDynamoDB :: (IsSymbol n, Decode i, DynamoItem n k i) => MonadScanItems i (DynamoDB n k i) where
    scanItems = DynamoDB $ ReaderT $ flip scan
        { "TableName": reflectSymbol (SProxy :: SProxy n)
        }
