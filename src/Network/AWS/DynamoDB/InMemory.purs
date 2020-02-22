module Network.AWS.DynamoDB.InMemory where

import Prelude

import Control.Alt                (class Alt)
import Control.Alternative        (class Alternative)
import Control.Lazy               (class Lazy)
import Control.Monad.Cont.Class   (class MonadCont)
import Control.Monad.Error.Class  (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class    (class MonadRec)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class  (class MonadState)
import Control.Monad.State.Trans  (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class  (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus          (class MonadPlus)
import Control.MonadZero          (class MonadZero)
import Control.Plus               (class Plus)
import Data.Array                 (fromFoldable)
import Data.Identity              (Identity)
import Data.Newtype               (class Newtype, unwrap)
import Data.Map                   (Map, delete, insert, lookup, values)
import Data.Tuple                 (Tuple)
import Effect.Class               (class MonadEffect)
import Network.AWS.DynamoDB.Class (class DynamoKeyItem, getKey,
                                   class MonadGetItem, class MonadPutItem,
                                   class MonadRemoveItem, class MonadScanItems)


newtype DynamoDBInMemoryT k i m a = DynamoDBInMemoryT (StateT (Map k i) m a)

runDynamoDBInMemoryT :: forall k i m a. Monad m => DynamoDBInMemoryT k i m a -> Map k i -> m (Tuple a (Map k i))
runDynamoDBInMemoryT = runStateT <<< unwrap

type DynamoDBInMemory k i a = DynamoDBInMemoryT k i Identity a

runDynamoDBInMemory :: forall k i a. DynamoDBInMemory k i a -> Map k i -> Tuple a (Map k i)
runDynamoDBInMemory = map unwrap <<< runDynamoDBInMemoryT

derive         instance newtypeDynamoDBInMemoryT     :: Newtype (DynamoDBInMemoryT k i m a) _
derive newtype instance functorDynamoDBInMemoryT     :: Functor m => Functor (DynamoDBInMemoryT k i m)
derive newtype instance applyDynamoDBInMemoryT       :: Monad m => Apply (DynamoDBInMemoryT k i m)
derive newtype instance applicativeDynamoDBInMemoryT :: Monad m => Applicative (DynamoDBInMemoryT k i m)
derive newtype instance altDynamoDBInMemoryT         :: (Monad m, Alt m) => Alt (DynamoDBInMemoryT k i m)
derive newtype instance plusDynamoDBInMemoryT        :: (Monad m, Plus m) => Plus (DynamoDBInMemoryT k i m)
derive newtype instance bindDynamoDBInMemoryT        :: Monad m => Bind (DynamoDBInMemoryT k i m)
derive newtype instance monadDynamoDBInMemoryT       :: Monad m => Monad (DynamoDBInMemoryT k i m)
derive newtype instance monadRecDynamoDBInMemoryT    :: MonadRec m => MonadRec (DynamoDBInMemoryT k i m)
derive newtype instance alternativeDynamoDBInMemoryT :: (Monad m, Alternative m) => Alternative (DynamoDBInMemoryT k i m)
derive newtype instance monadZeroDynamoDBInMemoryT   :: MonadZero m => MonadZero (DynamoDBInMemoryT k i m)
derive newtype instance monadPlusDynamoDBInMemoryT   :: MonadPlus m => MonadPlus (DynamoDBInMemoryT k i m)
derive newtype instance monadTransDynamoDBInMemoryT  :: MonadTrans (DynamoDBInMemoryT k i)
derive newtype instance lazyDynamoDBInMemoryT        :: Lazy (DynamoDBInMemoryT k i m a)
derive newtype instance monadEffectDynamoDBInMemoryT :: MonadEffect m => MonadEffect (DynamoDBInMemoryT k i m)
derive newtype instance monadContDynamoDBInMemoryT   :: MonadCont m => MonadCont (DynamoDBInMemoryT k i m)
derive newtype instance monadThrowDynamoDBInMemoryT  :: MonadThrow e m => MonadThrow e (DynamoDBInMemoryT k i m)
derive newtype instance monadErrorDynamoDBInMemoryT  :: MonadError e m => MonadError e (DynamoDBInMemoryT k i m)
derive newtype instance monadAskDynamoDBInMemoryT    :: MonadAsk r m => MonadAsk r (DynamoDBInMemoryT k i m)
derive newtype instance monadReaderDynamoDBInMemoryT :: MonadReader r m => MonadReader r (DynamoDBInMemoryT k i m)
derive newtype instance monadStateDynamoDBInMemoryT  :: Monad m => MonadState (Map k i) (DynamoDBInMemoryT k i m)
derive newtype instance monadTellDynamoDBInMemoryT   :: MonadTell w m => MonadTell w (DynamoDBInMemoryT k i m)
derive newtype instance monadWriterDynamoDBInMemoryT :: MonadWriter w m => MonadWriter w (DynamoDBInMemoryT k i m)

instance monadGetItemDynamoDBInMemoryT :: (Ord k, Monad m) => MonadGetItem k i (DynamoDBInMemoryT k i m) where
    getItem key = DynamoDBInMemoryT $ lookup key <$> get

instance monadPutItemDynamoDBInMemoryT :: (DynamoKeyItem k i, Ord k, Monad m) => MonadPutItem i (DynamoDBInMemoryT k i m) where
    putItem item = DynamoDBInMemoryT $ modify_ $ (getKey >>= insert) item

instance monadRemoveItemDynamoDBInMemoryT :: (Ord k, Monad m) => MonadRemoveItem k (DynamoDBInMemoryT k i m) where
    removeItem key = DynamoDBInMemoryT $ modify_ $ delete key

instance monadScanItemsDynamoDBInMemoryT :: (Ord k, Monad m) => MonadScanItems i (DynamoDBInMemoryT k i m) where
    scanItems = DynamoDBInMemoryT $ fromFoldable <<< values <$> get
