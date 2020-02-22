module Network.AWS.DynamoDB.Plumbing
    ( get
    , delete
    , put
    , scan
    ) where

import Prelude

import Control.Monad.Error.Class  (class MonadThrow)
import Control.Monad.Except       (runExcept)
import Control.Promise            (Promise, toAffE)
import Data.Profunctor.Choice     (left, (|||))
import Data.Foldable              (fold)
import Data.List.Types            (toList)
import Data.Maybe                 (Maybe)
import Effect.Aff                 (Aff, Error, error, throwError)
import Effect.Uncurried           (EffectFn2, runEffectFn2)
import Foreign                    (Foreign, MultipleErrors, renderForeignError)
import Foreign.Generic.Class      (class Decode, decode)
import Network.AWS.DynamoDB.Types (DocumentClient)

foreign import _get    :: forall r. EffectFn2 DocumentClient { | r } (Promise Foreign)
foreign import _delete :: forall r. EffectFn2 DocumentClient { | r } (Promise Unit)
foreign import _put    :: forall r. EffectFn2 DocumentClient { | r } (Promise Unit)
foreign import _scan   :: forall r. EffectFn2 DocumentClient { | r } (Promise Foreign)


get :: forall r a. Decode a => DocumentClient -> { | r } -> Aff (Maybe a)
get c r = (_."Item") <$> get' c r
    where
    get' :: DocumentClient -> { | r } -> Aff { "Item" :: Maybe a }
    get' d = toAffEffectFn2 _get d >=> decode'

delete :: forall r. DocumentClient -> { | r } -> Aff Unit
delete = toAffEffectFn2 _delete

put :: forall r. DocumentClient -> { | r } -> Aff Unit
put = toAffEffectFn2 _put

scan :: forall r a. Decode a => DocumentClient -> { | r } -> Aff (Array a)
scan c = toAffEffectFn2 _scan c >=> decode'

toAffEffectFn2 :: forall a b c. EffectFn2 a b (Promise c) -> a -> b -> Aff c
toAffEffectFn2 = (map <<< map) toAffE <<< runEffectFn2

decode' :: forall m a. MonadThrow Error m => Decode a => Foreign -> m a
decode' = (throwError ||| pure)
    <<< left (error <<< renderMultipleErrors)
    <<< runExcept
    <<< decode

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = fold <<< map renderForeignError <<< toList
