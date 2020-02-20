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

foreign import _get    :: forall r a. EffectFn2 (DocumentClient a) { | r } (Promise Foreign)
foreign import _delete :: forall r a. EffectFn2 (DocumentClient a) { | r } (Promise Unit)
foreign import _put    :: forall r a. EffectFn2 (DocumentClient a) { | r } (Promise Unit)
foreign import _scan   :: forall r a. EffectFn2 (DocumentClient a) { | r } (Promise Foreign)


get :: forall r a. Decode a => DocumentClient a -> { | r } -> Aff (Maybe a)
get c = toAffEffectFn2 _get c >=> decode'

delete :: forall r a. DocumentClient a -> { | r } -> Aff Unit
delete = toAffEffectFn2 _delete

put :: forall r a. DocumentClient a -> { | r } -> Aff Unit
put = toAffEffectFn2 _put

scan :: forall r a. Decode a => DocumentClient a -> { | r } -> Aff (Array a)
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
