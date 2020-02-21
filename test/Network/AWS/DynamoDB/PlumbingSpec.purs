module Test.Network.AWS.DynamoDB.PlumbingSpec (spec) where

import Prelude

import Control.Monad.Error.Class     (catchError)
import Effect                        (Effect)
import Effect.Aff                    (Aff)
import Effect.Class                  (liftEffect)
import Effect.Exception              (message)
import Network.AWS.DynamoDB          (documentClient)
import Network.AWS.DynamoDB.Types    (DocumentClient)
import Network.AWS.DynamoDB.Plumbing (delete, get, put, scan)
import Test.Spec                     (Spec, describe, it)
import Test.Spec.Assertions          (shouldEqual)
import Type.Proxy                    (Proxy(..))


-- A type of an item.
type Person = { name :: String, age :: Int }

verifyMissingRegion :: Aff Unit -> Aff Unit
verifyMissingRegion action =
    catchError action $ message >>> shouldEqual "Missing region in config"

spec :: Spec Unit
spec = do
    describe "get" $
        it "is bound to `AWS.DocumentClient.get`" $ do
            client <- liftEffect $ (documentClient {} :: Effect (DocumentClient Person))
            verifyMissingRegion $
                void $ get client { "TableName": "", "Key": {} }

    describe "delete" $
        it "is bound to `AWS.DocumentClient.delete`" $ do
            client <- liftEffect $ (documentClient {} :: Effect (DocumentClient Person))
            verifyMissingRegion $
                delete client { "TableName": "", "Key": {} }

    describe "put" $
        it "is bound to `AWS.DocumentClient.put`" $ do
            client <- liftEffect $ (documentClient {} :: Effect (DocumentClient Person))
            verifyMissingRegion $
                put client { "TableName": "", "Item": {} }

    describe "scan" $
        it "is bound to `AWS.DocumentClient.scan`" $ do
            client <- liftEffect $ (documentClient {} :: Effect (DocumentClient Person))
            verifyMissingRegion $
                void $ scan client { "TableName": "" }
