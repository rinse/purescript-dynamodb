module Test.Network.AWS.DynamoDB.PlumbingSpec (spec) where

import Prelude

import Control.Monad.Error.Class     (catchError)
import Data.Maybe                    (Maybe)
import Effect                        (Effect)
import Effect.Aff                    (Aff)
import Effect.Class                  (liftEffect)
import Effect.Exception              (message)
import Network.AWS.DynamoDB          (documentClient)
import Network.AWS.DynamoDB.Plumbing (delete, get, put, scan)
import Network.AWS.DynamoDB.Types    (DocumentClient)
import Test.Spec                     (Spec, describe, it)
import Test.Spec.Assertions          (shouldEqual)


-- A documentClient with fake credentials.
documentClient' :: Effect DocumentClient
documentClient' = documentClient
    { accessKeyId: "accessKeyId"
    , secretAccessKey: "secretAccessKey"
    }

-- A type of an item.
type Person = { name :: String, age :: Int }

verifyMissingRegion :: Aff Unit -> Aff Unit
verifyMissingRegion action =
    catchError action $ message >>> shouldEqual "Missing region in config"

spec :: Spec Unit
spec = do
    describe "get" $
        it "is bound to `AWS.DocumentClient.get`" $ do
            client <- liftEffect documentClient'
            verifyMissingRegion $
                void $ (get client { "TableName": "", "Key": {} } :: Aff (Maybe Person))

    describe "delete" $
        it "is bound to `AWS.DocumentClient.delete`" $ do
            client <- liftEffect documentClient'
            verifyMissingRegion $
                delete client { "TableName": "", "Key": {} }

    describe "put" $
        it "is bound to `AWS.DocumentClient.put`" $ do
            client <- liftEffect documentClient'
            verifyMissingRegion $
                put client { "TableName": "", "Item": {} }

    describe "scan" $
        it "is bound to `AWS.DocumentClient.scan`" $ do
            client <- liftEffect documentClient'
            verifyMissingRegion $
                void $ (scan client { "TableName": "" } :: Aff (Array Person))
