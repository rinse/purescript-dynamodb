module Test.Network.AWS.DynamoDBSpec  where

import Prelude

import Control.Monad.Error.Class  (catchError)
import Effect                     (Effect)
import Effect.Aff                 (Aff)
import Effect.Class               (liftEffect)
import Effect.Exception           (message)
import Data.Symbol                (class IsSymbol, SProxy(..))
import Foreign.Generic.Class      (class Decode)
import Network.AWS.DynamoDB       (documentClient, runDynamoDB)
import Network.AWS.DynamoDB.Class (class DynamoItem, getItem, putItem, removeItem, scanItems)
import Network.AWS.DynamoDB.Types (DocumentClient)
import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)


-- A documentClient with fake credentials.
documentClient' :: Effect DocumentClient
documentClient' = documentClient
    { accessKeyId: "accessKeyId"
    , secretAccessKey: "secretAccessKey"
    }

-- A name of a table.
foreign import data TablePerson :: Symbol
instance isSymbolPersonTable :: IsSymbol TablePerson where
    reflectSymbol _ = "sample"

tablePerson :: SProxy TablePerson
tablePerson = SProxy

-- A type of an item.
newtype Person = Person { name :: String, age :: Int }
derive newtype instance decodePerson :: Decode Person
derive newtype instance showPerson :: Show Person
derive newtype instance eqPerson :: Eq Person

-- A type of the key of an item.
newtype PrimaryKeyPerson = PrimaryKeyPerson { name :: String }
derive newtype instance decodePrimaryKeyPerson :: Decode PrimaryKeyPerson

instance dynamoItemPerson :: DynamoItem TablePerson PrimaryKeyPerson Person

verifyMissingRegion :: Aff Unit -> Aff Unit
verifyMissingRegion action =
    catchError action $ message >>> shouldEqual "Missing region in config"


spec :: Spec Unit
spec = do
    describe "documentClient" $
        it "is bound to `new AWS.DocumentClient()`" $
            void $ liftEffect $ documentClient {}

    describe "runDynamoDB" $
        it "runs `DynamoDB` and get a result" $ do
            client <- liftEffect documentClient'
            runDynamoDB tablePerson client (pure unit)

    describe "DynamoDB" $ do
        describe "getItem" $
            it "is bound to `AWS.DocumentClient.get`" $ do
                client <- liftEffect documentClient'
                verifyMissingRegion $
                    void $ runDynamoDB tablePerson client $
                        getItem $ PrimaryKeyPerson { name: "John" }
        describe "putItem" $
            it "is bound to `AWS.DocumentClient.put`" $ do
                client <- liftEffect documentClient'
                verifyMissingRegion $
                    void $ runDynamoDB tablePerson client $
                        putItem $ Person { name: "John", age: 20 }
        describe "removeItem" $
            it "is bound to `AWS.DocumentClient.remove`" $ do
                client <- liftEffect documentClient'
                verifyMissingRegion $
                    void $ runDynamoDB tablePerson client $
                        removeItem $ PrimaryKeyPerson { name: "John" }
        describe "scanItems" $
            it "is bound to `AWS.DocumentClient.scan`" $ do
                client <- liftEffect documentClient'
                verifyMissingRegion $
                    void $ runDynamoDB tablePerson client $
                        scanItems
