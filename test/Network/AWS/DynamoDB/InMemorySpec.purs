module Test.Network.AWS.DynamoDB.InMemorySpec where

import Prelude

import Data.Array                    (length)
import Data.Map                      (fromFoldable)
import Data.Maybe                    (Maybe (..))
import Data.Tuple                    (Tuple (..), fst, snd)
import Network.AWS.DynamoDB.Class    (class DynamoKeyItem, getItem, putItem, removeItem, scanItems)
import Network.AWS.DynamoDB.InMemory (runDynamoDBInMemory, runDynamoDBInMemoryT)
import Test.Spec                     (Spec, describe, it)
import Test.Spec.Assertions          (shouldEqual, shouldContain, shouldSatisfy)


-- A type of an item.
newtype Person = Person { name :: String, age :: Int }
derive newtype instance showPerson :: Show Person
derive newtype instance eqPerson :: Eq Person

-- A type of the key of an item.
newtype PrimaryKeyPerson = PrimaryKeyPerson { name :: String }
derive newtype instance showPrimaryKeyPerson :: Show PrimaryKeyPerson
derive newtype instance eqPrimaryKeyPerson :: Eq PrimaryKeyPerson
derive newtype instance ordPrimaryKeyPerson :: Ord PrimaryKeyPerson

instance dynamoKeyItemPerson :: DynamoKeyItem PrimaryKeyPerson Person where
    getKey (Person p) = PrimaryKeyPerson $ { name: p.name }


spec :: Spec Unit
spec = do
    let db = fromFoldable
            [ Tuple (PrimaryKeyPerson { name: "John" }) (Person { name: "John", age: 20 })
            , Tuple (PrimaryKeyPerson { name: "Ann" })  (Person { name: "Ann", age: 30 })
            ]

    describe "runDynamoDBInMemory" $
        it "runs DynamoDBInMemory" $
            runDynamoDBInMemory (pure unit) db `shouldEqual` Tuple unit db

    describe "DynamoDBInMemory" $ do
        describe "getItem" $
            it "gets an item of db" $ do
                result <- flip runDynamoDBInMemoryT db $
                    getItem $ PrimaryKeyPerson { name: "John" }
                fst result `shouldEqual` (Just $ Person { name: "John", age: 20 })
        describe "putItem" $
            it "puts an item on db" $ do
                result <- flip runDynamoDBInMemoryT db $
                    putItem $ Person { name: "George", age: 25 }
                snd result `shouldEqual` fromFoldable
                    [ Tuple (PrimaryKeyPerson { name: "John" })   (Person { name: "John", age: 20 })
                    , Tuple (PrimaryKeyPerson { name: "Ann" })  (Person { name: "Ann", age: 30 })
                    , Tuple (PrimaryKeyPerson { name: "George" }) (Person { name: "George", age: 25 })
                    ]
        describe "removeItem" $
            it "removes an item of db" $ do
                result <- flip runDynamoDBInMemoryT db $
                    removeItem $ PrimaryKeyPerson { name: "John" }
                snd result `shouldEqual` fromFoldable
                    [ Tuple (PrimaryKeyPerson { name: "Ann" })  (Person { name: "Ann", age: 30 })
                    ]
        describe "scanItems" $
            it "scans all items of db" $ do
                result <- runDynamoDBInMemoryT scanItems db
                fst result `shouldSatisfy` (eq 2 <<< length)
                fst result `shouldContain` Person { name: "Ann", age: 30 }
                fst result `shouldContain` Person { name: "John", age: 20 }
