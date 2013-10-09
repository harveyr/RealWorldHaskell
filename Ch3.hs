
-- Defining new data type.
-- ActorInfo is "type constructor."
-- Actor is "value constructor."
-- Subsequent types are "components" of the type.
data ActorInfo = Actor Int String [String]
                 deriving (Show)

billyZane = Actor 1 "Billy Zane"
            ["Younger brother of actress Lisa Zane.",
             "Is an avid swimmer and started shaving his head in 1997 for aerodynamics."]

-- Type synonyms. Solely for more readable code.
type StalkerId = Int
type Name = String

data ActorStalker = Stalker StalkerId Name ActorInfo

-- More complex synonym
type Timestamp = Int
type Incident = (ActorStalker, Timestamp)  -- Synonym for tuple


-------------------------------------------------------------------------------
-- "Algebraic data types" can have more than one value constructor.
-- E.g., Bools:
data Bool = False | True

-- ... each value constructor can take 0 or more arguments.
-- CreditCard, CashOnDelivery, and Invoice (below) are value constructors
type CardHolder = String
type CardNumber = String
type CustomerID = Int
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- Using the type system instead of an equivalent tuple prevents ambiguity
-- and bugs. E.g., Cartesian2D vs. Polar2D. Both are Double, Double.

-- From the book: If you're using compound values widely in your code (as almost all non-trivial programs do), adding data declarations will benefit you in both type safety and readability. For smaller, localised uses, a tuple is usually fine.


-------------------------------------------------------------------------------
-- Haskell version of enumeration
data SoManyColors = Red
                  | Blue
                  | Purplee
                  | Greenish
                  | SageLike
                  | ToiletWhite
                    deriving (Eq, Show)



-------------------------------------------------------------------------------
-- Pattern matching
sumList (x:xs) = x + sumList xs
sumList []     = 0
-- ':' is the "cons" list constructor: http://book.realworldhaskell.org/read/getting-started.html#starting.list.op
-- Note that the standard function "sum" already does this


main = do
    putStrLn "Compiled!"
    print billyZane
    print (sumList [1, 2, 3, 4])
