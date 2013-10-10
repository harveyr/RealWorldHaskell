
-- Defining new data type
-- ActorInfo is "type constructor"
-- Actor is "value constructor"
-- Subsequent types are "components" of the type
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
--data Bool = False | True

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

third (a, b, c) = c

complicated (True, a, x:xs, 5) = (a, xs)
-- (True must match exactly in this one)

stalkerName (Stalker id name actor) = name

-- wild card
stalkerId (Stalker id _ _) = id

-- wild card as default case
summySum (x:xs) = x + summySum xs
summySum _      = 0 


-------------------------------------------------------------------------------
-- Record syntax. Down with boilerplate.

data AltStalker = AltStalker {
      altStalkerName     :: String
    , altStalkerHabits   :: [String]
} deriving (Show)

johnnyLurker = AltStalker "Johnny Lurker" ["Lurking"]

-- verbose creation
billyPeeper = AltStalker {
      altStalkerName = "Billy Peeper"
    , altStalkerHabits = ["Peeping"]
}


-------------------------------------------------------------------------------
-- Error reporting

maybeSecond :: [a] -> Maybe a
-- matches only if list is at least two elements long:
maybeSecond (_:x:_) = Just x
maybeSecond _       = Nothing


-------------------------------------------------------------------------------
-- Local variables

hugeOnion diameter = let hugeThreshold = 25
                     in if diameter < hugeThreshold
                        then False
                        else True

-- where clause
possiblePizza size toppings = if (size * toppings) < materialsOnHand
                              then True
                              else False
     where materialsOnHand  = 100


-- local function referencing variable from enclosing scope
pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts
       where plural 0 = "no " ++ word ++ "s"
             plural 1 = "one " ++ word
             plural n = show n ++ " " ++ word ++ "s"



-------------------------------------------------------------------------------
-- Guards

hugeOnionGuard diameter
    | diameter < hugeThreshold = False
    | otherwise                = True
    where hugeThreshold = 25

main = do
    putStrLn "Compiled!"
    print billyZane
    putStrLn "---"
    
    print (sumList [1, 2, 3, 4])
    print (third (1, 2, 3))
    putStrLn "---"

    print johnnyLurker
    print (altStalkerHabits johnnyLurker)
    print (altStalkerHabits billyPeeper)
    putStrLn "---"
    

    print(hugeOnion 20)
    print(hugeOnion 30)
    putStrLn "---"

    print(possiblePizza 10 5)
    print(possiblePizza 18 10)
    putStrLn "---"

    print(pluralize "bingo" [1, 2, 3])
    putStrLn "---"

    putStrLn "hugeOnionGuard:"    
    print(hugeOnionGuard 15)
    print(hugeOnionGuard 25)
    putStrLn "---"
