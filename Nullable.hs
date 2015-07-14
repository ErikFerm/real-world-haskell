import Prelude hiding (Maybe, Just, Nothing)
data Maybe a    = Just a
                | Nothing
                    deriving (Show)

someBool    = Just True
someString  = Just "Something"
wrapped = Just (Just "wrapped")