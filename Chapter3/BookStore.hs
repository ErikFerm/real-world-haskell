data BookInfo       = Book Int String [String]
                        deriving (Show) 
data MagazineInfo   = Magazine Int String [String]
                        deriving (Show)
data BookReview     = BookReview BookInfo CustomerID String
                        deriving (Show)
data BetterReview   = BetterReview BookInfo CustomerID ReviewBody
                        deriving (Show)
data BillingInfo    = CreditCard CardNumber CardHolder Address
                    | CashOnDelivery
                    | Invoice CustomerID
                        deriving(Show)

data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address 
    } 
    deriving (Show)

type CardHolder = String
type CardNumber = String
type Address    = [String]
type ReviewBody = String
type CustomerID = String
type Authors    = [String]

myInfo =    Book 1234 "Testing RealWorld Haskell"
            ["Me", "Myself"]


bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors
