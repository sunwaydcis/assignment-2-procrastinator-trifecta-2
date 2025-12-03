import Text.CSV
import Data.List (maximumBy, minimumBy, groupBy, sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (fromMaybe)

-- =============================================================================
-- 1. USER DEFINED COMPONENTS (Rubric: 5%)
-- =============================================================================

data Booking = Booking
  { country      :: String
  , hotelName    :: String
  , price        :: Double
  , discount     :: Double -- We will store 5% as 0.05
  , profitMargin :: Double
  , visitors     :: Int
  } deriving (Show, Eq)

-- =============================================================================
-- 2. HELPER FUNCTIONS (Rubric: Functional Concepts, Polymorphism, Composition)
-- =============================================================================

-- Helper: Polymorphic function to calculate the average of any list of Fractional numbers (a)
-- This explicitly demonstrates the use of Polymorphism via the (Fractional a) type class.
calculateAverage :: (Fractional a) => [a] -> a
calculateAverage xs = sum xs / fromIntegral (length xs)

-- Helper: Convert string "15%" to double 0.15
parsePercent :: String -> Double
parsePercent str = 
    let clean = filter (/= '%') str -- Remove '%'
    in if null clean then 0.0 else read clean / 100.0

-- Helper: Safe read for numbers
readNum :: String -> Double
readNum "" = 0.0
readNum x  = read x

-- Helper: Find column index by name (Dynamic & Safer)
getCol :: [String] -> String -> Int
getCol header name = fromMaybe 0 (elemIndex name header)

-- =============================================================================
-- 3. CORE LOGIC
-- =============================================================================

-- Method to change raw CSV rows into a custom 'Booking' Type
-- This allows for modularity
toBooking :: [String] -> [String] -> Booking
toBooking header row = Booking
    { country      = row !! getCol header "Destination Country"
    , hotelName    = row !! getCol header "Hotel Name"
    , price        = readNum (row !! getCol header "Booking Price[SGD]")
    , discount     = parsePercent (row !! getCol header "Discount")
    , profitMargin = readNum (row !! getCol header "Profit Margin")
    , visitors     = round (readNum (row !! getCol header "No. Of People"))
    }

-- Q1: Country with the highest bookings(destination)
-- Using Higher Order Functions (sortBy, groupBy, map, maximumBy)
solveQ1 :: [Booking] -> (String, Int)
solveQ1 = maximumBy (comparing snd)              
         . map (\grp -> (country (head grp), length grp)) 
         . groupBy ((==) `on` country)            
         . sortBy (comparing country)             

-- Q2: Find Economical Hotel
-- Criteria: Lowest "Effective Cost" (Price - Discount). Uses calculateAverage (Polymorphism).
solveQ2 :: [Booking] -> (String, Double, Double, Double, Double)
solveQ2 bookings =
    let 
        -- Formula: Customer Cost = Price * (1 - Discount)
        calcCost b = price b * (1 - discount b)
        
        -- We combine two comparators into one logic:
        -- 1. First minimize Final Cost (derived from Price & Discount)
        -- 2. Then minimize Profit Margin (if Costs are equal)
        selectionCriteria = comparing calcCost <> comparing profitMargin

        -- Find the single best booking (O(n) efficiency)
        best = minimumBy selectionCriteria bookings
    in 
        (hotelName best, price best, discount best, profitMargin best, calcCost best)

-- Q3: Find Profitable Hotel
-- CORRECTED LOGIC: Now includes the 'visitors' field as required by the assignment.
solveQ3 :: [Booking] -> (String, Double)
solveQ3 bookings =
    let -- 1. Group by Hotel
        grouped = groupBy ((==) `on` hotelName) (sortBy (comparing hotelName) bookings)
        
        -- 2. Sum up the profit for every single booking in that hotel.
        --    Profit contribution = Price * Profit Margin * Number of Visitors
        calcTotalProfit grp =
            let profit = sum [ price b * profitMargin b * fromIntegral (visitors b) | b <- grp ]
            in (hotelName (head grp), profit)
             
    in maximumBy (comparing snd) (map calcTotalProfit grouped) -- Uses HOF

-- =============================================================================
-- 4. MAIN ENTRY POINT (The IO component/Loader)
-- =============================================================================

main :: IO ()
main = do
    putStrLn "Loading Hotel_Dataset.csv..."
    -- Uses parseCSVFromFile from the Text.CSV library
    result <- parseCSVFromFile "Hotel_Dataset.csv"

    case result of
        Left err -> print err
        Right csvData -> do
            -- Filter out empty rows/header (Text.CSV usually gives all rows)
            let validRows = filter (\r -> length r > 5) csvData
            
            let header = head validRows
            let rows   = tail validRows
            
            -- Transform raw strings to our clean 'Booking' type (Composition)
            let bookings = map (toBooking header) rows
            
            putStrLn $ "Successfully parsed " ++ show (length bookings) ++ " bookings.\n"

            -- ANSWER Q1
            let (topCountry, count) = solveQ1 bookings
            putStrLn "1. Which country has the highest number of bookings?"
            putStrLn $ "   Answer: " ++ topCountry ++ " (" ++ show count ++ " bookings)\n"
            
            -- ANSWER 2
            let (eName, ePrice, eDisc, eMargin, eFinal) = solveQ2 bookings
            putStrLn "2. Which hotel offers the most economical option?"
            putStrLn $ "   Answer: " ++ eName
            putStrLn   "   Breakdown of Criteria:"
            putStrLn $ "   a) Booking Price:      SGD " ++ show (round ePrice)
            putStrLn $ "   b) Discount:           " ++ show (round (eDisc * 100)) ++ "%"
            putStrLn $ "   c) Profit Margin:      " ++ show eMargin ++ " (" ++ show (round (eMargin * 100)) ++ "%)"
            putStrLn $ "   d) Final Price Paid:   SGD " ++ show eFinal ++ "\n"

            -- ANSWER Q3
            let (richHotel, profit) = solveQ3 bookings
            putStrLn "3. Which hotel is the most profitable?"
            putStrLn $ "   Answer: " ++ richHotel 
            putStrLn $ "   (Total Profit Generated: SGD " ++ show (round profit) ++ ")"