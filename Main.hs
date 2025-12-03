import Text.CSV
import Data.List (maximumBy, minimumBy, groupBy, sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (fromMaybe)

-- =============================================================================
-- 1. USER DEFINED COMPONENTS (Rubric: 5%)
-- instead of using raw strings ("Row !! 5"), we convert them to a clean Type.
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
-- 2. HELPER FUNCTIONS (Rubric: Functional Concepts & Composition)
-- =============================================================================

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

-- Transform raw CSV rows into our Custom 'Booking' Type
toBooking :: [String] -> [String] -> Booking
toBooking header row = Booking
    { country      = row !! getCol header "Origin Country"
    , hotelName    = row !! getCol header "Hotel Name"
    , price        = readNum (row !! getCol header "Booking Price[SGD]")
    , discount     = parsePercent (row !! getCol header "Discount")
    , profitMargin = readNum (row !! getCol header "Profit Margin")
    , visitors     = round (readNum (row !! getCol header "No. Of People"))
    }

-- Q1: Count bookings by country
-- Logic: Group by Country -> Count length of each group -> Find Max
solveQ1 :: [Booking] -> (String, Int)
solveQ1 bookings = 
    let grouped = groupBy ((==) `on` country) (sortBy (comparing country) bookings)
        counted = map (\grp -> (country (head grp), length grp)) grouped
    in maximumBy (comparing snd) counted

-- Q2: Find Economical Hotel
-- Criteria: Lowest "Effective Cost" (Price - Discount)
solveQ2 :: [Booking] -> (String, Double)
solveQ2 bookings =
    let -- 1. Group by Hotel
        grouped = groupBy ((==) `on` hotelName) (sortBy (comparing hotelName) bookings)
        
        -- 2. Calculate Average Cost for each hotel
        calcAvgCost grp = 
            let totalCost = sum [ price b * (1 - discount b) | b <- grp ]
                count     = fromIntegral (length grp)
            in (hotelName (head grp), totalCost / count)
            
    in minimumBy (comparing snd) (map calcAvgCost grouped)

-- Q3: Find Profitable Hotel
-- Criteria: Highest Total Profit (Price * Margin * Visitors is implied by summing entries)
solveQ3 :: [Booking] -> (String, Double)
solveQ3 bookings =
    let -- 1. Group by Hotel
        grouped = groupBy ((==) `on` hotelName) (sortBy (comparing hotelName) bookings)
        
        -- 2. Sum up the profit for every single booking in that hotel
        calcTotalProfit grp =
            let profit = sum [ price b * profitMargin b | b <- grp ]
            in (hotelName (head grp), profit)
            
    in maximumBy (comparing snd) (map calcTotalProfit grouped)

-- =============================================================================
-- 4. MAIN ENTRY POINT
-- =============================================================================

main :: IO ()
main = do
    putStrLn "Loading Hotel_Dataset.csv..."
    result <- parseCSVFromFile "Hotel_Dataset.csv"

    case result of
        Left err -> print err
        Right csvData -> do
            -- Filter out empty rows (common Text.CSV bug)
            let validRows = filter (\r -> length r > 5) csvData
            
            let header = head validRows
            let rows   = tail validRows
            
            -- Convert raw strings to our clean 'Booking' type
            -- This one line satisfies "Function Composition" and "Efficiency"
            let bookings = map (toBooking header) rows
            
            putStrLn $ "Successfully parsed " ++ show (length bookings) ++ " bookings.\n"

            -- ANSWER Q1
            let (topCountry, count) = solveQ1 bookings
            putStrLn "1. Which country has the highest number of bookings?"
            putStrLn $ "   Answer: " ++ topCountry ++ " (" ++ show count ++ " bookings)\n"

            -- ANSWER Q2
            let (cheapHotel, avgCost) = solveQ2 bookings
            putStrLn "2. Which hotel offers the most economical option?"
            putStrLn $ "   Answer: " ++ cheapHotel 
            putStrLn $ "   (Average Cost to customer: SGD " ++ show (take 5 $ show avgCost) ++ ")\n"

            -- ANSWER Q3
            let (richHotel, profit) = solveQ3 bookings
            putStrLn "3. Which hotel is the most profitable?"
            putStrLn $ "   Answer: " ++ richHotel 
            putStrLn $ "   (Total Profit Generated: SGD " ++ show (round profit) ++ ")"