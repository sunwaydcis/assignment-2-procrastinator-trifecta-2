{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Map.Strict as M

-- Define the hotel booking record structure
data HotelBooking = HotelBooking
  { bookingId :: Text
  , dateOfBooking :: Text
  , time :: Text
  , customerId :: Text
  , gender :: Text
  , age :: Int
  , originCountry :: Text
  , state :: Text
  , location :: Text
  , destinationCountry :: Text
  , destinationCity :: Text
  , numOfPeople :: Int
  , checkInDate :: Text
  , numOfDays :: Int
  , checkOutDate :: Text
  , rooms :: Int
  , hotelName :: Text
  , hotelRating :: Double
  , paymentMode :: Text
  , bankName :: Text
  , bookingPrice :: Double
  , discount :: Double  -- Parsed as decimal (e.g., 1% -> 0.01)
  , gst :: Double
  , profitMargin :: Double
  } deriving (Show, Eq)

-- CSV parsing instance
instance FromNamedRecord HotelBooking where
  parseNamedRecord r = HotelBooking
    <$> r .: "Booking ID"
    <*> r .: "Date of Booking"
    <*> r .: "Time"
    <*> r .: "Customer ID"
    <*> r .: "Gender"
    <*> r .: "Age"
    <*> r .: "Origin Country"
    <*> r .: "State"
    <*> r .: "Location"
    <*> r .: "Destination Country"
    <*> r .: "Destination City"
    <*> r .: "No. Of People"
    <*> r .: "Check-in date"
    <*> r .: "No of Days"
    <*> r .: "Check-Out Date"
    <*> r .: "Rooms"
    <*> r .: "Hotel Name"
    <*> r .: "Hotel Rating"
    <*> r .: "Payment Mode"
    <*> r .: "Bank Name"
    <*> r .: "Booking Price[SGD]"
    <*> (parseDiscount <$> r .: "Discount")
    <*> r .: "GST"
    <*> r .: "Profit Margin"

-- Parse discount percentage string (e.g., "1%" -> 0.01, "19%" -> 0.19)
parseDiscount :: Text -> Double
parseDiscount txt = 
  let cleaned = T.strip txt
      withoutPercent = T.dropWhileEnd (== '%') cleaned
  in case T.uncons withoutPercent of
    Just (first, rest) | T.all (`elem` ['0'..'9']) (T.cons first rest) ->
      read (T.unpack withoutPercent) / 100.0
    _ -> 0.0

-- Question 1: Which country has the highest number of bookings?
findCountryWithHighestBookings :: V.Vector HotelBooking -> (Text, Int)
findCountryWithHighestBookings bookings =
  let countryCounts = V.foldl' (\acc booking -> 
        M.insertWith (+) (originCountry booking) 1 acc) M.empty bookings
  in M.foldlWithKey (\best@(_, bestCount) country count ->
        if count > bestCount then (country, count) else best)
      ("", 0) countryCounts

-- Question 2: Which hotel offers the most economical option?
-- Based on: Booking Price, Discount, Profit Margin
-- Lower price, higher discount, lower profit margin = more economical
data HotelEconomics = HotelEconomics
  { hotelName' :: Text
  , avgBookingPrice :: Double
  , avgDiscount :: Double
  , avgProfitMargin :: Double
  , bookingCount :: Int
  } deriving (Show)

calculateHotelEconomics :: V.Vector HotelBooking -> M.Map Text HotelEconomics
calculateHotelEconomics bookings =
  let grouped = V.foldl' (\acc booking ->
        let name = hotelName booking
            price = bookingPrice booking
            disc = discount booking
            margin = profitMargin booking
        in M.insertWith (\_ old -> HotelEconomics
          { hotelName' = hotelName' old
          , avgBookingPrice = (avgBookingPrice old * fromIntegral (bookingCount old) + price) / fromIntegral (bookingCount old + 1)
          , avgDiscount = (avgDiscount old * fromIntegral (bookingCount old) + disc) / fromIntegral (bookingCount old + 1)
          , avgProfitMargin = (avgProfitMargin old * fromIntegral (bookingCount old) + margin) / fromIntegral (bookingCount old + 1)
          , bookingCount = bookingCount old + 1
          }) name (HotelEconomics name price disc margin 1) acc
        ) M.empty bookings
  in grouped

-- Calculate economical score: lower is better (lower price, higher discount, lower margin)
economicalScore :: HotelEconomics -> Double
economicalScore HotelEconomics{..} =
  -- Normalize: lower price is better, higher discount is better, lower margin is better
  -- We'll use: price - (discount * price) - (margin * price)
  -- Actually, let's use: price * (1 - discount) * (1 - margin) as the effective cost
  avgBookingPrice * (1 - avgDiscount) * (1 - avgProfitMargin)

findMostEconomicalHotel :: V.Vector HotelBooking -> (Text, HotelEconomics)
findMostEconomicalHotel bookings =
  let economics = calculateHotelEconomics bookings
      scored = M.map (\e -> (economicalScore e, e)) economics
  in case M.minViewWithKey scored of
    Just ((name, (_, econ)), _) -> (name, econ)
    Nothing -> ("", HotelEconomics "" 0 0 0 0)

-- Question 3: Which hotel is the most profitable?
-- Considering: number of visitors and profit margin
data HotelProfitability = HotelProfitability
  { hotelName'' :: Text
  , totalVisitors :: Int
  , avgProfitMargin' :: Double
  , totalProfit :: Double  -- Approximate: sum of (price * profitMargin) for all bookings
  } deriving (Show)

calculateHotelProfitability :: V.Vector HotelBooking -> M.Map Text HotelProfitability
calculateHotelProfitability bookings =
  let grouped = V.foldl' (\acc booking ->
        let name = hotelName booking
            visitors = numOfPeople booking
            margin = profitMargin booking
            profit = bookingPrice booking * margin
        in M.insertWith (\_ old -> HotelProfitability
          { hotelName'' = hotelName'' old
          , totalVisitors = totalVisitors old + visitors
          , avgProfitMargin' = (avgProfitMargin' old * fromIntegral (totalVisitors old) + margin * fromIntegral visitors) / fromIntegral (totalVisitors old + visitors)
          , totalProfit = totalProfit old + profit
          }) name (HotelProfitability name visitors margin profit) acc
        ) M.empty bookings
  in grouped

-- Profitability score: higher is better
-- We'll use: totalProfit * avgProfitMargin (or just totalProfit)
profitabilityScore :: HotelProfitability -> Double
profitabilityScore HotelProfitability{..} =
  totalProfit * avgProfitMargin'  -- Or just totalProfit

findMostProfitableHotel :: V.Vector HotelBooking -> (Text, HotelProfitability)
findMostProfitableHotel bookings =
  let profitability = calculateHotelProfitability bookings
      scored = M.map (\p -> (profitabilityScore p, p)) profitability
  in case M.foldlWithKey (\best@(_, (bestScore, _)) name (score, prof) ->
        if score > bestScore then (name, (score, prof)) else best)
      ("", (0.0, HotelProfitability "" 0 0 0)) scored of
    (name, (_, prof)) -> (name, prof)

-- Main function
main :: IO ()
main = do
  -- Read CSV file and handle encoding issues
  csvData <- BL.readFile "Hotel_Dataset.csv"
  -- Clean invalid UTF-8 sequences by decoding with lenient error handling and re-encoding
  let cleanedData = BL.fromStrict $ TE.encodeUtf8 $ 
        TE.decodeUtf8With TEE.lenientDecode $ BL.toStrict csvData
  
  case decodeByName cleanedData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right (_, bookings) -> do
      putStrLn "=== Hotel Dataset Analysis ===\n"
      
      -- Question 1: Country with highest bookings
      let (country, count) = findCountryWithHighestBookings bookings
      putStrLn "Question 1: Which country has the highest number of bookings?"
      putStrLn $ "Answer: " ++ T.unpack country ++ " with " ++ show count ++ " bookings\n"
      
      -- Question 2: Most economical hotel
      let (econHotel, econData) = findMostEconomicalHotel bookings
      putStrLn "Question 2: Which hotel offers the most economical option?"
      putStrLn $ "  (Based on Booking Price, Discount, and Profit Margin)"
      putStrLn $ "Answer: " ++ T.unpack econHotel
      putStrLn $ "  Average Booking Price: SGD " ++ show (avgBookingPrice econData)
      putStrLn $ "  Average Discount: " ++ show (avgDiscount econData * 100) ++ "%"
      putStrLn $ "  Average Profit Margin: " ++ show (avgProfitMargin econData)
      putStrLn $ "  Number of Bookings: " ++ show (bookingCount econData)
      putStrLn $ "  Economical Score: " ++ show (economicalScore econData) ++ "\n"
      
      -- Question 3: Most profitable hotel
      let (profHotel, profData) = findMostProfitableHotel bookings
      putStrLn "Question 3: Which hotel is the most profitable?"
      putStrLn $ "  (Considering number of visitors and profit margin)"
      putStrLn $ "Answer: " ++ T.unpack profHotel
      putStrLn $ "  Total Visitors: " ++ show (totalVisitors profData)
      putStrLn $ "  Average Profit Margin: " ++ show (avgProfitMargin' profData)
      putStrLn $ "  Total Profit (approx): SGD " ++ show (totalProfit profData)
      putStrLn $ "  Profitability Score: " ++ show (profitabilityScore profData)

