# Hotel Dataset Analysis

This Haskell program analyzes hotel booking data from a CSV file to answer three questions:

1. Which country has the highest number of bookings?
2. Which hotel offers the most economical option (based on Booking Price, Discount, and Profit Margin)?
3. Which hotel is the most profitable (considering number of visitors and profit margin)?

## Requirements

- GHC (Glasgow Haskell Compiler) 9.x or later
- Cabal (Haskell package manager)

## Building and Running

1. **Install dependencies and build:**
   ```bash
   cabal build
   ```

2. **Run the analysis:**
   ```bash
   cabal run hotel-analysis
   ```

   Or if you've already built it:
   ```bash
   cabal exec hotel-analysis
   ```

## How It Works

- The program reads `Hotel_Dataset.csv` from the current directory
- Parses CSV data using the `cassava` library
- Performs three analyses:
  - **Country Analysis**: Counts bookings per origin country
  - **Economical Hotel**: Calculates a score based on booking price, discount, and profit margin (lower score = more economical)
  - **Profitable Hotel**: Calculates total profit considering number of visitors and profit margin

## Output

The program will display:
- The country with the highest number of bookings
- The most economical hotel with its statistics
- The most profitable hotel with its statistics

