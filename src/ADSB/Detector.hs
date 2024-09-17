module ADSB.Detector
    ( isPreambleValid
    , findPreamble
    ) where

import qualified Data.Vector.Storable as V
import Data.Word (Word16)
import Data.Maybe (listToMaybe)

-- | Checks if the preamble of the Mode S signal is valid.
-- The Mode S preamble is made of impulses of 0.5 microseconds at
-- the following time offsets:
--
-- 0   - 0.5 usec: first impulse.
-- 1.0 - 1.5 usec: second impulse.
-- 3.5 - 4   usec: third impulse.
-- 4.5 - 5   usec: last impulse.
-- 
-- Since we are sampling at 2 MHz, every sample in our magnitude vector
-- represents 0.5 usec. So the preamble will look like this, assuming there is
-- an impulse at offset 0 in the array:
--
-- 0   -----------------
-- 1   -
-- 2   ------------------
-- 3   --
-- 4   -
-- 5   --
-- 6   -
-- 7   ------------------
-- 8   --
-- 9   -------------------
--
-- The function ensures that the preamble adheres to these expectations and
-- that the magnitude values between and after the impulses are within expected
-- levels.
isPreambleValid :: V.Vector Word16 -> Bool
isPreambleValid m
    | V.length m < 15 = False  -- Ensure the vector is long enough to check preamble
    | otherwise = and [
        -- First check of relations between the first 10 samples
        -- representing a valid preamble. If this simple test is not passed,
        -- further investigation is not performed.
        m V.! 0  > m V.! 1
        , m V.! 1  < m V.! 2
        , m V.! 2  > m V.! 3
        , m V.! 3  < m V.! 0
        , m V.! 4  < m V.! 0
        , m V.! 5  < m V.! 0
        , m V.! 6  < m V.! 0
        , m V.! 7  > m V.! 8
        , m V.! 8  < m V.! 9
        , m V.! 9  > m V.! 6
        -- Ensure that samples between the two spikes are below the high level.
        -- This validates that the magnitude between spikes is within expected levels.
        , m V.! 4 < high
        , m V.! 5 < high
        -- Ensure that samples in the range 11-14 are below the high level.
        -- This checks the space between the preamble and the actual data.
        , m V.! 11 < high
        , m V.! 12 < high
        , m V.! 13 < high
        , m V.! 14 < high
        ]
  where
    -- Calculate the high value as the average of selected samples
    high = (m V.! 0 + m V.! 2 + m V.! 7 + m V.! 9) `div` 6

-- | Finds the next valid preamble in the given magnitude vector.
-- Returns the index of the start of the preamble if found, otherwise Nothing.
findPreamble :: V.Vector Word16 -> Maybe Int
findPreamble magVector = 
    listToMaybe $ findIndices isPreambleValid $ slidingVector 15 magVector

-- | Slides a window of size n over a vector, returning a list of all such windows.
slidingVector :: Int -> V.Vector Word16 -> [V.Vector Word16]
slidingVector n vec
    | n > V.length vec = []
    | otherwise = V.slice 0 n vec : slidingVector n (V.tail vec)

-- | Finds all indices where the predicate is True.
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p = map fst . filter (p . snd) . zip [0..]
