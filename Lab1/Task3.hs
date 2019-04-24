import Data.Scientific
f a b = if (isInteger (head a) && ((head a) > 0)) then b else (head b) : (tail a) 