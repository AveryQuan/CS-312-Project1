module IOHelpers where

-- Code from A3
-- getLineFixed: Use this when expecting user-typed input (strings)
getLineFixed =
   do
     line <- getLine
     return (fixdel line)

-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r