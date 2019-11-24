data Months = Jan | Feb | Mar | April | May | June | July | Aug | Sep | Oct | Nov | Dec deriving (Enum, Read, Eq, Show)
type Date = (Int, Months, Int)
monthss = [Jan, Feb ,Mar, April, May, June, July, Aug, Sep, Oct, Nov, Dec]
leap :: Int -> Bool


leap (x)
    | x `mod` 100 + x `mod` 400 == 0 = True
    | x `mod` 100 == 0 = False 
    | x `mod` 4 == 0 = True
    | otherwise =  False



mLengths :: Int -> [Int]
mLengths (y)
    | leap y == True = [31,29,31,30,31,30,31,31,30,31,30,31]
    | otherwise = [31,28,31,30,31,30,31,31,30,31,30,31]


dayOfWeeks :: Date -> Int
dayOfWeeks(x, y, z)
    | z == 1753 = 0
    | z > 1753 =  (sum (mLengths( z-1))) + (dayOfWeeks( x, y, z-1))

numDays :: Date -> Int
numDays (x, y, z) = 
    x + sum (take len (mLengths z)) + dayOfWeeks(x,y,z)
    where len = length(take (indexFind y monthss) monthss)


indexFind :: Months -> [Months] -> Int
indexFind a (x:xs)
    | not(elem a (x:xs)) = 0
    | (a == x) = 0
    | otherwise = 1 + (indexFind a xs)

days = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday"]
dayOfWeek :: Date -> String
dayOfWeek(x,y,z)=

    days !! number
    where number = numDays(x, y, z) `mod` 7