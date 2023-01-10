import Control.Lens
data Temp = Temp { _fahrenheit :: Float } deriving (Eq, Show)

fahrenheit :: Lens' Temp Float
fahrenheit f t = fmap (\temp -> t {_fahrenheit = temp}) (f $ _fahrenheit t)

centigrade :: Lens' Temp Float
centigrade f (Temp fahren) = fmap (\centi -> Temp (cToF centi)) (f $ fahrenheitToCentigrade fahren)

fToC :: Float -> Float
fToC f = (f-32)*5 / 9

cToF :: Float -> Float
cToF c = c*9 / 5 + 32

main = do
    let temp = Temp { _fahrenheit = (100 :: Float) }
    print $ "Temperature object(in fahrenheit)" ++ show temp
    let tempInCens = view centigrade temp
    print $ "Temperature in centigrade " ++ show tempInCens
    print $ "Temperature object(in fahrenheit)" ++ show temp
    let centTemp = over centigrade (\_fahrenheit -> _fahrenheit + 100) temp
    print $ "Temperature object for centigrate" ++ show temp
