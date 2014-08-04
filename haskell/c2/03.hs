type Point = (Double, Double)

makePoint :: Double -> Double -> Point
makePoint x y = (x, y)

xPoint :: Point -> Double
xPoint (x, _) = x

yPoint :: Point -> Double
yPoint (_, y) = y

-- Impl 1
-- data Rectangle = Rectangle Point Double Double
-- makeRect :: Point -> Double -> Double -> Rectangle
-- makeRect topLeft width height = Rectangle topLeft width height

-- widthRect :: Rectangle -> Double
-- widthRect (Rectangle _ width _) = width

-- heightRect :: Rectangle -> Double
-- heightRect (Rectangle _ _ height) = height

-- Impl 2
type Rectangle = (Point, (Double, Double))
makeRect :: Point -> Double -> Double -> Rectangle
makeRect topLeft width height = (topLeft, (width, height))

widthRect :: Rectangle -> Double
widthRect (_, (width, _)) = width

heightRect :: Rectangle -> Double
heightRect (_, (_, height)) = height

periRect :: Rectangle -> Double
periRect rect = 2 * (widthRect rect + heightRect rect)

areaRect :: Rectangle -> Double
areaRect rect = widthRect rect * heightRect rect

main :: IO ()
main = do
  topLeft <- return $ makePoint 0 0
  rect <- return $ makeRect topLeft 4 8
  area <- return $ areaRect rect
  perimeter <- return $ periRect rect

  putStrLn $ "The area: " ++ show area
  putStrLn $ "The perimeter: " ++ show perimeter
