type Point = (Double, Double)
type Segment = (Point , Point)

makeSegment :: Point -> Point -> Segment
makeSegment start end = (start, end)

startSegment :: Segment -> Point
startSegment (start, _) = start

endSegment :: Segment -> Point
endSegment (_, end) = end

makePoint :: Double -> Double -> Point
makePoint x y = (x, y)

xPoint :: Point -> Double
xPoint (x, _) = x

yPoint :: Point -> Double
yPoint (_, y) = y

midpointSegment :: Segment -> Point
midpointSegment ((x1,y1), (x2,y2)) = makePoint (avg x1 x2) (avg y1 y2)
  where avg x y = (x + y) / 2

main :: IO ()
main = do
  start <- return $ makePoint 0 0
  end <- return $ makePoint 4 8
  segment <- return $ makeSegment start end
  midpoint <- return $ midpointSegment segment

  putStrLn $ "from " ++ show start ++ " to " ++ show end
  putStrLn $ "midpoint: " ++ show midpoint
