module Shapes where
    data Point = Point Double Double deriving (Show, Read, Eq)
    data Shape = Circle Point Double | Rectangle Point Point | Triangle Point Point Point deriving (Show, Read, Eq)

    -- Area of a shape
    area :: Shape -> Double
    area (Circle _ r) = pi*r*r
    area (Rectangle (Point l t) (Point r b)) = abs (r-l)*(b-t)
    -- Using a simplified shoelace formula
    area (Triangle (Point x0 y0) (Point x1 y1) (Point x2 y2)) = abs ((x0-x2)*(y1-y0)-(x0-x1)*(y2-y0))/2

    -- Rectange bounding box of a shape
    box :: Shape -> Shape
    box (Circle (Point x y) r) = Rectangle (Point (x-r) (y-r)) (Point (x+r) (y+r))
    box (Rectangle tl br) = Rectangle tl br
    box (Triangle (Point x0 y0) (Point x1 y1) (Point x2 y2)) = Rectangle (Point (minimum [x0,x1,x2]) (minimum [y0,y1,y2])) (Point (maximum [x0,x1,x2]) (maximum [y0,y1,y2]))

    -- Centre of a shape (using the mean x and y coordinates)
    centre :: Shape -> Point
    centre (Circle c _) = c
    centre (Rectangle (Point l t) (Point r b)) = Point ((r+l)/2) ((t+b)/2)
    centre (Triangle (Point x0 y0) (Point x1 y1) (Point x2 y2)) = Point ((x0+x1+x2)/3) ((y0+y1+y2)/3)