import Char
import String
import Graphics.Input as Input
import Text
import Window
import Keyboard

-- Model

grid : [[Int]]
grid = [[2,4,8,16],
        [2,4,8,8],
        [4,0,2,16],
        [0,2,8,2048]]

-- Display

typeset x = if | x == 0  -> " "
               | otherwise ->  show x


mono : Int -> Element 
mono = centered . monospace . toText . typeset


border : Element -> Element
border x = let w : Int
               w = widthOf x
               h : Int
               h = heightOf x
               rectangle : Shape
               rectangle = rect (toFloat w) (toFloat h)
               frame : Form
               frame = outlined (solid black) rectangle
               border : Element
               border = collage  w h [outlined (solid black) rectangle]
           in layers [border, x]

box num = border (container 50 50 middle (mono num))

displayGrid : [[Int]] -> Element
displayGrid inpt = flow down (map (flow right . map box) inpt)


-- Update



rightCell : Int -> [Int] -> [Int]
-- takes the (1-indexed) position of a cell in a row, and returns
-- the result of moving that cell right.
-- Starts from the rightmost cell.
rightCell x ls = let width = length ls
                     lefts = take (x-1) ls
                     cell = head (drop (x-1) (take x ls))
                     rights = drop x ls 
                     spaces = filter (\x -> x == 0) rights
                     solids = filter (\x -> x /= 0) rights
                 in 
                   if | x == width -> ls
                      | isEmpty solids -> lefts ++ rights ++ [cell]
                      | head solids == cell -> lefts ++ spaces ++ 
                                               (0 :: cell*2 :: tail solids)
                      | otherwise -> lefts ++ spaces ++ [cell] ++ solids

rightRow : [Int] -> [Int]
-- moves all the cells in a row to the right
rightRow ls = foldr rightCell ls [1..length ls]

moveRight : {x : Int, y : Int} -> [[Int]] -> [[Int]]
moveRight {x} gr = if x > 0 then 
                       map rightRow gr
                   else gr


{- 
leftRow : [Int] -> [Int]
leftRow ls = reverse (rightRow (reverse ls))

moveLeft = {x} gr = if x < 0 then 
                        map leftRow gr
                    else gr

transpose : [[a]] -> [[a]]
transpose ls = if | isEmpty ls = []
                  | isEmpty (head ls) = []
                  | otherwise = map head ls : transpose (tail ls)

moveUp = 
-}


-- 2048

input = Keyboard.arrows

main =  (lift displayGrid) (foldp moveRight grid input) 

