import Char
import String
import Graphics.Input as Input
import Text
import Window
import Keyboard
import Random
import Array
-- Model

state = {grid = [[2,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]], turn = True}

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

displayGrid inpt = flow down (map (flow right . map box) inpt.grid)


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

moveRight :  [[Int]] -> [[Int]]
moveRight gr = map rightRow gr
               
leftRow : [Int] -> [Int]
leftRow ls = reverse (rightRow (reverse ls))

moveLeft gr = map leftRow gr
                    
transpose : [[a]] -> [[a]]
transpose ls = if | isEmpty ls -> []
                  | isEmpty (head ls) -> []
                  | otherwise -> map head ls :: transpose (map tail ls)

moveUp gr = transpose (moveLeft (transpose gr))

moveDown gr = transpose (moveRight (transpose gr))
                
index n xs = if | n == 0 -> head xs
           | otherwise -> index (n-1) (tail xs)

lookup : (Int , Int) -> [[a]] -> a
lookup (x,y) ls = index y (index x ls) 

coords = (concatMap (\x -> zip (repeat 4 x) [0..3]) [0..3])

place : Int -> [[Int]] -> [[Int]]
-- Assumes grid has at least one empty space
place seed gr = let empty (x,y) = lookup (x,y) gr == 0
                    spaces = filter empty coords
                    (i,j) = index (mod seed (length spaces)) spaces
                    ar = Array.fromList (map Array.fromList gr)
                    updatedRow = Array.set j 2 (Array.getOrFail i ar)
                in 
                  Array.toList (Array.map Array.toList (Array.set i updatedRow ar))
      

play ({x,y},n) gr = 
    if | gr.turn -> {gr | grid <- place n gr.grid, turn <- False}
       | x > 0 -> {gr | grid <- moveRight gr.grid,turn <-  moveRight gr.grid /= gr.grid}
       | x < 0 -> {gr | grid <- moveLeft gr.grid, turn <- moveLeft gr.grid /= gr.grid}
       | y > 0 -> {gr | grid <- moveUp gr.grid, turn <- moveUp gr.grid /= gr.grid}
       | y < 0 -> {gr | grid <- moveDown gr.grid, turn <- moveDown gr.grid /= gr.grid}
       | otherwise -> gr
                       

-- 2048

keys = Keyboard.arrows

input = lift2 (,) keys (Random.range 0 16 keys)

main =  (lift displayGrid) (foldp play state input)

