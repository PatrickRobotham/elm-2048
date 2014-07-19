import Char
import String
import Graphics.Input as Input
import Text
import Window
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


displayGrid = flow down (map (flow right . map box) grid)


-- Update




-- 2048


main : Element
main = displayGrid

