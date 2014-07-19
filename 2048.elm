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


mono = centered . monospace . toText . typeset


box num = (container 50 50 middle (mono num))


displayGrid = flow down (map (flow right . map box) grid)


-- Update




-- 2048


main : Element
main = displayGrid

