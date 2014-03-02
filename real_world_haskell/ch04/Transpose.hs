-- For ex4, on page 84
-- Transpose a text file.
import InteractWith (mainWith)

transpose :: [String] -> [String]
transpose lines = 
    if any (not.null) lines
    then map spaceHead lines : transpose (map innerTail lines)
    else []
    where spaceHead "" = ' '
          spaceHead cs = head cs
          innerTail "" = ""
          innerTail cs = tail cs


main = mainWith (\cs -> unlines (transpose (lines cs)))
