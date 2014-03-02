import SplitLines (splitLines)
import InteractWith (mainWith)

fixLines :: String -> String
fixLines input = unlines (splitLines input)

main = mainWith fixLines
