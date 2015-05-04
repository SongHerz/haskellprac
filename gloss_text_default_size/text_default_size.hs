import qualified Graphics.Gloss as G

-- It looks the default text size is 100 for gloss library.
showText :: G.Picture
showText = G.pictures [G.translate (width/2) (width/2) $ G.rectangleWire width width, G.text "Hello"]
    where width = 100

main = G.display (G.InWindow "abc" (500, 500) (10, 10)) G.white $ G.translate (-100) 0 showText
