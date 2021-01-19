module Pilvet where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

--Pilvi tietotyyppi, jossa pilven koordinaatit.
data Pilvi = Pilvi {pilvi_x :: Float, pilvi_y :: Float}


--Piirretään pilviä koristeeksi annetuihin koordinaatteihin.
piirräPilvi :: Pilvi -> Picture
piirräPilvi pilvi = let
                            pallo1 = translate (pilvi_x pilvi) (pilvi_y pilvi +20) ((color white) (circleSolid 60))
                            pallo2 = translate (pilvi_x pilvi -70) (pilvi_y pilvi) ((color white) (circleSolid 50))
                            pallo3 = translate (pilvi_x pilvi +70) (pilvi_y pilvi) ((color white) (circleSolid 50))
                            viiva = translate (pilvi_x pilvi) (pilvi_y pilvi -34) ((color white) (rectangleSolid 150 30))
                         in pallo1 <> pallo2 <> pallo3 <> viiva
