module Hemmot where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color
import Aritmetiikka

--Hemmot
data Hemmo = Hemmo {hemmo_sijainti :: Point}

                   -- x        y
haluaakoLiikkua :: (Float -> Float) -> Point -> Hemmo -> Bool
haluaakoLiikkua korkeusKohdassa kopterinPaikka hemmo = haluaaLiikkua && not putoaako
      where
        putoaako = abs (korkeusEdessä - snd (hemmo_sijainti hemmo)) > 50
        korkeusEdessä = korkeusKohdassa (fst (hemmo_sijainti hemmo) + suunta * 2 )
        
        haluaaLiikkua = magV (kopterinPaikka #- hemmo_sijainti hemmo) < 600
        suunta = minneHemmoMenisi kopterinPaikka hemmo

minneHemmoMenisi :: Point -> Hemmo -> Float
minneHemmoMenisi kopterinPaikka hemmo
    | fst kopterinPaikka < fst (hemmo_sijainti hemmo) = -5
    | otherwise     = 5

päivitäHemmoa :: (Float -> Float) -> Point -> Hemmo -> Hemmo
päivitäHemmoa korkeusKohdassa kopterinPaikka hemmo
    | haluaakoLiikkua korkeusKohdassa kopterinPaikka hemmo 
          = hemmo{hemmo_sijainti = hemmo_sijainti hemmo #+ (suunta, 0)}
    | otherwise = hemmo
  where
    suunta = minneHemmoMenisi kopterinPaikka hemmo
    
piirräHemmo :: Float -> Hemmo -> Picture
piirräHemmo aika hemmo = let 
                          (x,y) = hemmo_sijainti hemmo
                          lantio = (15,40)
                          vasenJalka = 15+sin (12*aika) * 7
                          oikeaJalka = 15+cos (12*aika) * 7
                          hemmonKuva = color white 
                              (translate 0 110 (circleSolid 20)
                                <> line [(0,100), lantio] -- selkä
                                <> line [(-40,90 + cos (8*aika+0.3) * 40),(-30,90), (30,90)
                                        , (40,90 + cos (8*aika) * 40)] -- kädet
                                <> line [(-25,vasenJalka), (-20,vasenJalka) 
                                        , lantio
                                        , (30,oikeaJalka), (35,oikeaJalka)] --jalat
                              )
                         in translate x y hemmonKuva