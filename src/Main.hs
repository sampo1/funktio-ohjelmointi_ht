module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Vector
import Data.List (partition)
import Prelude hiding (Down)

import Aritmetiikka
import Talot
import Hemmot
import Kopteri
import Pilvet


-- Tehdään pelin alkutilanne, annetaan paikat helikopterille, taloille, pilville ja hemmoille.
alkutilanne :: PeliTilanne
alkutilanne =
        (GameOn
          (Peli 
          0 
          (luoKopteri (10,0))
          
          [(Talo 400 500 600),(Talo 300 200 (-300)),(Talo 700 100 (-800))
          ,(Talo 350 200 1200),(Talo 100 700 (-1700)),(Talo 500 300 2100)
          ,(Talo 400 250 1500)]
          
          [(Pilvi 0 1000), (Pilvi 200 800), (Pilvi 500 700), (Pilvi (-200) 650)
          , (Pilvi (-850) 1200), (Pilvi (-1250) 900), (Pilvi (-1500) 800)
          , (Pilvi (-1800) 700), (Pilvi 1700 700), (Pilvi (-1900) 950)
          , (Pilvi 1900 1000), (Pilvi 1600 900), (Pilvi (2100) 700)]
          
          [Hemmo (500, 400), Hemmo ((-300), 300), Hemmo ((-1750), 100)
          , Hemmo ((1200), 350), Hemmo ((1500), 400), Hemmo ((2200), 500)]
        ))


main :: IO ()
main = play
        (InWindow "Choplifter" (2000,1000) (200,200))
        (light (light blue))
        60
        alkutilanne
        piirräPeliTilanne
        reagoiPeliTilanne
        päivitäPelitilanne


reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne 
    = case pelitilanne of
        GameOver cl -> GameOver cl
        GameOn cl -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli 
  = case tapahtuma of
      EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa 3) peli
      EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-3)) peli
      EventKey (Char 'a') Down _ _ -> kopterille (kallista (-15)) peli
      EventKey (Char 'd') Down _ _ -> kopterille (kallista (15)) peli
      _ -> peli


päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameOver peli -> GameOver peli
        GameOn peli   -> case  törmääköTaloon (kopteriTörmäysviivat (cl_kopteri peli)) (cl_talot peli) of
                          Nothing -> GameOn (päivitäPeliä aikaEdellisestä peli)
                          Just Roottori -> GameOver peli
                          Just Laskuteline 
                                | onkoHyväLaskeutuminen (cl_kopteri peli)
                                  -> GameOn (päivitäPeliä aikaEdellisestä (kopterille laskeudu peli))
                                | otherwise -> GameOver peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)}


päivitäPeliä ::  Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
      Peli aika kopteri talot pilvet hemmot
        -> let
            paikka = kop_paikka kopteri
            nouseekoKyytiin hemmo =  magV (hemmo_sijainti hemmo #- paikka ) < 70
            (hemmotKopteriin, hemmotUlkona) = partition nouseekoKyytiin hemmot
           in Peli (aika + aikaEdellisestä) 
                   (päivitäKopteria aikaEdellisestä (genericLength hemmotKopteriin) kopteri)
                   talot
                   pilvet
                   (map (päivitäHemmoa (flip korkeusKohdassa edellinenTila)
                                        paikka)
                        hemmotUlkona)


törmääköTaloon :: ((Point,Point),(Point,Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
    where
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
            (va,oy)   = nurkkaPisteet talo 
          in case (not (segClearsBox ala1 ala2 va oy) , not (segClearsBox ylä1 ylä2 va oy)) of
                (True, False) -> Just Laskuteline
                (False, False) -> Nothing
                _ -> Just Roottori



piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne 
    = case pelitilanne of
        GameOver cl -> piirräPeli cl <> translate (-400) (-200) (color yellow (text "GAME OVER"))
        GameOn cl   -> piirräPeli cl


-- Lisätty pilvet alkuun, jotta kopteri piirtyy niiden päälle ja sitä näkee lentää.
piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                      aika  = cl_aika peli
                      talot = cl_talot peli
                      pilvet = cl_pilvet peli

                      kopterikuva = piirräKopteri aika (cl_kopteri peli)


                      hemmoKuvat = map (piirräHemmo aika) (cl_hemmot peli) 

                      taloKuvat = map piirräTalo talot

                      pilviKuvat = map piirräPilvi pilvet

                      peliKuva = (pictures pilviKuvat)
                           <> maa  
                           <> pictures taloKuvat
                           <> pictures hemmoKuvat
                           <> kopterikuva 
                  in scale 0.4 0.4 (translate 0 (-180) peliKuva)


data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter 
  = Peli
    {
        cl_aika   :: Float            -- Aika pelin alusta
      , cl_kopteri :: Kopteri         -- Kopterin tiedot
      , cl_talot :: [Talo]            -- Esteet pelissä
      , cl_pilvet :: [Pilvi]          -- Pilvet taivaalla
      , cl_hemmot :: [Hemmo]          -- Pelihahmot
    }

                  --x
korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

          

-- Maa jota tummennettu useamman kerran.
maa :: Picture
maa = color (dark (dark (dark (dark green)))) (translate 0 (-500) (rectangleSolid 5000 1000))