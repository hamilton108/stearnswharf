module StearnsWharf.Profile where

import StearnsWharf.Common (Shear, StaticMoment)

class Profile a where
  desc ::
    a ->
    -- | Beskrivele av bjelke, mest brukt for SteelProfile
    String
  desc _ = "N/A"
  sigma ::
    a ->
    StaticMoment ->
    -- | Boyespenninger for tverrsnittet [a -> kNm -> N/mm2]
    Double
  tau ::
    a ->
    Shear ->
    -- | Skjaer for tverrsnittet [a -> kN -> N/mm2]
    Double
  area ::
    a ->
    -- | Tverrsnittsareal [m2]
    Double
  areaMM ::
    a ->
    -- | Tverrsnittsareal [mm2]
    Double
  areaMM x = 1000.0 ** 2 * area x -- \^ Default implementation areaMM
  emodulus ::
    a ->
    -- | Tverrsnittets emodul [N/mm2]
    Double
  secondAreaMoment ::
    a ->
    -- | Annet arealmoment [m4]
    Double
  secondAreaMomentMM ::
    a ->
    -- | Annet arealmoment [mm4]
    Double
  secondAreaMomentMM x = 1000.0 ** 4 * (secondAreaMoment x) -- \^ Default implementation secondAreaMomentMM
  sectionModulus ::
    a ->
    -- | Statisk moment, W [m3]
    Double
  sectionModulusMM ::
    a ->
    -- | Statisk moment, W [mm3]
    Double
  sectionModulusMM x = 1000.0 ** 3 * (sectionModulus x) -- \^ Default implementation sectionModulusMM
  centroid ::
    a ->
    -- | Profilet centroid [m]
    Double
  centroidMM ::
    a ->
    -- | Profilet centroid [mm]
    Double
  centroidMM x = (centroid x) * 1000

  -- | Nedbøyning fra jevnt fordelt last [a -> kN -> m -> mm]
  deflection ::
    a ->
    -- | Jevnt fordelt last q [kN/m]
    Double ->
    -- | Elementlengde l [m]
    Double ->
    -- | Nedbøyning [mm]
    Double
  deflection x q l = 5 * q * (l' ** 4) / (384 * ee * ii)
   where
    ee = emodulus x
    l' = l * 1000.0
    ii = secondAreaMomentMM x
