module Chemistry (
  Cation (..), cations, cation, catSym, catValence,
  Anion (..), anions, anion, anSym, anValence,
  AcidAlkaliSalt (..), acidAlkaliSalts, acidAlkaliSalt,
  Solubility (..), solubility,
  DoubleDecompReact (..), doubleDecompReacts, doubleDecompReact) where

import Data.List (tails)
import Data.Maybe (isJust)
import Data.Char (isUpper)
import Math (reduceFrac)

data Cation = H  | NH4 | K  | Na  | Ba  | Ca | Mg
            | Al | Mn  | Zn | Fe2 | Fe3 | Cu | Ag
            deriving Eq

cations :: [Cation]
cations = [H, NH4, K , Na , Ba , Ca, Mg, Al, Mn , Zn, Fe2, Fe3, Cu, Ag]

cation :: Num a => Cation -> (String, a)
cation cat = case cat of
  H   -> ("H"  , 1)
  NH4 -> ("NH4", 1)
  K   -> ("K"  , 1)
  Na  -> ("Na" , 1)
  Ba  -> ("Ba" , 2)
  Ca  -> ("Ca" , 2)
  Mg  -> ("Mg" , 2)
  Al  -> ("Al" , 3)
  Mn  -> ("Mn" , 2)
  Zn  -> ("Zn" , 2)
  Fe2 -> ("Fe" , 2)
  Fe3 -> ("Fe" , 3)
  Cu  -> ("Cu" , 2)
  Ag  -> ("Ag" , 1)

catSym :: Cation -> String
catSym = fst . cation

catValence :: Num a => Cation -> a
catValence = snd . cation

instance Show Cation where
  show = fst . cation

data Anion = OH | NO3 | Cl | SO4 | CO3 deriving Eq

anions :: [Anion]
anions = [OH, NO3, Cl, SO4, CO3]

anion :: Num a => Anion -> (String, a)
anion an = case an of
  OH  -> ("OH" , -1)
  NO3 -> ("NO3", -1)
  Cl  -> ("Cl" , -1)
  SO4 -> ("SO4", -2)
  CO3 -> ("CO3", -2)

anSym :: Anion -> String
anSym = fst . anion

anValence :: Num a => Anion -> a
anValence = snd . anion

instance Show Anion where
  show = fst . anion

data AcidAlkaliSalt = AcidAlkaliSalt Cation Anion deriving Eq

acidAlkaliSalts :: [AcidAlkaliSalt]
acidAlkaliSalts = tail [s | cat <- cations, an <- anions,
                            let s = AcidAlkaliSalt cat an,
                            solubility s /= Undefined]

acidAlkaliSalt :: Integral a => AcidAlkaliSalt -> ((Cation, a), (Anion, a))
acidAlkaliSalt (AcidAlkaliSalt cat an) =
  let x = - snd (anion an)
      y = snd (cation cat)
      gcdxy = gcd x y
   in ((cat, x `div` gcdxy), (an, y `div` gcdxy))

instance Show AcidAlkaliSalt where
  show (AcidAlkaliSalt H OH) = "H2O"
  show s = let (x, y) = acidAlkaliSalt s
            in f x ++ f y
    where f (s, x) = if x == 1 then show s else addBrackets s ++ show x
          addBrackets s
            | length (filter isUpper str) > 1 = "(" ++ str ++ ")"
            | otherwise = str
            where str = show s

data Solubility = Soluble | SightlySoluble | Insoluble | Undefined
                  deriving (Show, Eq)

solubility :: AcidAlkaliSalt -> Solubility
solubility (AcidAlkaliSalt Ag  OH ) = Undefined
solubility (AcidAlkaliSalt Al  CO3) = Undefined
solubility (AcidAlkaliSalt Fe3 CO3) = Undefined
solubility (AcidAlkaliSalt Ca  OH ) = SightlySoluble
solubility (AcidAlkaliSalt Ca  SO4) = SightlySoluble
solubility (AcidAlkaliSalt Ag  SO4) = SightlySoluble
solubility (AcidAlkaliSalt Mg  CO3) = SightlySoluble
solubility (AcidAlkaliSalt Mg  OH ) = Insoluble
solubility (AcidAlkaliSalt Al  OH ) = Insoluble
solubility (AcidAlkaliSalt Mn  OH ) = Insoluble
solubility (AcidAlkaliSalt Zn  OH ) = Insoluble
solubility (AcidAlkaliSalt Fe2 OH ) = Insoluble
solubility (AcidAlkaliSalt Fe3 OH ) = Insoluble
solubility (AcidAlkaliSalt Cu  OH ) = Insoluble
solubility (AcidAlkaliSalt Ag  Cl ) = Insoluble
solubility (AcidAlkaliSalt Ba  SO4) = Insoluble
solubility (AcidAlkaliSalt Ba  CO3) = Insoluble
solubility (AcidAlkaliSalt Ca  CO3) = Insoluble
solubility (AcidAlkaliSalt Mn  CO3) = Insoluble
solubility (AcidAlkaliSalt Zn  CO3) = Insoluble
solubility (AcidAlkaliSalt Fe2 CO3) = Insoluble
solubility (AcidAlkaliSalt Cu  CO3) = Insoluble
solubility (AcidAlkaliSalt Ag  CO3) = Insoluble
solubility _                        = Soluble

data DoubleDecompReact = DoubleDecompReact AcidAlkaliSalt AcidAlkaliSalt

doubleDecompReacts :: [DoubleDecompReact]
doubleDecompReacts = [r | (x : xs) <- tails acidAlkaliSalts,
                          y <- xs,
                          let r = DoubleDecompReact x y,
                          isJust $ doubleDecompReact r]

doubleDecompReact :: Integral a
                  => DoubleDecompReact
                  -> Maybe (((a, AcidAlkaliSalt), (a, AcidAlkaliSalt)),
                            ((a, AcidAlkaliSalt), (a, AcidAlkaliSalt)))
doubleDecompReact
  (DoubleDecompReact
    x @ (AcidAlkaliSalt xa xb)
    y @ (AcidAlkaliSalt ya yb)) =
  let z = AcidAlkaliSalt xa yb
      w = AcidAlkaliSalt ya xb
      ((_, xn), _) = acidAlkaliSalt x
      ((_, yn), _) = acidAlkaliSalt y
      ((_, zn), _) = acidAlkaliSalt z
      ((_, wn), _) = acidAlkaliSalt w
   in if (x /= y && x /= z && x /= w && y /= z && y /= w) &&
         (solubility x == Soluble || solubility y == Soluble) &&
         (solubility z == Insoluble || solubility w == Insoluble ||
          solubility z == SightlySoluble || solubility w == SightlySoluble ||
          z == AcidAlkaliSalt H CO3 || w == AcidAlkaliSalt H CO3 ||
          z == AcidAlkaliSalt H OH || w == AcidAlkaliSalt H OH) &&
         if xa == H && yb /= OH then solubility x == Soluble else
         if ya == H && yb /= OH then solubility y == Soluble else True
      then let (a, b) = reduceFrac (catValence ya * yn) (catValence xa * xn)
               (c, d) = reduceFrac (catValence ya * wn) (catValence xa * zn)
            in Just (((a, x), (b, y)), ((c, z), (d, w)))
      else Nothing

instance Show DoubleDecompReact where
  show r = case doubleDecompReact r of
    Nothing -> "No reaction"
    Just (((a, x), (b, y)), ((c, z), (d, w))) ->
      if solubility x == Soluble && solubility y == Soluble
      then omit1 a ++ show x ++ " + " ++ omit1 b ++ show y ++ " == " ++
           makeH2CO3 (c, z) ++ makeInsoluble z ++ " + " ++
           makeH2CO3 (d, w) ++ makeInsoluble w
      else omit1 a ++ show x ++ " + " ++ omit1 b ++ show y ++ " == " ++
           makeH2CO3 (c, z) ++ " + " ++ makeH2CO3 (d, w)
    where makeInsoluble x = if solubility x /= Soluble then "↓" else ""
          makeH2CO3 (n, x) = omit1 n ++ if x == AcidAlkaliSalt H CO3
                             then "H2O + " ++ omit1 n ++ "CO2↑"
                             else show x
          omit1 1 = ""
          omit1 x = show x
