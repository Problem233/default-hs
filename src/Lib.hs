module Lib (
  -- * Re-exported modules
  module ReExportedModules,
  -- * Utils
  renderSnd,
  myWind,
  spinFull, spin,
  Note, Score,
  tempo,
  shiftPitch, amplify,
  noteAmp, note, notes,
  playWith,
  -- * Scores
  twinkle) where

import Data.Char (isDigit)
import Csound.Base as ReExportedModules hiding (tempo)
import Csound.Patch as ReExportedModules

renderSnd :: RenderCsd a => String -> a -> IO ()
renderSnd = writeSndBy $ setRates 48000 64 <>
              def {
                csdFlags = def {
                  audioFileOutput = def {
                    formatSamples = Just Bit24
                  }
                }
              }

myWind :: SE Sig
myWind = osc . (\x -> 220 + 88 * (osc x + x)) <$> pink

spinFull :: Sig -> Sig -> Sig2
spinFull f x = let l = (osc f + 1) / 2
                   r = 1 - l
                 in at ((l, r) *) x

-- | Spins the sound around.
spin :: Sig -- ^ The amount of the sound saved in both left and right. 
            -- This argument should be @< 1@ and @>= 0@,
            -- or unknown error may occur.
     -> Sig -- ^ The frequency of spinning.
     -> Sig -- ^ The input signal.
     -> (Sig, Sig)
spin base f x = at (\x' -> base * x + (1 - base) * x') (spinFull f x)

type Note = CsdNote D
type Score = Sco Note

-- | Sets the tempo of a score.
-- Shouldn't be applied to a score twice.
tempo :: D -> Score -> Score
tempo t = str (60 / sig t)

-- | Shift the pitch of a score by semitone.
-- eg.
-- > shiftPitch 2 (note "4C") == note "4D"
shiftPitch :: D -> Score -> Score
shiftPitch x = fmap $ \(a, b) -> (a, b * semitone x)

-- | Amplify a score.
amplify :: D -> Score -> Score
amplify x = fmap $ \(a, b) -> (a * x, b)

noteAmp :: D -> String -> Score
noteAmp amp s =
  case last s of
    '^' -> let (len, note') = span (== '^') $ reverse s
            in str (1 / 2 ^ length len) $ noteAmp amp $ reverse note'
    '~' -> let (len, note') = span (== '~') $ reverse s
            in str (sig $ int $ 1 + length len) $ noteAmp amp $ reverse note'
    _ -> mkNote s
  where mkNote "0" = rest 1
        mkNote s = temp (amp, cpsmidinn $ ntom $ text s)

note :: String -> Score
note = noteAmp 1

-- TODO make it more friendly
-- eg. (3A 3B)^ 4C - 0 |
-- eg. 4C^ . 4D^^ |
notes :: String -> Score
notes = mel . fmap note . filter (isDigit . head) . words

playWith :: (RenderCsd a, SigSpace a, Sigs a) => Patch a -> Score -> IO ()
playWith instr = dac . mix . atSco instr

twinkle :: Score
twinkle = tempo 100 $ mel $ notes <$> [pA, pB, pA]
  where pA = "4C 4C | 4G 4G | 4A 4A | 4G~ - | 4F 4F | 4E 4E | 4D 4D | 4C~ - |"
        pB = "4G 4G | 4F 4F | 4E 4E | 4D~ - | 4G 4G | 4F 4F | 4E 4E | 4D~ - |"

test1 :: Score
test1 = tempo 100 $ notes
  "3A^ 3B^ 4C~~ - - | 3A^ 3B^ 4C~~ - - | 3B^ 4C^ 4D~~ - - | 4C^ 3B^ 3A~~ - - ||"

-- | This melody has been used in my music homework.
-- TODO Needs to be rewritten after `notes` is reworked.
test2 :: Score
test2 = tempo 160 $ notes $ unwords [
  "4F 4C^ 4F 5C^ 0^ 4B~ 4A^~~~~ |",
  "4E^ 4F^ 4G^ 4F^ 0^ 4E^ 0^ 4D^ 0^ 4E^ 0^ 4F^~~ 4G ||"]

-- | This melody requires to be edited to satisfy rhythm.
test3 :: Score
test3 = tempo 120 $ notes $ unwords [
  "[ 4C^ 3B^ ] 4C~ - 3F | 4C 4D 4C^ 3B [4.5] | 4C 3A~ 0^ [3.5] | 0 0 0 0 |",
  "[ 3A^ 3B^ ] 4C~ - 3B | 4C 4D 4C^ 4D [4.5] | 4F 4E~ 4D^ [3.5] | 4C~ - 0 0 |",
  "[ 4C^ 4D^ ] 4E~ - 4D^ 4E^~~~~ [6] | 4D 4E [2] | 4A 4G [ 4E^^ 4D^^ ] 4C^~~ |",
  "0 0 4C 3B | 3A^~~~~ 4C^ 4E | 4D 4C^ 3B^~~ - 4C | 3A~~~ - - - | 0 0 0 0 ||"]

sunriseAccordeon :: Patch Sig2
sunriseAccordeon = accordeon' $ Accordeon 0.625 1 0.875 0.75

-- suggested patches: noisyChoir, caveOvertonePad, sunriseAccordeon, bhumiLofi 80
test4 :: Score
test4 = shiftPitch (-5) $ tempo 160 $ mel $ notes <$> [pA, pB, pA, pC]
  where
    pA = "4A^~~ 4A^~~ 4A | 4G^~~ 4G^~~ 4G | 4A^~~ 4A^~~ 4A | 4G^~~ 4G^~~ 4G |"
    pB = "5C^~~ 5C^~~ 5C | 4A^~~ 4A^~~ 4A | 4G^~~ 4G^~~ 4G | 4F^~~ 4F^~~ 4E |"
    pC = "5C^~~ 5C^~~ 5C | 4A^~~ 4A^~~ 4A | 5C^~~ 5C^~~ 5C | 5D^~~ 5D^~~ 5D |"

test5 :: Score
test5 = tempo 140 $ mel [
  pA, notes "0^ 4E^", pB, notes "4G^ 4A^",
  pB, notes "0^ 4A^", pA, notes "4D^ 4C^"]
  where pA = mel [note "4C^", noteAmp 0.3 "4E^", loopBy 6 $ notes "0^ 4E^"]
        pB = mel [note "4D^", noteAmp 0.3 "4F^", loopBy 6 $ notes "0^ 4F^"]
