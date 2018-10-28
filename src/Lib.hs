module Lib (
  module ExportedModules,
  renderSnd,
  wind,
  spin, spinFull) where

import Data.Char (isDigit)
import Csound.Base as ExportedModules hiding (Instr)

renderSnd :: RenderCsd a => String -> a -> IO ()
renderSnd = writeSndBy $ setRates 48000 64 <>
              def {
                csdFlags = def {
                  audioFileOutput = def {
                    formatSamples = Just Bit24
                  }
                }
              }

wind :: SE Sig
wind = osc . (\x -> 220 + 88 * (osc x + x)) <$> pink

-- | Spins the sound.
spin :: Sig -- ^ The amount of the sound saved in both left and right. 
            -- This argument should be @< 1@ and @>= 0@,
            -- or unknown error may occur.
     -> Sig -- ^ The frequency of spinning.
     -> Sig -- ^ The input signal.
     -> (Sig, Sig)
spin save freq sig =
  let save' = 2 * save / (save - 2)
      left = (osc freq + 1 + save') / (2 + save')
      right = 1 - left
   in (left * sig, right * sig)

spinFull :: Sig -> Sig -> (Sig, Sig)
spinFull = spin 0

oscInstr :: D -> SE Sig
oscInstr = return . osc . sig

note :: String -> Sco D
note s =
  case last s of
    '^' -> let (len, note') = span (== '^') $ reverse s
            in str (1 / 2 ^ length len) $ mkNote $ reverse note'
    '~' -> let (len, note') = span (== '~') $ reverse s
            in str (sig $ int $ 1 + length len) $ mkNote $ reverse note'
    _ -> mkNote s
  where mkNote "0" = rest 1
        mkNote s = temp $ cpsmidinn $ ntom $ text s

-- TODO make it more friendly
-- eg. (3A 3B)^ 4C - 0 |
notes :: String -> [Sco D]
notes = fmap note . filter (isDigit . head) . words

twinkle :: [Sco D]
twinkle = notes "4C 4C | 4G 4G | 4A 4A | 4G~ | 4F 4F | 4E 4E | 4D 4D | 4C~ |"

testMelody :: [Sco D]
testMelody =
  notes "3A^ 3B^ 4C~ - 0 | 3A^ 3B^ 4C~ - 0 | 3B^ 4C^ 4D~ - 0 | 4C^ 3B^ 3A~ - 0 |"

run :: (D -> SE Sig) -> Sig -> [Sco D] -> IO ()
run instr stretch melody =
  dac $ mix $ mel $ fmap (sco instr . str stretch) melody
