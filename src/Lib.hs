module Lib (
  module Csound.Base,
  renderSnd,
  wind,
  spin, spinFull) where

import Csound.Base

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
