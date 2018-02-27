module Lib (
  module Csound.Base,
  renderSnd,
  wind) where

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
