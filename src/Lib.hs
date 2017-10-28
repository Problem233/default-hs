module Lib (
  module Csound.Base,
  renderSnd,
  wind) where

import Csound.Base

renderSnd :: RenderCsd a => String -> a -> IO ()
renderSnd = writeSndBy opt
  where opt = setRates 192000 64 <>
              def {
                csdFlags = def {
                  audioFileOutput = def {
                    formatSamples = Just Bit24
                  }
                }
              }

wind :: SE Sig
wind = fmap (osc . (\x -> 220 + 88 * (osc x + x))) pink
