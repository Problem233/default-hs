module Xenharmonic (tet, freeScan, freeRel) where

import Csound.Base
import Lib

tet :: D -> Score -> Score
tet steps = fmap $ \(amp, exp) -> (amp, 440 * 2 ** (exp / steps))

freeScan :: D -> [Note] -> [Note]
freeScan = scanl (\(_, f) (amp, mult) -> (amp, f * mult)) . (0,)

freeRel :: D -> Score -> Score
freeRel x = fmap $ \(amp, mult) -> (amp, x * mult)
