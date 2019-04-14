module Scores where

import Lib
import Xenharmonic

twinkle :: Score
twinkle = bpm 100 $ mel $ notes <$> [pA, pB, pA]
  where pA = "4C 4C | 4G 4G | 4A 4A | 4G~ - | 4F 4F | 4E 4E | 4D 4D | 4C~ - ||"
        pB = "4G 4G | 4F 4F | 4E 4E | 4D~ - | 4G 4G | 4F 4F | 4E 4E | 4D~ - ||"

test1 :: Score
test1 = bpm 100 $ notes
  "3A^ 3B^ 4C~~ - - | 3A^ 3B^ 4C~~ - - | 3B^ 4C^ 4D~~ - - | 4C^ 3B^ 3A~~ - - ||"

test2 :: Score
test2 = bpm 160 $ notes $ unwords [
  "4F 4C^ 4F 5C^ 0^ 4B~ 4A^~~~~ |",
  "4E^ 4F^ 4G^ 4F^ 0^ 4E^ 0^ 4D^ 0^ 4E^ 0^ 4F. 4G ||"]

test3 :: Score
test3 = bpm 120 $ notes $ unwords [
  "4C^ 3B^ | 4C~ - 3F 4C | 4D 4C^ 3B 4C. | 3A~~~ - - - |",
  "0 0 0 3A^ 3B^ | 4C~ - 3B 4C | 4D 4C^ 4D 4F. | 4E 4D^ 4C^~~~~ - - |",
  "0 0 4C^ 4D^ 4E | 4E 4D^ 4E^~~~~~~ - - | - 4D 4E 4A |",
    "4G 4E^^ 4D^^ 4C^~~~~ - - |",
  "0 0 0 4C^ 3B^ | 3A^~~~~ 4C^ 4E | 4D 4C^ 3B 4C. | 3A~~~ - - - ||"]

-- suggested patches:
-- noisyChoir, caveOvertonePad, sunriseAccordeon, bhumiLofi 80
test4 :: Score
test4 = shiftPitch (-5) $ bpm 160 $ mel $ notes <$> [pA, pB, pA, pC]
  where
    pA = "4A. 4A. 4A | 4G. 4G. 4G | 4A. 4A. 4A | 4G. 4G. 4G ||"
    pB = "5C. 5C. 5C | 4A. 4A. 4A | 4G. 4G. 4G | 4F. 4F. 4E ||"
    pC = "5C. 5C. 5C | 4A. 4A. 4A | 5C. 5C. 5C | 5D. 5D. 5D ||"

test5 :: Score
test5 = bpm 140 $ mel [
          pA, notes "0^ 4E^", pB, notes "4G^ 4A^",
          pB, notes "0^ 4A^", pA, notes "4D^ 4C^"]
  where pA = mel [note "4C^", noteAmp 0.3 "4E^", loopBy 6 $ notes "0^ 4E^"]
        pB = mel [note "4D^", noteAmp 0.3 "4F^", loopBy 6 $ notes "0^ 4F^"]

test6 :: Score
test6 = freeRel 440 $ bpm 80 $ mel [pA, pB]
  where pA = mel [melNotes [1, 3/2, 4/3, 3/2, 2, 27/16, 3/2], rest 1]
        pB = mel [str (1/2) $ melNotes [6/5, 4/3],
                  melNotes [3/2, 4/3],
                  str (1/2) $ melNotes [1, 4/3],
                  melNotes [27/16, 16/9, 3/2],
                  rest 1]
