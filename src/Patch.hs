module Patch (sunriseAccordeon) where

import Csound.Base
import Csound.Patch

sunriseAccordeon :: Patch Sig2
sunriseAccordeon = accordeon' $ Accordeon 0.625 1 0.875 0.75
