
module HW
    (
    -- * Hardware Monad
      HWM, Mealy, mealyHW, ClockSensitive
    -- * Register Primitives
    , Register, mkReg, readReg, writeReg
    -- * RAM Primitives
    , AsyncRam, mkRam, readRam, writeRam
    ) where

import HW.HWM
import HW.AsyncRAM
import HW.Register
