
module Examples.Blink where

import Clash.Monad

import Silicon


topEntity
    :: Signal Bool
    -> Signal Bool
    -> Signal ()
    -> Signal Int

topEntity = mealyClash (mkReg 0) blink

blink :: () -> Clash (Register Int) Int
blink () = do
    rx <- get
    x <- readReg rx
    writeReg rx $ x + 1
    pure x
    
get :: Clash s s
get = Clash $ \s -> pure s
