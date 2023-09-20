
module Examples.Blink where

import Clash.Monad

import Silicon


topEntity
    :: Signal Bool
    -> Signal Bool
    -> Signal ()
    -> Signal Bool

topEntity = mealyClash (mkReg False) blink

blink :: () -> Clash (Register Bool) Bool
blink () = do
    rx <- get
    x <- readReg rx
    case x of
        True  -> writeReg rx False
        False -> writeReg rx True
    pure x
    
get :: Clash s s
get = Clash $ \s -> pure s
