
module HW.Module where

import HW.HWM


-- | A combinational circuit with associated latches.
newtype HWST s a = HWST (s -> HWM a)

instance Functor (HWST s) where
    fmap f (HWST m) = HWST $ \s -> f <$> m s

instance Applicative (HWST s) where
    pure x = HWST $ \_ -> pure x
    
    HWST mf <*> HWST mx = HWST $ \s -> mf s <*> mx s

instance Monad (HWST s) where
    return = pure
    
    HWST mx >>= k = HWST $ \s -> do
        x <- mx s
        let HWST my = k x in my s


-- | Get the underlying combinational logic of a circuit.
runHWST :: HWST s a -> s -> HWM a
runHWST (HWST m) = m


-- | A self-contained hardware module.
-- 
-- The internal state of an encapsulated module should not be accessible
-- to any outside code.
-- 
-- The only exception where other modules can influence the module
-- state is when the influence is mediated by functions with the form
-- @
--  a -> ... -> 'HWM' b
-- @
-- where the internal state @s@ does /not/ appear in any of the arguments
-- or the result.
data HWMod a b where
    HWMod :: ClockSensitive s => HWM s -> (a -> HWST s b) -> HWMod a b

-- | Create a synchronous circuit from an encapsulated 'HWMod'.
mealyHWMod :: HWMod a b -> Mealy a b
mealyHWMod (HWMod s0 f) = mealyHW s0 (\s1 a -> runHWST (f a) s1)
