
module HW.HWM where

import Data.Free
import GHC.Generics
import Silicon
import System.IO.Unsafe


-- | Hardware Monad: describes the intra-cycle combinational logic
-- and latch actions in a synchronous circuit.
newtype HWM a = HWM (Free Action a)
    deriving (Functor, Applicative, Monad)

-- | Compute some combinational logic. Collects all the latch actions
-- so that all of them happen at once on the next clock tick.
-- 
-- NOTE: Not synthesizeable.
runHW :: HWM a -> IO a
runHW (HWM m) = go m
    where
    go (Pure  a) = pure a
    go (Free fa) = case fa of
        ActIO   act -> act >>= go
        Read  s act -> act (readToken  s) >>= go
        Write s act -> act (writeToken s) >>= go


type Mealy i o = Signal Bool -> Signal Bool -> Signal i -> Signal o

-- | Create a synchronous circuit from a monadic description of a Mealy
-- machine's combinational logic.
mealyHW
    :: ClockSensitive hwthing
    => HWM hwthing
    -> (hwthing -> i -> HWM o)
    -> Mealy i o

mealyHW ms mf !_ rs0 inputs =
    unsafePerformIO (runHW ms >>= go rs0 inputs)
    where
    go (r :- rs1) (x :- xs) !s =
        (:-) <$> goOnce r x s <*> unsafeInterleaveIO (go rs1 xs s)

    goOnce r x s = do
        o <- runHW (mf s x)
        if r then reset s else tick s
        pure o


-- | 
class ClockSensitive hwthing where
    reset :: hwthing -> IO ()
    tick  :: hwthing -> IO ()
    
    default reset :: (Generic hwthing, GClockSensitive (Rep hwthing))
                  => hwthing -> IO ()
    default tick  :: (Generic hwthing, GClockSensitive (Rep hwthing))
                  => hwthing -> IO ()
    
    reset = greset . from
    tick  = gtick  . from

class GClockSensitive hwrep where
    greset :: hwrep x -> IO ()
    gtick  :: hwrep x -> IO ()


-- | Clock-sensitive components that have tokens for collecting read and
-- write actions generated during the clock cycle.
class ClockSensitive hwthing => Latch hwthing where
    type RTok hwthing
    readToken :: hwthing -> RTok hwthing

    type WTok hwthing
    writeToken :: hwthing -> WTok hwthing


-- | The three types of clock-synchronized actions are:
-- 
--  [@ActIO@]: @IO@ escape hatch for creating tokens in the software
--             simulation. Not synthesizeable!
--  
--  [@Read@]: Read from a component with only its read token.
--  
--  [@Write@]: Collect a write to the component with only its write token.
data Action a where
    ActIO :: IO a -> Action a
    Read  :: Latch hw => hw -> (RTok hw -> IO a) -> Action a
    Write :: Latch hw => hw -> (WTok hw -> IO a) -> Action a

instance Functor Action where
    fmap f = \case
        ActIO   act -> ActIO (f <$> act)
        Read  s act -> Read  s (fmap f . act)
        Write s act -> Write s (fmap f . act)

hoistIO :: IO a -> HWM a
hoistIO = HWM . Free . ActIO . fmap Pure

mkRead :: Latch hw => hw -> (RTok hw -> IO a) -> HWM a
mkRead s r = HWM $ Free $ Read s (fmap Pure . r)

mkWrite :: Latch hw => hw -> (WTok hw -> IO a) -> HWM a
mkWrite s w = HWM $ Free $ Write s (fmap Pure . w)


instance GClockSensitive V1 where
    greset = \case {}
    gtick  = \case {}

instance GClockSensitive U1 where
    greset U1 = pure ()
    gtick  U1 = pure ()

instance ClockSensitive hwthing => GClockSensitive (K1 i hwthing) where
    greset (K1 hwthing) = reset hwthing
    gtick  (K1 hwthing) = tick  hwthing

instance GClockSensitive hwrep => GClockSensitive (M1 i c hwrep) where
    greset (M1 a) = greset a
    gtick  (M1 a) = gtick  a

instance (GClockSensitive a, GClockSensitive b) => GClockSensitive (a :*: b)
    where
    greset (a :*: b) = greset a >> greset b
    gtick  (a :*: b) = gtick  a >> gtick  b

instance (GClockSensitive a, GClockSensitive b) => GClockSensitive (a :+: b)
    where
    greset (L1 a) = greset a
    greset (R1 b) = greset b
    gtick  (L1 a) = gtick  a
    gtick  (R1 b) = gtick  b
