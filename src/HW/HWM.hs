
{-# LANGUAGE MagicHash #-}

module HW.HWM where

import Data.Free
import GHC.Generics
import Silicon
import System.IO.Unsafe


-- * HWM

-- | @'HWM' a@ describes a computation that results in a combinational
-- value of @a@ and some actions on latches.
newtype HWM a = HWM (Free Action a)

instance Functor HWM where
    fmap = mapHW#

mapHW# :: (a -> b) -> HWM a -> HWM b
{-# NOINLINE mapHW# #-}
mapHW# f (HWM ma) = HWM $ f <$> ma

instance Applicative HWM where
    pure  = pureHW#
    (<*>) = appHW#

pureHW# :: a -> HWM a
{-# NOINLINE pureHW# #-}
pureHW# = HWM . pure

appHW# :: HWM (a -> b) -> HWM a -> HWM b
{-# NOINLINE appHW# #-}
appHW# (HWM mf) (HWM mx) = HWM $ mf <*> mx

instance Monad HWM where
    return = pure
    (>>=)  = bindHW#

bindHW# :: HWM a -> (a -> HWM b) -> HWM b
{-# NOINLINE bindHW# #-}
bindHW# (HWM ma) k = HWM $ do
    a <- ma
    let HWM mb = k a in mb


-- | Run the computation described by 'HWM'. Not synthesizeable.
runHW :: HWM a -> IO a
runHW (HWM m) = go m
    where
    go (Pure  a) = pure a
    go (Free fa) = case fa of
        ActIO   act -> act >>= go
        Read  s act -> act (readToken  s) >>= go
        Write s act -> act (writeToken s) >>= go


-- * Actions on Latches

-- | 
class ClockSensitive hwthing => Latch hwthing where
    type RTok hwthing
    readToken :: hwthing -> RTok hwthing

    type WTok hwthing
    writeToken :: hwthing -> WTok hwthing


-- | An action on a clock-sensitive latch. Not synthesizeable.
data Action a where
    -- | 'IO' escape hatch, needed for some simulation features.
    ActIO :: IO a -> Action a
    -- | Submit a read request to the latch.
    Read  :: Latch hw => hw -> (RTok hw -> IO a) -> Action a
    -- | Submit a write request to the latch.
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


-- * Mealy Machines

type Mealy i o = Signal Bool -> Signal Bool -> Signal i -> Signal o

-- | Create a synchronous circuit from an 'HWM' description of a Mealy
-- machine.
mealyHW
    :: ClockSensitive hwthing
    => HWM hwthing
    -> (hwthing -> i -> HWM o)
    -> Mealy i o

{-# NOINLINE mealyHW #-}

mealyHW ms mf !_ rs0 inputs =
    unsafePerformIO (runHW ms >>= go rs0 inputs)
    where
    go (r :- rs1) (x :- xs) !s =
        (:-) <$> goOnce r x s <*> unsafeInterleaveIO (go rs1 xs s)

    goOnce r x s = do
        o <- runHW (mf s x)
        if r then reset s else tick s
        pure o


-- * Clock Sensitivity

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
