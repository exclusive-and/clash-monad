
module Clash.Monad  ( Synchronous (..), Clash (..), mealyClash
                    , Register, mkReg, readReg, writeReg
                    , AsyncRam, mkAsyncRam, readRam, writeRam
                    ) where

import Data.Foldable
import Data.IORef
import Data.Kind
import Data.Primitive.Array
import GHC.Exts
import Silicon
import System.IO.Unsafe


-- | Clock-synchronized state semantics.
class Synchronous a where
    type OutToken (a :: Type)
    type InToken  (a :: Type)

    outToken :: a -> OutToken a
    inToken  :: a -> InToken  a
    
    -- | Clock ticks apply the write token to modify the read token.
    tick  :: a -> OutToken a -> InToken a -> IO ()

    -- | Resets the read and write tokens to their initial states.
    reset :: a -> OutToken a -> InToken a -> IO ()


-- | Experimental Clash Monad.
data Clash s a = Clash (s -> IO a)

instance Functor (Clash s) where
    fmap f (Clash m) = Clash $ \s -> f <$> m s

instance Applicative (Clash s) where
    pure a = Clash $ \_ -> pure a

    Clash mf <*> Clash mx = Clash $ \s -> mf s <*> mx s

instance Monad (Clash s) where
    return = pure

    Clash m >>= k = Clash $ \s -> do
        a <- m s
        unClash (k a) s

unClash :: Clash s a -> (s -> IO a)
unClash (Clash m) = m


mealyClash
    :: Synchronous s
    => Clash () s
    -> (input -> Clash s output)
    -> Signal Bool
    -> Signal Bool
    -> Signal input
    -> Signal output
{-# NOINLINE mealyClash #-}

mealyClash (Clash ms) f !_ rsts inputs =
    unsafePerformIO (ms () >>= go rsts inputs)
    where
    go ~(r :- rs) q@(~(x :- xs)) !s =
        (:-) <$> goOnce r x s
             <*> unsafeInterleaveIO (q `seq` go rs xs s)

    goOnce r x s = do
        o <- unClash (f x) s
        if not r
           then tick s (outToken s) (inToken s)
           else reset s (outToken s) (inToken s)
        pure o


data Register a = Register a (IORef a) (IORef a)

instance Show a => Synchronous (Register a) where
    type OutToken (Register a) = IORef a
    type InToken  (Register a) = IORef a

    outToken (Register _ rtok _) = rtok
    inToken  (Register _ _ wtok) = wtok
    
    tick _ rtok wtok = readIORef wtok >>= writeIORef rtok
    {-# NOINLINE tick  #-}

    reset (Register resetval _ _) rtok wtok = do
        writeIORef rtok resetval
        writeIORef wtok resetval
    {-# NOINLINE reset #-}

mkReg :: a -> Clash s (Register a)
{-# NOINLINE mkReg #-}

mkReg resetval = Clash $ \_ -> do
    rtok <- newIORef resetval
    wtok <- newIORef resetval
    pure $ Register resetval rtok wtok

writeReg :: Register a -> a -> Clash s ()
{-# NOINLINE writeReg #-}

writeReg (Register _ _rtok wtok) x = Clash $ \_ -> do
    writeIORef wtok x

readReg :: Register a -> Clash s a
{-# NOINLINE readReg #-}

readReg (Register _ rtok _wtok) = Clash $ \_ -> do
    readIORef rtok


data AsyncRam a =
    AsyncRam [a]
             (MutableArray RealWorld a)
             (IORef (Maybe (Int, a)))

instance Synchronous (AsyncRam a) where
    type OutToken (AsyncRam a) = MutableArray RealWorld a
    type InToken  (AsyncRam a) = IORef (Maybe (Int, a))

    outToken (AsyncRam _ rtok _) = rtok
    inToken  (AsyncRam _ _ wtok) = wtok
    
    tick _ rtok wtok = do
        w <- readIORef wtok
        case w of
            Just (i, x) -> writeArray rtok i x
            Nothing     -> pure ()
        writeIORef wtok Nothing
    {-# NOINLINE tick  #-}

    reset (AsyncRam resetval _ _) rtok wtok = do
        forM_ (zip [0..] resetval) $ \(i, x) -> writeArray rtok i x
        writeIORef wtok Nothing
    {-# NOINLINE reset #-}

mkAsyncRam :: [a] -> Clash s (AsyncRam a)
{-# NOINLINE mkAsyncRam #-}

mkAsyncRam resetval = Clash $ \_ -> do
    rtok <- newArray (length resetval) (resetval !! 0)
    forM_ (zip [0..] resetval) $ \(i, x) -> writeArray rtok i x
    wtok <- newIORef Nothing
    pure $ AsyncRam resetval rtok wtok

writeRam :: AsyncRam a -> Int -> a -> Clash s ()
{-# NOINLINE writeRam #-}

writeRam (AsyncRam _ _rtok wtok) i x = Clash $ \_ -> do
    writeIORef wtok $ Just (i, x)

readRam :: AsyncRam a -> Int -> Clash s a
{-# NOINLINE readRam #-}

readRam (AsyncRam _ rtok _wtok) i = Clash $ \_ -> do
    readArray rtok i
