
module HW.AsyncRAM where

import HW.HWM
import Data.IORef
import Data.Primitive.Array
import GHC.Exts


data AsyncRam a =
    AsyncRam (Array a)
             (MutableArray RealWorld a)
             (IORef (Maybe (Int, a)))

instance ClockSensitive (AsyncRam a) where
    reset (AsyncRam rstval arr wtok) = do
        copyArray arr 0 rstval 0 (length rstval)
        writeIORef wtok Nothing
    
    tick (AsyncRam _ arr wtok) = do
        w <- readIORef wtok
        case w of
            Just (i, x) -> writeArray arr i x
            Nothing     -> pure ()
        writeIORef wtok Nothing

instance Latch (AsyncRam a) where
    type RTok (AsyncRam a) = MutableArray RealWorld a
    readToken (AsyncRam _ rtok _) = rtok

    type WTok (AsyncRam a) = IORef (Maybe (Int, a))
    writeToken (AsyncRam _ _ wtok) = wtok


mkRam :: Array a -> HWM (AsyncRam a)
mkRam rstval = hoistIO $ do
    arr <- newArray (length rstval) undefined
    copyArray arr 0 rstval 0 (length rstval)
    wtok <- newIORef Nothing
    pure $ AsyncRam rstval arr wtok


readRam :: AsyncRam a -> Int -> HWM a
readRam ram i = mkRead ram (\rtok -> readArray rtok i)

writeRam :: AsyncRam a -> Int -> a -> HWM ()
writeRam ram i x = mkWrite ram (\wtok -> writeIORef wtok (Just (i, x)))

