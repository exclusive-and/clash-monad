
module HW.Register where

import HW.HWM
import Data.IORef


data Register a = Register a (IORef a) (IORef a)

instance ClockSensitive (Register a) where
    reset (Register rstval rtok wtok) = do
        writeIORef rtok rstval
        writeIORef wtok rstval
    
    tick (Register _ rtok wtok) = readIORef wtok >>= writeIORef rtok

instance Latch (Register a) where
    type RTok (Register a) = IORef a
    readToken (Register _ rtok _) = rtok

    type WTok (Register a) = IORef a
    writeToken (Register _ _ wtok) = wtok


mkReg :: a -> HWM (Register a)
mkReg rstval = hoistIO $ do
    rtok <- newIORef rstval
    wtok <- newIORef rstval
    pure $ Register rstval rtok wtok


readReg :: Register a -> HWM a
readReg reg = mkRead reg readIORef

writeReg :: Register a -> a -> HWM ()
writeReg reg x = mkWrite reg (\wtok -> writeIORef wtok x)
