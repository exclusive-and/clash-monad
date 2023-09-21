
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Synchronous ( Synchronous (..) ) where

import Data.Kind
import GHC.Generics


-- | Clock-synchronized state semantics.
class Synchronous a where
    type OutToken (a :: Type)
    type OutToken a = GOutToken (Rep a)
    type InToken  (a :: Type)
    type InToken  a = GInToken  (Rep a)

    outToken :: a -> OutToken a
    inToken  :: a -> InToken  a
    
    default outToken
        :: (Generic a, GSynchronous (Rep a), OutToken a ~ GOutToken (Rep a))
        => a -> OutToken a
    
    default inToken
        :: (Generic a, GSynchronous (Rep a), InToken  a ~ GInToken  (Rep a))
        => a -> InToken  a
    
    outToken = gOutToken . from
    inToken  = gInToken  . from
    
    -- | Clock ticks apply the write token to modify the read token.
    tick  :: a -> OutToken a -> InToken a -> IO ()

    default tick
        :: (Generic a, GSynchronous (Rep a))
        => a -> OutToken a -> InToken a -> IO ()

    tick a _rtok _wtok = gTick g (gOutToken g) (gInToken g)
        where
        g = from a
    
    -- | Resets the read and write tokens to their initial states.
    reset :: a -> OutToken a -> InToken a -> IO ()
    
    default reset
        :: (Generic a, GSynchronous (Rep a))
        => a -> OutToken a -> InToken a -> IO ()
    
    reset a _rtok _wtok = gReset g (gOutToken g) (gInToken g)
        where
        g = from a


class GSynchronous g where
    data GOutToken (g :: Type -> Type)
    data GInToken  (g :: Type -> Type)
    
    gOutToken :: g p -> GOutToken g
    gInToken  :: g p -> GInToken  g
    
    gTick  :: g p -> GOutToken g -> GInToken g -> IO ()
    
    gReset :: g p -> GOutToken g -> GInToken g -> IO ()

instance Synchronous a => GSynchronous (K1 i a) where
    data GOutToken (K1 i a) = GOut1 (OutToken a)
    data GInToken  (K1 i a) = GIn1  (InToken  a)
    
    gOutToken (K1 x) = GOut1 (outToken x)
    gInToken  (K1 x) = GIn1  (inToken  x)
    
    gTick (K1 x) (GOut1 rtok) (GIn1 wtok) = tick x rtok wtok
    
    gReset (K1 x) (GOut1 rtok) (GIn1 wtok) = reset x rtok wtok

instance GSynchronous a => GSynchronous (M1 i c a) where
    data GOutToken (M1 i c a) = GOutM1 (GOutToken a)
    data GInToken  (M1 i c a) = GInM1  (GInToken  a)
    
    gOutToken (M1 x) = GOutM1 (gOutToken x)
    gInToken  (M1 x) = GInM1  (gInToken  x)
    
    gTick (M1 x) (GOutM1 rtok) (GInM1 wtok) = gTick x rtok wtok
    
    gReset (M1 x) (GOutM1 rtok) (GInM1 wtok) = gReset x rtok wtok

instance (GSynchronous a, GSynchronous b) => GSynchronous (a :*: b) where
    data GOutToken (a :*: b) = GOut2 (GOutToken a) (GOutToken b)
    data GInToken  (a :*: b) = GIn2  (GInToken  a) (GInToken  b)
    
    gOutToken (a :*: b) = GOut2 (gOutToken a) (gOutToken b)
    gInToken  (a :*: b) = GIn2  (gInToken  a) (gInToken  b)
    
    gTick (a :*: b) (GOut2 rtoka rtokb) (GIn2 wtoka wtokb) = do
        gTick a rtoka wtoka
        gTick b rtokb wtokb

    gReset (a :*: b) (GOut2 rtoka rtokb) (GIn2 wtoka wtokb) = do
        gReset a rtoka wtoka
        gReset b rtokb wtokb


