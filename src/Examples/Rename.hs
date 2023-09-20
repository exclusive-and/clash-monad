
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples.Rename where

import Clash.Monad

import Silicon
import Data.Word


topEntity
    :: Signal Bool
    -> Signal Bool
    -> Signal Instruction
    -> Signal Renamed

topEntity = mealyClash mkRename rename

-- * Register Renamer State
---------------------------------------------------------------------

data Rename = Rename (Register Tag) (AsyncRam Tag)

instance Synchronous Rename where
    type OutToken Rename = (OutToken (Register Tag), OutToken (AsyncRam Tag))
    type InToken  Rename = (InToken  (Register Tag), InToken  (AsyncRam Tag))
    
    outToken (Rename reg ram) = (outToken reg, outToken ram)
    inToken  (Rename reg ram) = (inToken  reg, inToken  ram)
    
    tick (Rename reg ram) (reg_rtok, ram_rtok) (reg_wtok, ram_wtok) = do
        tick reg reg_rtok reg_wtok
        tick ram ram_rtok ram_wtok
    
    reset (Rename reg ram) (reg_rtok, ram_rtok) (reg_wtok, ram_wtok) = do
        reset reg reg_rtok reg_wtok
        reset ram ram_rtok ram_wtok

mkRename :: Clash s Rename
mkRename = do
    reg <- mkReg (Tag 0)
    ram <- mkAsyncRam (replicate 16 (Tag 0))
    pure $ Rename reg ram

-- * Register Renamer
---------------------------------------------------------------------
    
rename :: Instruction -> Clash Rename Renamed
rename (Instruction op rdest srca srcb) = do
    ift <- getRenameTable

    -- Rename the first source operand.
    psrca <- case srca of
        NoOperand       -> pure NoOperand
        ArchReg   ra    -> Tagged <$> readRam ift (fromEnum ra)
        Immediate imm   -> pure $ Immediate imm

    -- Rename the second source operand.
    psrcb <- case srcb of
        NoOperand       -> pure NoOperand
        ArchReg   rb    -> Tagged <$> readRam ift (fromEnum rb)
        Immediate imm   -> pure $ Immediate imm
    
    -- Rename the destination operand. Assigns the operation a new tag,
    -- then updates the rename table and tag allocator.
    tag <- getTagRegister
    pdest@(Tag rawtag) <- readReg tag
    
    writeRam ift (fromEnum rdest) pdest
    
    writeReg tag (Tag $ rawtag + 1)

    pure $ Renamed op pdest psrca psrcb

getTagRegister :: Clash Rename (Register Tag)
getTagRegister = Clash $ \(Rename reg _) -> pure reg

getRenameTable :: Clash Rename (AsyncRam Tag)
getRenameTable = Clash $ \(Rename _ ram) -> pure ram


-- * Instruction Set
---------------------------------------------------------------------

data Dc     -- ^ Decode stage.
data Rn     -- ^ Rename stage.

data Tag = Tag Int deriving Show

data Operand p
    = NoOperand
    | Register  (XRegister p)
    | Immediate Word64

deriving instance Show (XRegister stage) => Show (Operand stage)

pattern ArchReg :: Word8 -> Operand Dc
pattern ArchReg arch = Register arch

{-# COMPLETE NoOperand, ArchReg, Immediate #-}

pattern Tagged :: Tag -> Operand Rn
pattern Tagged dep = Register dep

{-# COMPLETE NoOperand, Tagged, Immediate #-}

type family XRegister stage where
    XRegister Dc = Word8
    XRegister Rn = Tag


data Opcode
    = LDI
    | Add
    | Sub
    deriving Show

data Instruction = Instruction Opcode Word8 (Operand Dc) (Operand Dc)

data Renamed = Renamed Opcode Tag (Operand Rn) (Operand Rn)
    deriving Show

