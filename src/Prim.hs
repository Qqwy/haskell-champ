-- Contains some orphans that would be nicer to move to the `primitive` package
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Prim where

import Data.Primitive.Types (Prim(..))
import Data.Tuple (Solo(..))
import Data.Proxy (Proxy(Proxy))


instance Prim () where
    sizeOfType# _ = 0#
    alignmentOfType# _ = 1#
    indexByteArray# _arr _idx = ()
    readByteArray# _arr _idx s = (# s, () #)
    writeByteArray# _arr _idx () s = s
    setByteArray# _arr _offset _length () s = s
    indexOffAddr# _ _ = ()
    readOffAddr# _ _ s = (# s, () #)
    writeOffAddr# _ _ () s = s
    setOffAddr# _ _ _ () s = s

instance (Prim a) => Prim (Solo a) where
    sizeOfType# _ = sizeOfType# (Proxy @a)
    alignmentOfType# _ = alignmentOfType# (Proxy @a)
    indexByteArray# arr idx = MkSolo $ indexByteArray# arr idx
    readByteArray# arr idx s = 
        let !(# s', a #) = readByteArray# arr idx s 
        in (# s', MkSolo a #)
    writeByteArray# arr idx (MkSolo a) s = writeByteArray# arr idx a s
    setByteArray# arr offset len (MkSolo a) s = setByteArray# arr offset len a s
    indexOffAddr# addr offset = MkSolo $ indexOffAddr# addr offset
    readOffAddr# addr offset s = 
        let !(# s', a #) = readOffAddr# addr offset s
        in (# s', MkSolo a #)
    writeOffAddr# addr offset (MkSolo a) s = writeOffAddr# addr offset a s
    setOffAddr# addr offset len (MkSolo a) s = setOffAddr# addr offset len a s
