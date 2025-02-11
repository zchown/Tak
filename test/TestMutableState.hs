module MutableState where

import qualified Board as B
import Data.IORef (IORef, readIORef)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import MutableState
import PTN
import TPS
