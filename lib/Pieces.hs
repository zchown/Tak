module Pieces where

data Stone
  = Flat
  | Standing
  | Cap
  deriving (Show, Eq)

type Stack = [Stone]

type Position = (Int, Int)
