module Robot where

import Data.Array
import Data.List
import Control.Monad
import Control.Applicative
import System.IO

type Position = (Int,Int)

type Grid = Array Position [Direction]

data Direction = North | Ease | South | West
      deriving (Eq,Show,Enum)

data Color = Black | Blue | Green | Cyan
           | Red | Magenta | Yellow | White
      deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

data RobotState
   = RobotState
         { position  :: Position
         , facing    :: Direction
         , pen       :: Bool 
         , color     :: Color
         , treasure  :: [Position]
         , pocket    :: Int
         }
      deriving Show

newtype Robot a
  = Robot (RobotState)

right :: Direction -> Direction
right d = toEnum $ succ (fromEnum d) `mod` 4

left :: Direction -> Direction
left d = toEnum $ pred (fromEnum d) `mod` 4

updateState :: (RobotState -> RobotState) -> Robot
updateState u = Robot (\s -> return (u s, ()))

-- turnLeft :: Robot
-- turnLeft = updateState (\s -> s {facing=left (facing s)})

-- turnRight :: Robot
-- turnRight = updateState (\s -> s {facing=right (facing s)})

-- testFunc :: RobotState -> RobotState
-- testFunc = (\s -> s {facing=right (facing s)})