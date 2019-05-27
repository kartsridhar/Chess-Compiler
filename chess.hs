import Text.Yoda
import Data.Monoid

data Chess = Turn Move Move Chess -- simply takes a turn
           | EndGame              -- or ends the game
   deriving Show

data Move = Move MoveP Quant      -- makes a move with quantifiers
          | Cslt Bool             -- checks if a castle move has been made
          | End Winner            -- result of the game
   deriving Show

data Quant = Prom Piece Quant     -- states what piece a pawn is being promoted to
           | Chck Quant           -- checks if a check has been made
           | Null
   deriving Show

data MoveP = Alg Piece Cell       -- basic piece being called and its resulting Cell
           | Smh Cell Cell        -- Takes the original and final cell after move
           | AlgDis Piece Cell Cell -- unambiguous call of Alg
           | Tke Piece Cell       -- quantifies that a piece has been taken
   deriving Show

data Winner = White
            | Black
            | Draw
            | AO
   deriving Show

data Cell = Cell Char Int         -- takes in the position letter and digit
   deriving (Show, Eq)

data Piece = King
           | Queen
           | Rook
           | Knight
           | Bishop
           | Pawn
   deriving (Show, Eq)
   
