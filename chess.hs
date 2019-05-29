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

-- Parser function to get rid of extra spaces
whitespace :: Parser ()
whitespace = skip(many(oneOf[' ', '\t', '\n']))

-- Parser function to get one of the letters from a to h
posLetter :: Parser Char
posLetter = (oneOf['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']) <* whitespace

-- Parser function to get one of the digits from 1 to 9
posDigit :: Parser Int
posDigit = (many(oneOf['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])) <* whitespace

chessParser :: Parser Chess
chessParser = Turn <$ posDigit <*> moveParser <*> moveParser <*> chessParser
          <|> EndGame <$ string "gg"

moveParser :: Parser Move
moveParser = Move <$> movepParser <*> quantParser
         <|> Cslt <$> checkCastle
         <|> End <$> winnerParser

quantParser :: Parser Quant
quantParser = Prom <$> specialPieceParser <*> quantParser
          <|> Chck <$> char '+'
          <|> Null <$> char ' '

movepParser :: Parser MoveP
movepParser = Alg <$> pieceParser <*> cellParser
          <|> Smh <$> cellParser <*> cellParser
          <|> ALgDis <$> pieceParser <*> cellParser <*> cellParser
          <|> Tke <$> pieceParser <* char 'x' <*> cellParser

winnerParser :: Parser Winner
winnerParser = White <$ string "1-0"
           <|> Black <$ string "0-1"
           <|> Draw <$ string "1/2-1/2"
           <|> AO <$ char ' '

cellParser :: Parser Cell
cellParser = Cell <$> posLetter <*> posDigit

specialPieceParser :: Parser Piece
specialPieceParser = King <$ char 'k'
                 <|> Queen <$ char 'q'
                 <|> Rook <$ char 'r'
                 <|> Knight <$ char 'n'
                 <|> Bishop <$ char 'b'

pieceParser :: Parser Piece
pieceParser = specialPieceParser
          <|> Pawn <$ char ''
