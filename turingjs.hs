module Main where

import Haste

import Haste.Graphics.Canvas

import Data.IORef

-- TODO: add special halt state

import Text.Parsec hiding (State)

import qualified Data.Map as Map

import qualified Data.List as List

data Move = L | R | S deriving (Show, Eq, Ord)

newtype State = State String deriving (Show, Eq, Ord)

data Character = Character !Char | Blank deriving (Show, Eq, Ord)

type Transition = (State, Character, State, Character, Move)

type Transitions = [Transition]

separator :: Monad m => ParsecT String u m String
separator = string "," <|> many1 space

state :: Monad m => ParsecT String u m State
state = many1 (letter <|> char '_' <|> digit) >>= return . State

move :: Monad m => ParsecT String u m Move
move = do c <- char '<' <|> char '>' <|> char '!' <?> "Move has to be one of '<', '>' or '!'."
          case c of
            '<' -> return L
            '>' -> return R
            '!' -> return S

character :: Monad m => ParsecT String u m Character
character = ((letter <|> digit) >>= return . Character) <|> (char '_' >> (return Blank))

end :: Monad m => ParsecT String u m String
end = many1 space

transition :: Monad m => ParsecT String u m Transition
transition =
  do s <- state
     separator
     c <- character
     separator
     s' <- state
     separator
     c' <- character
     separator
     m <- move
     return (s, c, s', c', m)

transitions :: Monad m => ParsecT String u m Transitions
transitions = spaces >> transition `sepEndBy` end

parseTransitions :: String -> Either ParseError Transitions
parseTransitions = runParser transitions () "machine box" 

parseInput :: String -> Either ParseError [Character]
parseInput = runParser inputParser () "input box"
  where inputParser = many (alphaNum <|> char ' ' <|> char '_') >>=
                       \cs -> return $ List.map (\c -> if c == ' ' || c == '_' then Blank else Character c) cs

type Delta = Map.Map (State, Character) (State, Character, Move)

buildMap :: Transitions -> Delta
buildMap = List.foldl (\map (s, c, s', c', m) -> Map.insert (s, c) (s', c', m) map) Map.empty

type Position = Integer

type Config = (Position, [Character], State, [Character])

data Result = OK !Config | MovedOffTape !Config | NoTransition !Config deriving (Show)

step :: Delta -> Config -> Result
step d cfg@(pos, lt, q, rt) =
  let (c, r) = case rt of
                 [] -> (Blank, [])
                 (x : xs) -> (x, xs)
  in case Map.lookup (q, c) d of
       Nothing -> NoTransition cfg
       Just (q', c', mv) -> case mv of
                              L -> if pos == 0 then MovedOffTape cfg else
                                   case lt of
                                     [] -> OK (pos - 1, [], q', Blank : c' : r)
                                     (lc : l) -> OK (pos - 1, l, q', lc : c' : r)
                              R -> OK (pos + 1, c' : lt, q', r)
                              S -> OK (pos, lt, q', c' : r)

run :: Delta -> Config -> Result
run d = go
  where go cfg = case step d cfg of
                   NoTransition cfg -> NoTransition cfg
                   MovedOffTape cfg -> MovedOffTape cfg
                   OK cfg -> go cfg

-- test = case (parseTransitions s, parseInput i) of
--          (Right cfg, Right inpt) -> run (buildMap cfg) (0, [], State "1", inpt)
--          _ -> undefined
main = do
  machine <- newIORef Map.empty
  input <- newIORef (0, [], State "", [])
  withElems ["read-button",
             "step-button",
             "run-button",
             "left-button",
             "right-button",
             "animation-canvas",
             "input-machine",
             "input",
             "log",
             "current-message"]
            (handler machine input)

squareSize = 10

drawTapeSquare highlight (x, y) = 
  let clr = if highlight then RGB 255 0 0 else RGB 0 0 0
      rct = rect (x - squareSize, y - squareSize) (x + squareSize, y + squareSize) in 
  if highlight then 
    (color (RGB 255 255 255) . fill $ rct) >> (color clr . stroke $ rct)
  else (color clr . stroke $ rct)

drawChar (x, y) (Character c) = font "20px Bitstream Vera" $ text (x-7, y+7) [c]
drawChar (x, y) Blank = font "20px Bitstream Vera" $ text (x-7, y+7) " "

joinSquareSymbol [] [] = []
joinSquareSymbol (x:xs) [] = (x, Blank) : joinSquareSymbol xs []
joinSquareSymbol (x:xs) (c:cs) = (x, c) : joinSquareSymbol xs cs
joinSquareSymbol [] _ = []

drawConfig (x, y) minx maxx (pos, ls, State q, rs) =
  let centersl = List.map (\x -> (x, y)) $ take (fromIntegral pos) (takeWhile (>= minx) (iterate (\x -> x - 2 * squareSize) (x - 2 * squareSize))) 
      centersr = List.map (\x -> (x, y)) $ takeWhile (<= maxx) (iterate (+ (2 * squareSize)) x)
      centers = reverse centersl ++ centersr in
  do mapM_ (\(xc, yc) -> drawTapeSquare (x == xc) (xc, yc)) centers
     mapM_ (uncurry drawChar) $ joinSquareSymbol centersl ls
     mapM_ (uncurry drawChar) $ joinSquareSymbol centersr rs
     font "20px Bitstream Vera" $ text (x, y + 4 * squareSize) ("In state " ++ q)
     
-- TODO, read widths of canvas as properties
handler :: IORef Delta -> IORef Config -> [Elem] -> IO ()
handler machine cfg [rb,sb,runb,leftb,rightb,canvas,imachine,input,log,curr] = do
    Just cnvs <- getCanvas canvas
    halted <- newIORef False
    maxy <- fmap read (getProp canvas "height") :: IO Int
    maxx <- fmap read (getProp canvas "width") :: IO Int
    onEvent rb OnClick (\_ _ -> readInputs halted (updateCanvas maxx maxy cnvs))
    onEvent sb OnClick (\_ _ -> makeStep halted (updateCanvas maxx maxy cnvs))
    onEvent runb OnClick (\_ _ -> runWithPause 500 halted (updateCanvas maxx maxy cnvs))
    onEvent leftb OnClick (\_ _ -> left (updateCanvas maxx maxy cnvs))
    onEvent rightb OnClick (\_ _ -> right (updateCanvas maxx maxy cnvs))
    return ()
  where updateCanvas maxx maxy cnvs = do
          readIORef cfg >>= render cnvs . drawConfig (maxx `div` 2, maxy `div` 2) 0 maxx

        newParagraph s = do
          np <- newElem "p"
          setProp np "innerHTML" s
          return np

        setLog s = do
          np <- newParagraph s
          setChildren log [np]

        setLogs s = setCurrent s >> setLog s

        setCurrent s = do 
          np <- newParagraph s
          setChildren curr [np]

        addToLog s = do
          nplog <- newParagraph s
          addChild nplog log
          npcurr <- newParagraph s
          setChildren curr [npcurr]

        readInputs halted update = do
          Just trans <- getValue imachine
          Just inpt <- getValue input

          case parseTransitions trans of
            Left pe -> setLogs $  show pe
            Right [] -> setLogs $ "Machine has to have at least one transition."
            Right (ptrans@((q,_,_,_,_) : _)) ->
              do 
                 writeIORef machine (buildMap ptrans)
                 setLogs ("Successfully read machine transitions. All " ++ show (length ptrans) ++ " of them.")                    
                 case parseInput inpt of 
                   Left pe -> setLogs (show pe)
                   Right pinpt -> do writeIORef halted False
                                     writeIORef cfg (0, [], q, pinpt)
                                     addToLog "Successfully read input."
                                     update

        makeStep halted update = do
          h <- readIORef halted
          if (not h) then do
            delta <- readIORef machine
            config <- readIORef cfg
            case step delta config of
              OK (config@(_, ls, State q, rs)) 
                -> writeIORef cfg config >> update >>
                   (addToLog $
                                 ("Current config: " ++ 
                                 (List.reverse $ charsToString ls) ++
                                 "(" ++ q ++ ")" ++ (charsToString rs)))
              NoTransition (_, ls, State q, rs) 
                -> do writeIORef halted True
                      addToLog $ ("Halted in state: " ++ 
                                 (List.reverse $ charsToString ls) ++
                                "(" ++ q ++ ")" ++ (charsToString rs))
              MovedOffTape (_, ls, State q, rs) 
                -> do writeIORef halted True
                      addToLog $ ("Moved off tape on the left in state: " ++ 
                                 (List.reverse $ charsToString ls) ++
                                 "(" ++ q ++ ")" ++ (charsToString rs))
            else setCurrent "Machine halted!"

        left update = do
          (pos, ll, q, lr) <- readIORef cfg
          if pos > 0 then
            (case ll of
               [] -> writeIORef cfg (pos-1, [], q, Blank : lr)
               (l : ll) -> writeIORef cfg (pos-1, ll, q, l : lr)) >> update
          else return ()

        right update = do
          (pos, ll, q, lr) <- readIORef cfg
          (case lr of
              [] -> writeIORef cfg (pos+1, Blank : ll, q, [])
              (r : lr) -> writeIORef cfg (pos+1, r : ll, q, lr)) >> update
        
        runWithPause delay halted update = go
          where go = do h <- readIORef halted
                        if not h then do
                          makeStep halted update
                          setTimeout delay $ go
                        else return ()

  
charsToString :: [Character] -> String
charsToString = List.map (\x -> case x of 
                                  Character c -> c
                                  Blank -> '_')

paragraphize s = "<p>" ++ s ++ "</p>"
