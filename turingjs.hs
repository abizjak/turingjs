{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Haste

import Haste.Graphics.Canvas

import Data.IORef

import Data.Char (isAlphaNum, isSpace, isAlpha)

-- TODO: add special halt state

import qualified Data.Map.Strict as Map

import qualified Data.List as List

data Move = L | R | S deriving (Show, Eq, Ord)

newtype State = State String deriving (Show, Eq, Ord)

data Character = Character !Char | Blank deriving (Show, Eq, Ord)

type Transition = (State, Character, State, Character, Move)

type Transitions = [Transition]

data Pos = Pos !Int !Int

data ParseState = ParseState !String !Pos

prependPos (Pos x y) s = "At line " ++ show x ++ ", column " ++ show y ++ ": " ++ s

parseError p s = Left $ prependPos p s

dropP :: (Char -> Bool) -> ParseState -> ParseState
dropP p (ps@(ParseState (s:ss) (Pos x y))) | p s = 
  if s == '\n' then dropP p (ParseState ss (Pos (x + 1) 0)) 
  else dropP p (ParseState ss (Pos x (y + 1)))
dropP _ ps = ps

spanP :: (Char -> Bool) -> ParseState -> (String, ParseState)
spanP p ps@(ParseState (s:ss) (Pos x y)) | p s =
          let (ys,zs) = if s == '\n' then spanP p (ParseState ss (Pos (x + 1) 0))
                        else (spanP p (ParseState ss (Pos x (y + 1)))) in 
              (s : ys, zs)
spanP _ ps              =  ([], ps)

separator :: ParseState -> ParseState
separator = dropP (\x -> x == ',' || isSpace x)

state :: ParseState -> Either String (State, ParseState)
state s@(ParseState _ p) = 
    let (q, r) = spanP isAlphaNum s in
    case q of 
      [] -> parseError p "State has to consist of at least one alphanumeric character." 
      _ -> return (State q, r)

move :: ParseState -> Either String (Move, ParseState)
move (ParseState (h : t) p@(Pos x y)) = 
  case h of
    '<' -> return (L, ParseState t (Pos x (y + 1)))
    '>' -> return (R, ParseState t (Pos x (y + 1)))
    '!' -> return (S, ParseState t (Pos x (y + 1)))
    _ -> parseError p "Move has to be one of `<', `>' or `!'."
move (ParseState _ p) = parseError p "Empty input. Expecting `<', `>' or `!'."
 
character :: ParseState -> Either String (Character, ParseState)
character (ParseState (h:s) p@(Pos x y)) =
    if isAlphaNum h then return (Character h, ParseState s (Pos x (y + 1)))
    else if h == '_' then return (Blank, ParseState s (Pos x (y + 1)))
         else (parseError p $ "Expecting character, got `" ++ [h] ++ "'")

end :: ParseState -> Either String ParseState
end ps@(ParseState (h : s) _) | isSpace h = return $! dropP isSpace ps
end (ParseState _ p) = parseError p "Transitions have to be separated by at least one whitespace characer."

transition :: ParseState -> Either String (Transition, ParseState)
transition s = do
      (q, s1) <- state s
      (c, s2) <- character (separator s1)
      (q', s3) <- state (separator s2)
      (c', s4) <- character (separator s3)
      (m, s5) <- move (separator s4)
      return ((q, c, q', c', m), s5)

transitions :: String -> Either String Transitions
transitions = go . dropP isSpace . \x -> ParseState x (Pos 1 0)
  where go (ParseState [] _) = return []
        go xs = do (t, xs') <- transition xs
                   case xs' of 
                     (ParseState [] _) -> return [t]
                     _ -> do rest <- end xs'
                             ts <- go rest
                             return (t : ts)

parseTransitions :: String -> Either String Transitions
parseTransitions s = case transitions s of 
                       Left x -> Left x
                       Right ts -> return ts

parseInput :: String -> Either String [Character]
parseInput = Right . List.map (\c -> if c == ' ' || c == '_' then Blank else Character c)

type Delta = Map.Map (State, Character) (State, Character, Move)

buildMap :: Transitions -> Delta
buildMap = List.foldl' (\map (s, c, s', c', m) -> Map.insert (s, c) (s', c', m) map) Map.empty

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
    (color (RGB 255 255 255) . fill $! rct) >> (color clr . stroke $! rct)
  else (color clr . stroke $! rct)

drawChar (x, y) (Character c) = font "20px Bitstream Vera" $! text (x-7, y+7) [c]
drawChar (x, y) Blank = font "20px Bitstream Vera" $! text (x-7, y+7) " "

joinSquareSymbol [] [] = []
joinSquareSymbol (x:xs) [] = (x, Blank) : joinSquareSymbol xs []
joinSquareSymbol (x:xs) (c:cs) = (x, c) : joinSquareSymbol xs cs
joinSquareSymbol [] _ = []

drawConfig (x, y) minx maxx (pos, ls, State q, rs) =
  let centersl = List.map (\x -> (x, y)) $! take (fromIntegral pos) (takeWhile (>= minx) (iterate (\x -> x - 2 * squareSize) (x - 2 * squareSize))) 
      centersr = List.map (\x -> (x, y)) $! takeWhile (<= maxx) (iterate (+ (2 * squareSize)) x)
      centers = reverse centersl ++ centersr in
  do mapM_ (\(xc, yc) -> drawTapeSquare (x == xc) (xc, yc)) centers
     mapM_ (uncurry drawChar) $! joinSquareSymbol centersl ls
     mapM_ (uncurry drawChar) $! joinSquareSymbol centersr rs
     font "20px Bitstream Vera" $! text (x, y + 4 * squareSize) ("In state " ++ q)
     
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
  where updateCanvas !maxx !maxy !cnvs = do
          readIORef cfg >>= render cnvs . drawConfig (maxx `div` 2, maxy `div` 2) 0 maxx

        newParagraph !s = do
          np <- newElem "p"
          setProp np "innerHTML" s
          return np

        setLog !s = do
          np <- newParagraph s
          setChildren log [np]

        setLogs !s = setCurrent s >> setLog s

        setCurrent !s = do 
          np <- newParagraph s
          setChildren curr [np]

        addToLog !s = do
          nplog <- newParagraph s
          addChild nplog log
          npcurr <- newParagraph s
          setChildren curr [npcurr]

        readInputs !halted !update = do
          Just !trans <- getValue imachine
          Just !inpt <- getValue input

          case parseTransitions trans of
            Left pe -> setLogs $! show pe
            Right [] -> setLogs $! "Machine has to have at least one transition."
            Right (!ptrans@((q,_,_,_,_) : _)) ->
              do 
                 writeIORef machine (buildMap ptrans)
                 setLogs ("Successfully read machine transitions. All " ++ show (length ptrans) ++ " of them.")                    
                 case parseInput inpt of 
                   Left pe -> setLogs (show pe)
                   Right pinpt -> do writeIORef halted False
                                     writeIORef cfg (0, [], q, pinpt)
                                     addToLog "Successfully read input."
                                     update

        makeStep !halted !update = do
          h <- readIORef halted
          if (not h) then do
            delta <- readIORef machine
            config <- readIORef cfg
            case step delta config of
              OK (config@(_, ls, State q, rs)) 
                -> writeIORef cfg config >> update >>
                   (addToLog $!
                                 ("Current config: " ++ 
                                 (List.reverse $! charsToString ls) ++
                                 "(" ++ q ++ ")" ++ (charsToString rs)))
              NoTransition (_, ls, State q, rs) 
                -> do writeIORef halted True
                      addToLog $! ("Halted in state: " ++ 
                                 (List.reverse $! charsToString ls) ++
                                "(" ++ q ++ ")" ++ (charsToString rs))
              MovedOffTape (_, ls, State q, rs) 
                -> do writeIORef halted True
                      addToLog $! ("Moved off tape on the left in state: " ++ 
                                 (List.reverse $! charsToString ls) ++
                                 "(" ++ q ++ ")" ++ (charsToString rs))
            else setCurrent "Machine halted!"

        left !update = do
          (pos, ll, q, lr) <- readIORef cfg
          if pos > 0 then
            (case ll of
               [] -> writeIORef cfg (pos-1, [], q, Blank : lr)
               (l : ll) -> writeIORef cfg (pos-1, ll, q, l : lr)) >> update
          else return ()

        right !update = do
          (pos, ll, q, lr) <- readIORef cfg
          (case lr of
              [] -> writeIORef cfg (pos+1, Blank : ll, q, [])
              (r : lr) -> writeIORef cfg (pos+1, r : ll, q, lr)) >> update
        
        runWithPause !delay !halted !update = go
          where go = do h <- readIORef halted
                        if not h then do
                          makeStep halted update
                          setTimeout delay $! go
                        else return ()
  
charsToString :: [Character] -> String
charsToString = List.map (\x -> case x of 
                                  Character c -> c
                                  Blank -> '_')

paragraphize s = "<p>" ++ s ++ "</p>"
