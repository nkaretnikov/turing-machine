-- | Moves.
data Move = S -- ^ stay
          | L -- ^ go left
          | R -- ^ go right

-- | Symbols of the alphabet.
data Sym = Start -- ^ (⊛) start marker
         | Empty -- ^ (□) nothing, empty cell
         | Hash  -- ^ (#) failure; got no words, just 'Empty'
         | A     -- ^ symbol a
         | B     -- ^ symbol b

-- | A symbol at some location.
type SymLoc = (Sym, Int)

-- | A tape is just a zipper.
type Tape = ([SymLoc], SymLoc, [SymLoc])

-- | A command is the expected symbol, returning a pair: the resulting symbol
-- and the next move.
type Cmd = (SymLoc, (Sym,Move))

apply :: Cmd -> SymLoc -> (Sym,Move)
apply (es,el,(s,m)) (as,al) =
  if al /= el
  then error $ mconcat
    [ "expected location " <> show el
    , ", but got " <> show al
    ]
  else
    if es /= as
    then error $ mconcat
      [ "expected symbol " <> show es
      " , but got " <> show as
      ]
    else

-- | Apply a command to the current focus and move accordingly.
eval :: [Cmd] -> Tape -> Tape
eval []     t                       = t
eval (c:cs) (ls@(l:lt),x,rs@(r:rt)) =
  case apply c x of
    (y,S) -> eval cs (ls,y,rs)
    (y,L) -> eval cs (lt,l,y:rs)
    (y,R) -> eval cs (y:ls,r,rt)
