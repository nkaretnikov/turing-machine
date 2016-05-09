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
    if es /= as -- XXX: don't error out, just move to the next 'Move'
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

{-
q0 star -> q0 star, R
q0 box  -> qf hash, L
q0 a    -> q1 a, R
q0 b    -> q0' b, R  -- start state, non-empty word
q0' ... 
-}
