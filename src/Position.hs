module Position where

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Position = Position { line :: !Int, col :: !Int }
  deriving (Eq, Ord, Show)

data Range
  = Range { from   :: !Position
          , to     :: !Position
          , source :: FilePath }
  deriving (Eq, Show)

data Located a = Located { srcRange :: !Range, thing :: a }
  deriving (Eq, Show)

emptyRange :: Range
emptyRange = Range { from = start, to = start, source = "" }

start :: Position
start = Position { line = 1, col = 1 }

move :: Position -> Char -> Position
move p c = case c of
  '\t' -> p { col = ((col p + 7) `div` 8) * 8 + 1 }
  '\n' -> p { col = 1, line = 1 + line p }
  _    -> p { col = 1 + col p }

rComb :: Range -> Range -> Range
rComb r1 r2  = Range { from = rFrom, to = rTo, source = source r1 }
  where rFrom = min (from r1) (from r2)
        rTo   = max (to r1)   (to r2)

rCombs :: [Range] -> Range
rCombs = foldl1 rComb

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

instance Functor Located where
  fmap f l = l { thing = f (thing l) }

class HasLoc t where
  getLoc :: t -> Maybe Range

instance HasLoc Range where
  getLoc = Just

instance HasLoc (Located a) where
  getLoc r = Just (srcRange r)

instance (HasLoc a, HasLoc b) => HasLoc (a, b) where
  getLoc (x, y) =
    case getLoc x of
      Nothing -> getLoc y
      Just l1 ->
        case getLoc y of
          Nothing -> return l1
          Just l2 -> return (rComb l1 l2)

instance HasLoc a => HasLoc [a] where
  getLoc = go Nothing
    where
    go x [] = x
    go Nothing (x : xs)  = go (getLoc x) xs
    go (Just l) (x : xs) = case getLoc x of
                             Nothing -> go (Just l) xs
                             Just l1 -> go (Just (rComb l l1)) xs

class HasLoc t => AddLoc t where
  addLoc  :: t -> Range -> t
  dropLoc :: t -> t

instance AddLoc (Located a) where
  addLoc t r = t { srcRange = r }
  dropLoc r  = r

at :: (HasLoc l, AddLoc t) => l -> t -> t
at l e = maybe e (addLoc e) (getLoc l)
