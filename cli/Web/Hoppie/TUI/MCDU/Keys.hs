module Web.Hoppie.TUI.MCDU.Keys
where

data LSK
  = LSKL Int
  | LSKR Int
  deriving (Show, Read, Eq)

instance Enum LSK where
  toEnum i
    | even i
    = LSKL (i `div` 2)
    | otherwise
    = LSKR (i `div` 2)
  fromEnum (LSKL i) = i * 2
  fromEnum (LSKR i) = i * 2 + 1

instance Ord LSK where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Bounded LSK where
  minBound = LSKL 0
  maxBound = LSKR 5

numLSKs :: Int
numLSKs = 6

data FunctionKey
  = PageUp
  | PageDown
  | DEL
  | CLR
  | Menu
  | DLK
  | ATC
  | FPL
  | RTE
  | NAV
  | PROG
  | INIT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data MCDUKey
  = MCDUChar Char
  | MCDULSK LSK
  | MCDUFunction FunctionKey
  deriving (Show, Read, Eq, Ord)

