{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellClass where

data Type1 a = TC1 a
             | TC2 Int
             | TC3 String
  deriving (Show, Eq)

instance Functor Type1 where
  fmap :: (a->b) -> Type1 a -> Type1 b
  fmap f (TC1 a) = TC1 (f a)
  fmap _ (TC2 x) = TC2 x
  fmap _ (TC3 x) = TC3 x

data Type2 a = T a
  deriving(Show, Eq)

instance Functor Type2 where
  fmap :: (a->b) -> Type2 a -> Type2 b
  fmap f (T a) = T (f a)

fx :: Type1 a -> Type1 a
fx (TC2 x) = TC2 (x+x)
fx x       = x

fn :: Type1 a -> Type1 a
fn (TC2 x) = TC2 (5*x)
fn a       = a

func :: Int -> Type1 a -> Maybe Bool
func a (TC2 x) | x == a = Just True
               | otherwise = Just False
func a _ = Nothing

func1 :: Int -> Type1 a -> Maybe Bool
func1 a (TC2 x) | x > a = Just True
               | otherwise = Just False
func1 a _ = Nothing

data MyMaybe a = Null | Only a
  deriving (Show,Eq)

instance Functor MyMaybe where
  fmap :: (a->b) -> MyMaybe a -> MyMaybe b
  fmap f (Only a) = Only (f a)
  fmap f Null = Null

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure x = Only x

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (Only f) <*> (Only a) = Only (f a)
  _ <*> _ = Null

instance Semigroup a => Semigroup (MyMaybe a) where
  m1 <> m2 = (<>) <$> m1 <*> m2

instance Semigroup a => Monoid (MyMaybe a) where
  mempty = Null

instance Monad MyMaybe where
 (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
 (Only x) >>= f = f x
 _ >>= _ = Null

data MyList a = Empty
            | a ::: MyList a
  deriving Eq

instance Show a => Show (MyList a) where
  show :: MyList a -> String
  show ls = show $ toList ls

instance Functor MyList where
  fmap :: (a -> b) -> MyList a -> MyList b
  fmap _ Empty = Empty
  fmap f (x:::xs) = f x ::: fmap f xs

instance Applicative MyList where
  pure :: a -> MyList a
  pure x = x ::: Empty

  (<*>) :: MyList (a->b) -> MyList a -> MyList b
  f:::fs <*> ls = (fmap f ls) <> (fs <*> ls)
  _ <*> _ = Empty

instance Monad MyList where
 (>>=) :: MyList a -> (a -> MyList b) -> MyList b
 Empty >>= f = Empty
 (x:::xs) >>= f = f x <> (xs >>= f)

instance Semigroup (MyList a) where
 (<>) :: MyList a -> MyList a -> MyList a
 (x:::xs) <> ys = x::: (xs <> ys)
 xs <> Empty = xs
 Empty <> ys = ys

instance Semigroup a => Monoid (MyList a) where
  mempty = Empty

toList :: MyList a -> [a]
toList Empty = []
toList (x:::xs) = x: toList xs

countS :: String -> [String]
countS str = undefined

f1 :: Char -> [Char]
f1 'a' = "a"
f1 _   = []

f2 :: Char -> [Char]
f2 'b' = "b"
f2 _   = []




pc :: (Applicative f) => f [(a->Bool)] -> f [a] -> f Bool
pc funcs ls = fmap and $ (<*>) <$> funcs <*> ls

instance MonadFail (Either String) where
  fail s = Left s

fun :: MonadFail m => m Int -> m [()]
fun m = do
  n <- m
  let str = replicate n "xxx"
      res = if odd (length str) then fail "yeah extra mauj" else pure (length str)
  (\k -> replicate (k+n) ()) <$> res

fun' m = m >>= (\x -> pure $ replicate x "***") >>= (\x -> if (odd$length x) then Nothing else Just ())
