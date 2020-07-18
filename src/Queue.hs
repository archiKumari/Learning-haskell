module Queue where

data Queue a = Queue {
   qHead  :: Maybe a
  , qTail  :: [a]
  }
  deriving (Show, Eq)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue Nothing t) a = Queue (Just a) t
enqueue (Queue h t) a = Queue h (t ++ [a])

dequeue :: Queue a -> Either (Queue a) (a, Queue a)
dequeue q@(Queue Nothing _) = Left q
dequeue (Queue (Just a) t) = case t of
  [] -> Right (a, Queue Nothing [])
  xs -> Right (a, Queue (Just $ head xs) (drop 1 xs))

makeQueue :: [a] -> Queue a
makeQueue [] = Queue Nothing []
makeQueue xs = Queue (Just $ head xs) (tail xs)

isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue Nothing _) = True
isEmptyQueue _                 = False

bulkEnque :: Queue a -> [a] -> Queue a
bulkEnque q [] = q
bulkEnque q (x:xs) = bulkEnque (enqueue q x) xs


