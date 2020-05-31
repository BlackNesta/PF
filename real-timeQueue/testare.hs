import Test.QuickCheck
-- | Implementarea unei cozi cu complexitate amortizata
data Queue a = Queue [a] [a] deriving(Show)

instance Eq a => Eq (Queue a) where
    (==) (Queue [] []) (Queue [] []) = True
    (==) q1 q2 = do
        if (size q1) /= (size q2) then False
        else if (front q1) /= (front q2) then False
        else (==) (pop q1) (pop q2) 

-- | Constructs an empty queue.
newQ :: Queue a
newQ = Queue [] []
-- | Checks if the queue is empty.
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False
-- | Inserts a single element into the 'Queue'.
push :: Queue a -> a -> Queue a 
push (Queue xs ys) x = Queue xs (x : ys)
-- | Attempts to extract an element from the 'Queue'. 'Queue' must not be empty!
pop :: Queue a -> Queue a
pop (Queue [] ys) = pop (Queue (reverse ys) [])
pop (Queue (x : xs) ys) = Queue xs ys
-- | Gets the element that will next be extracted from the 'Queue'. 'Queue' must not be empty!front :: Queue a -> a
front (Queue [] ys) = front (Queue (reverse ys) [])
front (Queue (x : xs) ys) = x
-- | Returns the elements of the 'Queue' in reverse order.
rev :: Queue a -> Queue a
rev (Queue xs ys) = Queue ys xs
-- | Gets the size of the queue
size :: Queue a -> Int
size (Queue xs ys) = (length xs) + (length ys)

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do
        xs <- arbitrary
        ys <- arbitrary
        return (Queue xs ys)

valempty :: Queue a -> Bool -> Bool
valempty (Queue [] []) True = True
valempty (Queue [] []) False = False
valempty (Queue xs ys) True = False
valempty (Queue xs ys) False = True

testempty :: Eq a => Queue a -> Bool
testempty q = valempty q (empty q)

testpush1 :: Eq a => Queue a -> a -> Bool
testpush1 q x = size q == (size (push q x)) - 1

testpush2 :: Eq a => Queue a-> a -> Bool
testpush2 q x = q == rev (pop (rev (push q x)))

testpop :: Eq a => Queue a -> Bool
testpop (Queue [] []) = True
testpop q = (size q) - 1== size (pop q)


testrev :: Eq a => Queue a-> Bool
testrev q = q == rev (rev q)

testfront1 :: Eq a => Queue a -> Bool
testfront1 (Queue [] []) = True
testfront1 q = do
    let f = front q
    if (front q == front (rev (push (rev (pop q)) f)) ) then True
    else False

testfront2 :: Eq a => Queue a -> Bool
testfront2 (Queue [] []) = True
testfront2 q = do
    let f = front q
    if (q == (rev (push (rev (pop q)) f))) then True
    else False

testsize :: Eq a => Queue a -> Bool
testsize (Queue xs ys) = size (Queue xs ys) == (length xs) + (length ys)
