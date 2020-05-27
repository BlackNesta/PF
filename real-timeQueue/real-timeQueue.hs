-- | Abstracts the implementation details of a single-insertion, single-extraction queuelike structure with amortised complexity.
module Real.Time.Queue where
data Queue a = Queue [a] [a] deriving (Show, Eq)

-- | A generic type class encapsulating a generic queuelike structure, that supports single-insertion and single-extraction
class QueueOperations q where
    -- | Constructs an empty queue.
    newQ :: q a
    -- | Checks if the queue is empty.
    empty :: q a -> Bool
    -- | Inserts a single element into the 'Queue'.
    push :: q a -> a -> q a
    -- | Attempts to extract an element from the 'Queue'. 'Queue' must not be empty!
    pop :: q a -> q a
    -- | Gets the element that will next be extracted from the 'Queue'. 'Queue' must not be empty!
    front :: q a -> a
    -- | Returns the elements of the 'Queue' in reverse order.
    rev :: q a -> q a
    -- | Gets the size of the queue
    size :: q a -> Int

instance QueueOperations Queue where
    empty (Queue [] []) = True
    empty _ = False
    newQ = Queue [] []
    push (Queue xs ys) x = Queue xs (x : ys)
    pop (Queue [] ys) = pop (Queue (reverse ys) [])
    pop (Queue (x : xs) ys) = Queue xs ys
    front (Queue [] ys) = front (Queue (reverse ys) [])
    front (Queue (x : xs) ys) = x
    rev (Queue xs ys) = Queue ys xs
    size (Queue xs ys) = (length xs) + (length ys)
