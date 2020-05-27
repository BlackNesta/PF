-- | Implementarea unei cozi cu complexitate amortizata
module Queue where
data Queue a = Queue [a] [a] deriving (Show, Eq)

class QueueOperations q where
    -- | Construieste un coada vid
    newQ :: q a
    -- | Verifica daca coada este vida
    empty :: q a -> Bool
    -- | Introduce un element in coada.
    push :: q a -> a -> q a 
    -- | Scoate din coada elementul front. Atentie!!!: Coada nu trebuie sa fie goala
    pop :: q a -> q a
    -- | Ia cel mai vechi element introdus in coada fara a face modificari in coada. Atentie!!!: Coada nu trebuie sa fie goala
    front :: q a -> a
    -- | Inverseaza elementele din coada
    rev :: q a -> q a

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
