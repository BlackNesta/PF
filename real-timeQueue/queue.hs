import Test.QuickCheck
import Text.Printf
import System.CPUTime
data Queue a = Queue [a] 
               deriving(Show, Eq)

newQ :: Queue a
empty :: Queue a -> Bool
push :: Queue a -> a -> Queue a 
pop :: Queue a -> Queue a
front :: Queue a -> a
rev :: Queue a -> Queue a
size :: Queue a -> Int

newQ = Queue []
empty (Queue []) = True
empty (Queue (hd : tl)) = False
push (Queue q) x = Queue (q ++ [x])
pop (Queue (hd : tl)) = Queue tl
front (Queue (hd : tl)) = hd
rev (Queue q) = Queue (reverse q)
size (Queue q) = length q

forPush :: Eq a => (Integral) a => a -> a -> Queue a -> Queue a
forPush i j q = if (i < j) then
                if (i `mod` 3 == 0 && (empty q) == False) then
                    forPush (i + 1) j (pop q)
                else 
                   forPush (i + 1) j (push q i)
            else (push q i)

forRev :: Eq a => (Integral) a => a -> a -> Queue a -> Queue a
forRev i j q = do 
    if (i < j) then 
        forRev (i + 1) j (rev q)
    else
        rev q

time :: IO t -> IO t
time a = do 
    start <- getCPUTime
    newq <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.9f sec\n" (diff :: Double)
    return newq

time_push = do 
    putStrLn "Starting to push and pop..."
    time $ (forPush 1 100000 newQ) `seq` return()
    putStrLn "Done"
    
time_rev = do
    let q = forPush 1 100000 newQ
    putStrLn "Starting to reverse..."
    time $ (forRev 1 10 q) `seq` return()
    putStrLn "Done"

