import Data.List
import Control.Exception
import Debug.Trace

_b_sort :: [Int] -> [Int]
_b_sort[] = []
_b_sort[x] = [x]
_b_sort (x:y:xs) = if x > y then [y] ++ _b_sort([x]++xs) else [x] ++ _b_sort([y]++xs)
b_sort x = iterate _b_sort x !! length x

ll = [1..5]
ll_list = permutations ll
all_sorted = all (\x -> trace (show x) (x == ll)) (map b_sort ll_list)

msg = "bubblesort test done, total lines: "
main = if assert all_sorted True
    then do print (msg ++ show (length ll_list))
    else return ()
