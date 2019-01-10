import Data.List
import Control.Exception

q_sort :: [Int] -> [Int]
q_sort[] = []
q_sort (x:xs) = q_sort[a | a<-xs, a<=x] ++ [x] ++ q_sort[a | a<-xs, a>x]

ll = [1..5]
ll_list = permutations ll
all_sorted = all (\x -> x == ll) (map q_sort ll_list)

msg = "quicksort test done, total lines: "
main = if assert all_sorted True
    then do print (msg ++ show (length ll_list))
    else return ()
