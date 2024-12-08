Future improvements :
- make screen pixels, CPU memory and registers vectors instead of lists to have O(1) indexing (with packages vector and fixed-vector for instance)
- add error handling around ROM opening/reading (file not found, permission error etc..), probably with Expected monad