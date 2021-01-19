import "../../diku-dk/cpprandom/random"
import "../../diku-dk/sorts/radix_sort"

let not (b: bool): bool = !b
let fst (a,_) = a
let snd (_,b) = b
let cumsum [n] (x: [n]f64): [n]f64 = scan (+) 0.0 x
let traverse = flip map
let concat_1 [n] [m1] [m2] [m] (x: [n][m1]f64) (y: [n][m2]f64): [n][m]f64 =
  (transpose x ++ transpose y :> [m][n]f64) |> transpose
let tuple4 't (l:[4]t): (t,t,t,t) = (l[0],l[1],l[2],l[3])

-- | Sort
let sort_by f = radix_sort_float_by_key f f64.num_bits f64.get_bit
let sort = sort_by id
let sort_with_index [n] (v:[n]f64): [n](f64,i64) = iota n |> zip v |> sort_by fst

-- | Stats
let percentile_sorted 'a [l] (p: f64) (x: [l]a): a = let i = p / 100 * (f64.i64 l) in f64.ceil i |> f64.to_i64 |> \j -> x[j]
let percentile (p: f64) = sort >-> percentile_sorted p

-- | Random
type rng = minstd_rand.rng

let join_rng  = minstd_rand.join_rng
let split_rng = minstd_rand.split_rng
