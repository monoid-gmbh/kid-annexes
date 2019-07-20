import "lib/github.com/diku-dk/sorts/radix_sort"

let not (b: bool): bool = !b
let fst (a,_) = a
let snd (_,b) = b
let cumsum: []f64 -> *[]f64 = scan (+) 0.0
let traverse = flip map
let concat_1 x y = transpose x ++ transpose y |> transpose
let tuple4 't (l:[4]t): (t,t,t,t) = (l[0],l[1],l[2],l[3])

-- | Sort
let sort_by f = radix_sort_float_by_key f f64.num_bits f64.get_bit
let sort = sort_by id
let sort_with_index [n] (v:[n]f64): [n](f64,i32) = iota n |> zip v |> sort_by fst

-- | Rolling window
let rolling [t] (w: i32) (f: [w]f64 -> f64) (v: [t]f64): []f64 =
  let x = iota t in zip x (map (+w) x) |> take (t-w) |> map (\(i,j) -> unsafe (f v[i:j]))
