-- | ignore

import "../../diku-dk/cpprandom/random"
import "../../diku-dk/sorts/radix_sort"
import "../../diku-dk/statistics/statistics"

module stats = mk_statistics f64

-- | Market Risk Measure, MRM (Annex II, 2)
-- The VaR Equivalent Volatility (VEV) is assigned a market risk category (1-7)
let market_risk_measure (v:f64): i64 =
       if               v < 0.005 then 1
  else if 0.005 <= v && v < 0.05  then 2
  else if 0.05  <= v && v < 0.12  then 3
  else if 0.12  <= v && v < 0.2   then 4
  else if 0.2   <= v && v < 0.3   then 5
  else if 0.3   <= v && v < 0.8   then 6
  else                                 7

type moments  = (f64,f64,f64,f64,f64,f64,f64)
type scenario = [4]f64

-- | Log Returns (Annex II, 22a)
let returns [n] (v: [n]f64): [n-1]f64 = map2 (/) (tail v) (init v) |> map f64.log

-- | sigma for stress scenario (Annex IV, 10)
let sigma_strs [n] (t: f64) (v: [n]f64): f64 =
  let rolling (w: i64) (v: [n]f64): []f64 = iota (n-w) |> map (\i -> stats.stddev_pop v[i:i+w])
   in if t > 1 then stats.quantile (rolling 63 v) 0.90 -- (Annex IV, 10 a)
               else stats.quantile (rolling 21 v) 0.99 -- (Annex IV, 10 a)

-- | Moments (Annex II, 22c; Annex IV, 11-13)
let moments [t] (v: [t]f64): moments =

  -- zeroth and first moment
  let m0 = f64.i64 t
  let m1 = f64.sum v / m0

  -- second to forth moments
  let m2 = f64.sum (map (\x -> (x-m1)**2) v) / m0
  let m3 = f64.sum (map (\x -> (x-m1)**3) v) / m0
  let m4 = f64.sum (map (\x -> (x-m1)**4) v) / m0

  -- volatility
  let sigma = f64.sqrt m2

  -- skew and excess kurtosis
  let mu1 = m3/sigma**3
  let mu2 = m4/sigma**4 - 3

   in (m1,m2,m3,m4,sigma,mu1,mu2)

-- | Helpers
let not (b: bool): bool = !b
let fst (a,_) = a
let snd (_,b) = b
let cumsum [n] (x: [n]f64): [n]f64 = scan (+) 0.0 x
let traverse = flip map
let concat_1 [n] [m1] [m2] [m] (x: [n][m1]f64) (y: [n][m2]f64): [n][m]f64 =
  (transpose x ++ transpose y :> [m][n]f64) |> transpose

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
