import "lib/github.com/diku-dk/statistics/statistics"

module stats = mk_statistics f64

-- helper functions
let not (b: bool): bool = !b
let fst (a,_) = a
let snd (_,b) = b
let cumsum: []f64 -> *[]f64 = scan (+) 0.0
let traverse = flip map
let concat_1 x y = transpose x ++ transpose y |> transpose

-- | Market Risk Measure, MRM (Annex II, 2)
-- The VaR Equivalent Volatility (VEV) is assigned a market risk category (1-7)
let market_risk_measure (v:f64): i32 =
       if               v < 0.005 then 1
  else if 0.005 <= v && v < 0.05  then 2
  else if 0.05  <= v && v < 0.12  then 3
  else if 0.12  <= v && v < 0.2   then 4
  else if 0.2   <= v && v < 0.3   then 5
  else if 0.3   <= v && v < 0.8   then 6
  else                                 7

type moments  = (i32,f64,f64,f64,f64,f64,f64,f64)
type scenario = (f64,f64,f64,f64)
type payoff   = [][]f64 -> f64

-- | Log Returns (Annex II, 22a)
let returns [n] (v: [n]f64): []f64 = map2 (/) (tail v) (init v) |> map f64.log

-- | Rolling window
let rolling [t] (w: i32) (f: [w]f64 -> f64) (v: [t]f64): []f64 =
  let x = iota t in zip x (map (+w) x) |> take (t-w) |> map (\(i,j) -> f v[i:j])

-- | sigma for stress scenario (Annex IV, 10)
let sigma_strs (t: f64) (v: []f64): f64 =
  if t > 1 then rolling 63 stats.stddev_pop v |> (flip stats.quantile) 0.90 -- (Annex IV, 10 a)
           else rolling 21 stats.stddev_pop v |> (flip stats.quantile) 0.99 -- (Annex IV, 10 a)

-- | Moments (Annex II, 22c; Annex IV, 11-13)
let moments [t] (v: [t]f64): moments =

  -- zeroth and first moment
  let m0 = f64.i32 t
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

   in (t,m1,m2,m3,m4,sigma,mu1,mu2)
