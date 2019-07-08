-- Category 3 (Annex II, IV)
--
--    Non-linear products, calculation of
--
--    * VaR (Monte-Carlo simulations)
--    * VEV (VaR Equivalent Volatility)
--    * Performance scenarios
--    * Performance scenarios for intermediate holding periods
--

import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/statistics/statistics"

import "Base"

module stats = mk_statistics f64
module dist  = uniform_int_distribution i32 minstd_rand

type rng = minstd_rand.rng

let join_rng  = minstd_rand.join_rng
let split_rng = minstd_rand.split_rng

-- | Number of simulations (Annex II, 19)
let nr_sim: i32 = 100000

-- | Bootstrap indices
let bootstrap_index_vector (s: i32) (t: i32) (g: rng): (rng,[t]i32) =
  let f = dist.rand (0,s-1)
  let (gs,ys) = split_rng t g |> map f |> unzip
   in (join_rng gs,ys)

-- | Resample
let resample [n] [s] (g: rng) (t: i32) (r: [n][s]f64): (rng,[nr_sim][n][t]f64) =
  let f = bootstrap_index_vector s t
  let (gs,ix) = split_rng nr_sim g |> map f |> unzip
   in (join_rng gs, tabulate_3d nr_sim n t (\x y z -> let i = ix[x,z] in r[y,i]))

-- | Construct a path starting at s0
let path [t] (r: [t]f64) (s0: f64) (f: f64 -> f64 -> f64): [t]f64 =
  let s = cumsum r in tabulate t (\i -> f (r64 i) s[i]) |> map f64.exp |> map (*s0)

-- Returns are corrected with the measured expectation (Annex II, 22c)
let path_mrm [t] (m1: f64) (sigma: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = x - k*m1 - k*0.5*sigma**2 in path r s0 f

-- Construct the scenario simulation path (Annex IV, 12)
let path_scen [t] (sigma: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = x - k*0.5*sigma**2 in path r s0 f

-- Construct the stress scenario simulation path (Annex IV, 13)
let path_strs [t] (sigma: f64) (sigma_S: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = sigma_S/sigma*x - k*0.5*sigma_S**2 in path r s0 f

-- | VaR Equivalent Volatility (Annex II, 17)
let var_equivalent_volatility (p: f64) (t: f64) =
  (f64.sqrt(3.842-2*f64.log(p))-1.96)/(f64.sqrt t)

let value_at_risk v = stats.quantile v 0.025

-- | Day count convention
let days: f64 = 256.0

let quantiles o_strs o_scen =
  ( stats.quantile o_strs 0.1  -- stress
  , stats.quantile o_scen 0.1  -- unfavorable
  , stats.quantile o_scen 0.5  -- moderate
  , stats.quantile o_scen 0.9) -- favorable

-- | Category 3 simulations for MRM (Annex II)
let category3 [n] [l] (g: rng) (p: payoff) (t: i32) (v: [n][l]f64): (rng,f64,f64,i32,[]scenario) =

  let s0: [n]f64   = transpose v |> head
  let r : [n][]f64 = map returns v

  -- Bootstrap returns
  let (g',s): (rng,[nr_sim][n][]f64) = resample g t r

  -- RHP in years
  let y = r64 t/days

  -- Measured moments
  let r_measured: []f64 = let f i = p v[:,:i] in iota l |> map f |> returns
  let m1_measured       = stats.mean   r_measured
  let sigma_measured    = stats.stddev r_measured
  let sigma_S_measured  = sigma_strs y r_measured

  -- Market risk measurements (Annex II)
  let o_mrm: [nr_sim]f64 = let f = path_mrm m1_measured sigma_measured in map2 f s0 >-> p |> traverse s

  let var = value_at_risk o_mrm
  let vev = var_equivalent_volatility var y
  let mrm = market_risk_measure vev

  -- Scenaios for full RHP (Annex IV, 11-13)
  let o_scen: [nr_sim]f64 = let f = path_scen sigma_measured                  in map2 f s0 >-> p |> traverse s
  let o_strs: [nr_sim]f64 = let f = path_strs sigma_measured sigma_S_measured in map2 f s0 >-> p |> traverse s

  -- Intermediate holding periods (Annex IV, 24)
  let idx: i32 = 0 -- FIXME
  let s_seed: [nr_sim][n][]f64 = replicate nr_sim s[idx,:,:255]

  let (g'',s'): (rng,[nr_sim][n][]f64) = resample g' t r
  let s'': [nr_sim][n][]f64 = map2 concat_1 s_seed s'

  let o_scen': [nr_sim]f64 = let f = path_scen sigma_measured                  in map2 f s0 >-> p |> traverse s''
  let o_strs': [nr_sim]f64 = let f = path_strs sigma_measured sigma_S_measured in map2 f s0 >-> p |> traverse s''

   in (g''
     , var, vev, mrm
     , [ quantiles o_strs o_scen, quantiles o_strs' o_scen' ]
     )
