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
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/statistics/statistics"

import "Base"

module stats = mk_statistics f64
module dist  = uniform_int_distribution i32 minstd_rand

type rng = minstd_rand.rng

let join_rng  = minstd_rand.join_rng
let split_rng = minstd_rand.split_rng

-- | Number of simulations (Annex II, 19)
let nr_sim: i32      = 10000
let nr_intermed: i32 = 3333
let percentile_10 x  = x[1000]
let percentile_50 x  = x[5000]
let percentile_90 x  = x[9000]
let percentile_2_5 x = x[250]

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

-- | Day count convention
let days: f64 = 256.0

-- TODO: ok?
let measured_moments [n] [l] (p: payoff) (v: [n][l]f64) (y: f64) =
  let r_obs: []f64 = let f i = p v[:,:i] in iota l |> map f |> returns
   in (stats.mean r_obs, stats.stddev r_obs, sigma_strs y r_obs)

-- | Category 3 simulations for MRM (Annex II)
let category3 [n] [l] (g: rng) (p: payoff) (t: i32) (v: [n][l]f64): (rng,f64,f64,i32,[]scenario) =

  let s0: [n]f64   = transpose v |> head
  let r : [n][]f64 = map returns v

  -- Bootstrap returns
  let (ga,s): (rng,[nr_sim][n][t]f64) = resample g t r

  -- RHP in years
  let y = r64 t/days

  -- Measured moments
  let (m1,sigma,sigma_S) = measured_moments p v y

  let simulate f = map2 f s0 >-> p |> traverse s

  -- Market risk measurements (Annex II)
  let sT_mrm: [nr_sim]f64 = path_mrm m1 sigma |> simulate |> sort

  let var = percentile_2_5 sT_mrm
  let vev = var_equivalent_volatility var y
  let mrm = market_risk_measure vev

  -- Scenaios full RHP (Annex IV, 11-13)
  let sT_scen: [nr_sim](f64,i32) = path_scen sigma |> simulate |> sort_with_index
  let sT_strs: [nr_sim](f64,i32) = path_strs sigma sigma_S |> simulate |> sort_with_index

  let (str,idx_str) = percentile_10 sT_strs -- TODO: depends on t
  let (ufa,idx_ufa) = percentile_10 sT_scen
  let (med,idx_med) = percentile_50 sT_scen
  let (fav,idx_fav) = percentile_90 sT_scen
  let scen_full_rhp = (str,ufa,med,fav)

  -- Intermediate holding periods (Annex IV, 24)
  let intermediate_holding_period g' d =
    let simulate_ihp gx idx f h =

      -- build seed paths (up to d)
      let seed: [nr_intermed][n][]f64 = replicate nr_intermed s[idx,:,:d]

      -- bootstrap (from d to rhp)
      let t' = t-d
      let (gy,sim): (rng,[nr_intermed][n][t']f64) = resample gx t' r
      let s': [nr_intermed][n][t]f64              = map2 concat_1 seed sim
       in map2 f s0 >-> p |> traverse s' |> sort |> h |> \x -> (gy,x)

    let f_scen = path_scen sigma
    let f_strs = path_strs sigma sigma_S

    let gs = split_rng 4 g'
    let (g0,str) = simulate_ihp gs[0] idx_str f_strs percentile_10 -- TODO: depends on t
    let (g1,ufa) = simulate_ihp gs[1] idx_ufa f_scen percentile_10
    let (g2,med) = simulate_ihp gs[2] idx_med f_scen percentile_50
    let (g3,fav) = simulate_ihp gs[3] idx_fav f_scen percentile_90
     in (join_rng [g0,g1,g2,g3], (str,ufa,med,fav))

  let (gb, scen_one_year) = intermediate_holding_period ga 255
  let (gc, scen_rhp_half) = intermediate_holding_period gb 265

   in (gc
     , var, vev, mrm
     , [scen_full_rhp, scen_rhp_half, scen_one_year]
     )
