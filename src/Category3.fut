-- Category 3 (Annex II, IV)
--
--    Non-linear products, calculation of
--
--    * VaR (Monte-Carlo simulations)
--    * VEV (VaR Equivalent Volatility)
--    * Performance scenarios
--    * Performance scenarios for intermediate holding periods
--

import "Base"

module stats = mk_statistics f64
module dist  = uniform_int_distribution i32 minstd_rand

-- | Bootstrap indices
let bootstrap_index_vector (s: i32) (t: i32) (g: rng): (rng,[t]i32) =
  let (gs,ys) = let f = dist.rand (0,s-1) in split_rng t g |> map f |> unzip
   in (join_rng gs,ys)

-- | Resample
let resample [n] [s] (t: i32) (l: i32) (r: [n][s]f64) (g: rng): (rng,[l][n][t]f64) =
  let (gs,ix) = let f = bootstrap_index_vector s t in split_rng l g |> map f |> unzip
   in (join_rng gs, tabulate_3d l n t (\x y z -> let i = ix[x,z] in unsafe r[y,i]))

-- | Construct a path starting at s0
let path [t] (r: [t]f64) (s0: f64) (f: f64 -> f64 -> f64): [t]f64 =
  let s = cumsum r in tabulate t (\i -> f s[i] (r64 i)) |> map f64.exp |> map (*s0)

-- | Returns are corrected with the measured expectation (Annex II, 22c)
let path_mrm [t] (m1: f64) (sigma: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = x - k*m1 - k*0.5*sigma**2 in path r s0 f

-- | Construct the scenario simulation path (Annex IV, 12)
let path_scen [t] (sigma: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = x - k*0.5*sigma**2 in path r s0 f

-- | Construct the stress scenario simulation path (Annex IV, 13)
let path_strs [t] (sigma: f64) (sigma_S: f64) (s0: f64) (r: [t]f64): [t]f64 =
  let f x k = sigma_S/sigma*x - k*0.5*sigma_S**2 in path r s0 f

-- | VaR Equivalent Volatility (Annex II, 17)
let var_equivalent_volatility (p: f64) (t: f64) =
  (f64.sqrt(3.842-2*f64.log(p))-1.96)/(f64.sqrt t)

--let measured_moments [n] [l] (p: payoff) (v: [n][l]f64) (y: f64) =
--  let r_obs: []f64 = let f i = p v[:,:i] in iota l |> map f |> returns
--   in (stats.mean r_obs, stats.stddev r_obs, sigma_strs y r_obs)

-- | Category 3 simulations for MRM (Annex II)
let category3 [n] [l] (g: rng) (t: i32) (p: [n][t]f64 -> f64) (v: [n][l]f64): (rng,f64,f64,i32,[]scenario) =

  let s0: [n]f64   = transpose v |> head
  let r : [n][l]f64 = map returns v

  -- Number of simulations (Annex II, 19)
  let nr_sim: i32 = 10000

  -- Bootstrap returns
  let (g0,s): (rng,[nr_sim][n][t]f64) = resample t nr_sim r g

  -- Day count convention
  let days: f64 = 256.0

  -- RHP in years
  let y = r64 t/days

  -- Measured moments
  -- TODO: let (m1,sigma,sigma_S) = measured_moments p v y
  let (m1,sigma,sigma_S) = let r_obs = r[0] in (stats.mean r_obs, stats.stddev r_obs, sigma_strs y r_obs)

  -- Simulation
  let simulate s f = map2 f s0 >-> p |> traverse s

  -- Market risk measurements (Annex II)
  let var = path_mrm m1 sigma |> simulate s |> percentile 2.5
  let vev = var_equivalent_volatility var y
  let mrm = market_risk_measure vev

  -- Scenarios, full RHP (Annex IV, 11-13)
  let sT_scen: [nr_sim](f64,i32) = path_scen sigma |> simulate s |> sort_with_index
  let sT_strs: [nr_sim](f64,i32) = path_strs sigma sigma_S |> simulate s |> sort_with_index

  let (scenarios_rhp, scenarios_rhp_idxs) = unzip
    [ percentile_sorted 10 sT_strs -- TODO: depends on t
    , percentile_sorted 10 sT_scen
    , percentile_sorted 50 sT_scen
    , percentile_sorted 90 sT_scen ]

  -- Scenarios, intermediate holding periods (Annex IV, 24)
  let intermediate_holding_period h i: (rng,scenario) =
    let nr_resim: i32 = 3333
    let h0 = split_rng 4 h

    -- Get seed and resample for each scenario. TODO: choose "good" paths as index for seed
    let seed: [4][nr_resim][n][i]f64 = replicate nr_resim <-< (\x -> unsafe s[x,:,:i]) |> traverse scenarios_rhp_idxs
    let rem = t-i
    let (h1,sim): ([]rng, [4][nr_resim][n][rem]f64) = resample rem nr_resim r |> traverse h0 |> unzip
    let xs: [4][nr_resim][n][t]f64 = let f = map2 concat_1 in map2 f seed sim

    let scenarios_ihp = tuple4
      [ path_strs sigma sigma_S |> simulate xs[0] |> percentile 10 -- TODO: depends on t
      , path_scen sigma |> simulate xs[1] |> percentile 10
      , path_scen sigma |> simulate xs[2] |> percentile 50
      , path_scen sigma |> simulate xs[3] |> percentile 90 ]
     in (join_rng h1, scenarios_ihp)

   in if (y > 1)
    then
      let (g2, scenarios_ihps) = if (y > 3)
        then let g1 = split_rng 2 g0 in unzip
          [ intermediate_holding_period g1[0] (f64.to_i32 days)              -- 1 year
          , intermediate_holding_period g1[1] (f64.ceil y/2 |> f64.to_i32) ] -- half rhp
        else let g1 = split_rng 2 g0 in unzip
          [ intermediate_holding_period g1[0] (f64.to_i32 days) ]            -- 1 year
      in (join_rng g2,var,vev,mrm,[tuple4 scenarios_rhp] ++ scenarios_ihps)

    else (g0,var,vev,mrm,[tuple4 scenarios_rhp])
