import "../src/Category3"

let sample_payoff [n] [t] (m: [n][t]f64): f64 = m[0,t-1]

-- ==
-- entry: test_category3
-- compiled input @ SX5E.in
-- output { -0.411746 0.199975 4 1 1.384678 1.073437 0.832576 0.335831 }
entry test_category3 (v: []f64) =

  let rng = minstd_rand.rng_from_seed [123]

  let w: [1][]f64 = [v]
  let (g0, var, vev, mrm, sce) = category3 rng sample_payoff 100 w
  let (fav, mod, ufav, strs) = sce[0]
   in (var, vev, mrm, length sce, fav, mod, ufav, strs)
