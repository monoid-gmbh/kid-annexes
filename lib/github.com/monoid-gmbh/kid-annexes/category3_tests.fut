import "category3"

-- ==
-- entry: test_trivial
-- input { 1.0 }
-- output { 10.0 }
entry test_trivial (x: f64): f64 =
  let rng = minstd_rand.rng_from_seed [123]
  let dummy_payoff [n] [t] (m: [n][t]f64): f64 = transpose m |> last |> reduce (+) 0.0
  let v: [1000]f64 = replicate 1000 x
  let w: [10][1000]f64 = [v,v,v,v,v,v,v,v,v,v]

  let (_,var,_,_,_) = category3 rng 2560 dummy_payoff w
   in var
-- ==
