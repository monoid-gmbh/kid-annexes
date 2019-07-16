import "../src/Category2"

--
-- Example from the document, page 8:
-- https://esas-joint-committee.europa.eu/Publications/Technical%20Standards/JC%202017%2049%20(PRIIPs_flow_diagram_risk_reward).pdf
-- Moments (Trading days per year 256, under paragraph 10 of Annex II)
-- M0:  1280          Number of observations in the period 256*5=1280
-- M1:  0,0003389     Mean of all the observed returns in the sample (daily)
-- M2:  0,000149905   Second Moment Volatility 0,01224357
-- M3: -6,44479E-07   Third Moment Skew -0,351143435
-- M4:  1,46705E-07   Fourth Moment Excess Kurtosis
-- # Market Risk
-- RHP (Recommended Holding Period expressed in years): 1
-- Number of Days:      256
-- VaR (Return Space): -0,4053
-- VEV Return Space:    0,1969

-- ==
-- entry: test_moments
-- compiled input @ SX5E.in
-- output { 1258 3027.870319 133007.388283 -15902559.773593 49504041886.439789 364.701780 -0.327834 -0.201733 }
entry test_moments (v: []f64): (i32, f64, f64, f64, f64, f64, f64, f64) = moments v
-- ==

-- ==
-- entry: test_category2
-- compiled input @ SX5E.in
-- output { -0.411746 0.199975 4 1 1.384678 1.073437 0.832576 0.335831 }
entry test_category2 (v: []f64): (f64, f64, i32, i32, f64, f64, f64, f64) =
  let (var, vev, mrm, sce) = category2 1.0 v
  let (fav, mod, ufav, strs) = sce[0]
   in (var, vev, mrm, length sce, fav, mod, ufav, strs)
-- ==