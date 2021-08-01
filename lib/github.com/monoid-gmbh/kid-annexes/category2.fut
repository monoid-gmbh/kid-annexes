-- Category 2 (Annex II, IV)
--
--    Linear products, calculation of
--
--    * VaR (Cornish Fisher)
--    * VEV (VaR Equivalent Volatility)
--    * Performance scenarios
--    * Performance scenarios for intermediate holding periods
--

import "base"

-- | VaR measure in return space, Cornish-Fisher expansion (Annex II, 12)
let cornish_fisher (n: f64) ((_,_,_,_,sigma,mu1,mu2): moments): f64 =
  sigma*(f64.sqrt n)*(-1.96 + 0.474*mu1/(f64.sqrt n) - 0.0687*mu2/n + 0.146*(mu1**2)/n) - 0.5*(sigma**2)*n

-- | VaR Equivalent Volatility (Annex II, 13)
let var_equivalent_volatility (p: f64) (t: f64) =
  (f64.sqrt(3.842-2*p)-1.96)/(f64.sqrt t)

-- | Unfavourable scenario (Annex IV, 9a)
let unfavourable (n: f64) ((m1,_,_,_,sigma,mu1,mu2): moments): f64 =
  f64.exp(m1*n + sigma*f64.sqrt(n)*(-1.28 + 0.107*mu1/f64.sqrt(n) + 0.0724*mu2/n - 0.0611*(mu1**2)/n) - 0.5*(sigma**2)*n)

-- | Moderate scenario (Annex IV, 9b)
let moderate (n: f64) ((m1,_,_,_,sigma,mu1,_): moments): f64 =
  f64.exp(m1*n + sigma*mu1/6 - 0.5*(sigma**2)*n)

-- | Favourable scenario (Annex IV, 9c)
let favourable (n: f64) ((m1,_,_,_,sigma,mu1,mu2): moments): f64 =
  f64.exp(m1*n + sigma*f64.sqrt(n)*(1.28 + 0.107*mu1/f64.sqrt(n) - 0.0724*mu2/n + 0.0611*(mu1**2)/n) - 0.5*(sigma**2)*n)

-- | Stress scenario (Annex IV, 11)
let stress (n: f64) ((_,_,_,_,_,mu1,mu2): moments) (sigma: f64) (z_alpha: f64): f64 = f64.exp(
  sigma*f64.sqrt(n)*(
     z_alpha + ((  z_alpha**2 - 1        )/6 )*mu1   /f64.sqrt(n)
             + ((  z_alpha**3 - 3*z_alpha)/24)*mu2   /n
             - ((2*z_alpha**3 - 5*z_alpha)/36)*mu1**2/n
  ) - 0.5*sigma**2*n)

-- | Day count convention
let days: f64 = 256.0

-- | Category 2 (Annex II, IV)
let category2 [n] (t: f64) (v: [n]f64): (f64,f64,i64,[]scenario) =
  let k = n-1
  let r:[k]f64 = returns v
  let m = moments r

  -- Market risk measurements (Annex II)
  let var = cornish_fisher (days*t) m
  let vev = var_equivalent_volatility var t
  let mrm = market_risk_measure vev

  -- Scenarios (Annex IV)
  let s = sigma_strs t r -- stressed volatility
  let z_alpha = if t > 1 then -1.644853627 -- alpha 5% (Annex IV, 11)
                         else -2.326347874 -- alpha 1% (Annex IV, 11)

  let scenarios (n: f64) (m: moments): scenario =
    [favourable n m, moderate n m, unfavourable n m, stress n m s z_alpha]

  in if (t <= 1) then (var, vev, mrm, [scenarios (days*t) m])
    else if (t <= 3)
        then (var, vev, mrm, [scenarios 1.0 m, scenarios (days*t)   m])
        else (var, vev, mrm, [scenarios 1.0 m, scenarios (days*t/2) m, scenarios (days*t) m])
