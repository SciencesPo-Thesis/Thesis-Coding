===================================================
  SETUP — things that never change
===================================================
  
  Load your data:
  Y        = norm support scores for all people (FIXED FOREVER)
  X_known  = running variable for non-1994-born people (FIXED FOREVER)
  who_1994 = list of people born in 1994 (they need birth dates drawn)
  
  Set:
    S = 1000   (outer iterations — birth date draws)
  B = 500    (inner iterations — bootstrap draws per outer iteration)
  cutoff = 1994.849
  
  Create empty lists:
    point_estimates = []       (will hold S values)
  null_estimates  = []       (will hold S*B values)
  
  
  ===================================================
    OUTER LOOP — handles missing birth date problem
  ===================================================
    
    For s = 1, 2, 3, ..., S:
    
    --- Step 1: Build a complete dataset for this iteration ---
    
    Start with X = X_known for all non-1994 people (these never change)
  
  For each person in who_1994:
    Draw a random birth month uniformly from {1, 2, ..., 12}
  Draw a random birth day uniformly from {1, 2, ..., 28}
  Convert to decimal year:
    X_person = 1994 + (day_of_year / 365)
  Assign to X for this person
  
  You now have a complete X for all 10 people.
  Y is unchanged — it's always the real survey data.

    --- Step 2: Run rdrobust on real Y, store point estimate ---

    result = rdrobust(Y, X, cutoff)
    τ_s    = result$coef   (the estimated jump)

    Append τ_s to point_estimates


    ==============================================
    INNER LOOP — handles inference (null distribution)
    ==============================================

    --- Step 3: Fit the null model on this iteration's dataset ---
    (Do this ONCE per outer iteration, before the inner loop)
  
  Get bandwidth h from rdrobust result above
  
  Keep only people within bandwidth:
    in_bandwidth = people where |X - cutoff| <= h
  
  Fit a smooth polynomial through everyone in bandwidth:
    null_model = lm(Y ~ polynomial(X),
                    data    = in_bandwidth,
                    weights = triangular_kernel(X, h))
  (NO treatment indicator, NO cutoff, just smooth trend)
  
  Compute predicted values:
    Y_hat = predicted values from null_model for each person
  
  Compute residuals:
    residuals = Y - Y_hat   (one residual per person)
  
  --- Step 4: Bootstrap loop ---
    
    For b = 1, 2, 3, ..., B:
    
    -- Resample residuals --
    resampled_residuals = draw N values WITH REPLACEMENT from residuals
  (where N = number of people in bandwidth)
  
  -- Build fake Y --
    Y_fake = Y_hat + resampled_residuals
  (smooth trend + reshuffled noise = no jump by construction)
  
  -- X does NOT change. Nobody moves. --
    
    -- Run rdrobust on fake Y --
    fake_result = rdrobust(Y_fake, X[in_bandwidth], cutoff,
                           bandwidth = h)
  (FIXED bandwidth — same h as real model)
  
  τ_null = fake_result$coef
  
  Append τ_null to null_estimates
  
  --- End of inner loop ---
    
    --- End of outer loop ---
    
    
    ===================================================
    COLLAPSE RESULTS
  ===================================================
    
    Point estimate:
    τ_bar = mean(point_estimates)
  SE_MC = sd(point_estimates)
  (this is your existing Monte Carlo result — unchanged)
  
  Null distribution:
    (null_estimates now contains S*B values)
  Plot histogram of null_estimates — should be centered near zero
  
  P-value:
    p = proportion of null_estimates where |τ_null| >= |τ_bar|
    
    Confidence interval from null:
    lower = 2.5th percentile of null_estimates
  upper = 97.5th percentile of null_estimates
  
  
  ===================================================
    INTERPRETATION
  ===================================================
    
    If p < 0.05:
    Your real τ_bar falls in the tail of the null distribution
  → The jump is unlikely to be noise
  → Evidence for a real effect
  
  If p >= 0.05:
    Your real τ_bar is unremarkable compared to the null distribution
  → Cannot rule out that the jump is noise