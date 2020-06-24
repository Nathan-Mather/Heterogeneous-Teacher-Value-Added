# ========================= #
# == Weighting Functions == #
# ========================= #


# Equal weights (our current estimators are identical under equal weights)
# in_test_1 is the weighting variable
equal_weight_fun <- function(in_test_1){
  weight <- 1
}


# Linear weighting function
# alpha is how much we weight the minimum compared to the maximum 
# in_test_1 is the weighting variable
linear_weight_fun <- function(alpha, in_test_1){
  max_score <- max(in_test_1)
  min_score <- min(in_test_1)
  weight <-  alpha-(in_test_1-min_score)*(1/(max_score-min_score))*(alpha-1)
}


# Mike Ricks weights - I changed these a bit so there is a "kernel" around the pctile we care about and it is 0.0001 elsewhere
# pctile is the percentile that we give full weight to, between 0 and 1
# dist is the maximum distance away from pctile that observations get weight (in terms of percentiles, between 0 and 1)
mr_weight_fun <- function(pctile, dist, in_test_1){
  w_i <- quantile(in_test_1, pctile)
  min_score <- quantile(in_test_1, max(pctile - dist, 0))
  max_score <- quantile(in_test_1, min(pctile + dist, 100))
  
  weight <- (in_test_1 > min_score & in_test_1 <= w_i)*(in_test_1 - min_score)/(w_i - min_score) + (in_test_1 < max_score & in_test_1 > w_i)*(1 - (in_test_1-w_i)/(max_score-w_i)) + 0.0001
}

#w_i <- median(r_dt$test_1)
#max_score <-  max(r_dt$test_1)
#min_score <-  min(r_dt$test_1)
#r_dt[test_1<=w_i, mr_weights := (test_1-min_score)/(w_i-min_score) ]
#r_dt[test_1>w_i, mr_weights := 1 - (test_1-w_i)/(max_score-w_i)]


# Rawlsian weights
# pctile is the cutoff percentile above which we give zero weight, between 0 and 1
# in_test_1 is the weighting variable
rawlsian_weight_fun <- function(pctile, in_test_1){
  weight <- as.integer(in_test_1 <= quantile(in_test_1, pctile)) + 0.0001 # Is there a better way to do this and allow the weighting matrix to be always nonsingular?
}


# V-Shaped weights (Inverse Mike Ricks weights? ;) )
# alpha is the slope away from the mean
# in_test_1 is the weighting variable
v_weight_fun <- function(alpha, in_test_1){
  median_score <- median(in_test_1)
  weight <- alpha*abs(in_test_1 - median_score)
}

