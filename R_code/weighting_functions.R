# ========================= #
# == Weighting Functions == #
# ========================= #


# Linear weighting function
linear_weight_fun <- function(alpha, in_test_1){
  max_score <- max(in_test_1)
  min_score <- min(in_test_1)
  weight <-  alpha-(in_test_1-min_score)*(1/(max_score-min_score))*(alpha-1)
  weight <- weight/sum(weight) # I think it is more helpful if the weights sum to one in talking about them, though it does not affect the estimates at all.
}


# Mike Ricks weights 
#w_i <- median(r_dt$test_1)
#max_score <-  max(r_dt$test_1)
#min_score <-  min(r_dt$test_1)
#r_dt[test_1<=w_i, mr_weights := (test_1-min_score)/(w_i-min_score) ]
#r_dt[test_1>w_i, mr_weights := 1 - (test_1-w_i)/(max_score-w_i)]