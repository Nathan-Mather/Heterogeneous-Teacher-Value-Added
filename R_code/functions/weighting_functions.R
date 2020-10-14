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

# I thought if thus right as I was finishing but a smarter way to code the "val" thing would have been 
# to just include an option to have two data sets. ONe for getting parameters and one for issueing weights. 
linear_weight_fun <- function(alpha, 
                              in_test_1,
                              quantiles = c(.1,.9),
                              quant_val_l = NULL,
                              quant_val_h = NULL){
  if(is.null(quant_val_l) | is.null(quant_val_h)){
    print("Calculating quantiles from in_test_1")
    quntile_lh <- quantile(in_test_1, probs = quantiles)
    quant_val_l <- quntile_lh["10%"]
    quant_val_h <- quntile_lh["90%"]
  }
  
  weight <-  alpha-(in_test_1-quant_val_l)*(1/(quant_val_h-quant_val_l))*(alpha-1)
  weight <- weight/sum(weight) # I think it is more helpful if the weights sum to one in talking about them, though it does not affect the estimates at all.
}


# Mike Ricks weights - I changed these a bit so there is a "kernel" around the pctile we care about and it is 0.0001 elsewhere
# pctile is the percentile that we give full weight to, between 0 and 1
# dist is the maximum distance away from pctile that observations get weight (in terms of percentiles, between 0 and 1)
mr_weight_fun <- function(pctile = NULL,
                          dist,
                          in_test_1,
                          pctile_val = NULL,
                          min_score  = NULL,
                          max_score = NULL){
  
  if(is.null(pctile_val) | is.null(min_score) | is.null(max_score)){
    pctile_val <- quantile(in_test_1, pctile)
    min_score <- quantile(in_test_1, max(pctile - dist, 0))
    max_score <- quantile(in_test_1, min(pctile + dist, 100))
  }
  
  weight <- (in_test_1 > min_score & in_test_1 <= pctile_val)*(in_test_1 - min_score)/(pctile_val - min_score) + (in_test_1 < max_score & in_test_1 > pctile_val)*(1 - (in_test_1-pctile_val)/(max_score-pctile_val)) + 0.0001
}

#w_i <- median(r_dt$test_1)
#max_score <-  max(r_dt$test_1)
#min_score <-  min(r_dt$test_1)
#r_dt[test_1<=w_i, mr_weights := (test_1-min_score)/(w_i-min_score) ]
#r_dt[test_1>w_i, mr_weights := 1 - (test_1-w_i)/(max_score-w_i)]


# Rawlsian weights
# pctile is the cutoff percentile above which we give zero weight, between 0 and 1
# in_test_1 is the weighting variable
# pctile_val is for when you have the numeric value of the percentile already. 
# this is useful for applying this weighting scheme to other data sets or points
rawlsian_weight_fun <- function(pctile = NULL,
                                weight_below = 1.0001,
                                weight_above = .0001,
                                in_test_1 = NULL,
                                pctile_val = NULL){
  
  if(is.null(pctile_val) & is.null(pctile)){
    stop("Need to specify a pctile or pctile_val")
  }
  if(!is.null(pctile_val) & !is.null(pctile)){
    stop("Need to specify a pctile or pctile_va, cannot have both")
  }
    
  if(!is.null(pctile)){
    weight <- ifelse(in_test_1 <= quantile(in_test_1, pctile), weight_below, weight_above) # Is there a better way to do this and allow the weighting matrix to be always nonsingular?
  }
  
  if(!is.null(pctile_val)){
    weight <- ifelse(in_test_1 <= pctile_val, weight_below, weight_above)
  }
  
  return(weight)
}


# V-Shaped weights (Inverse Mike Ricks weights? ;) )
# alpha is the slope away from the mean
# in_test_1 is the weighting variable
v_weight_fun <- function(alpha, 
                         in_test_1, 
                         median_val = NULL){
  if(is.null(median_val)){
    median_val <- median(in_test_1)
  }
  weight <- alpha*abs(in_test_1 - median_val)
}




# write a function that takes a type option and calculates the proper weight 
# what it wont take though is the "Val" arguments. This function is for adding 
# weights to the same data set that we are deriving the percentiles from 
ww_general_fun <- function(weight_type  = NULL,
                           in_test_1    = NULL,
                           lin_alpha    = NULL,
                           pctile       = NULL,
                           weight_below = 1.0001,
                           weight_above = .0001,
                           v_alpha      = NULL,
                           mrpctile     = NULL, 
                           mrdist       = NULL,
                           pctile_val   = NULL){
  
  if(weight_type == "linear"){
    
     linear_weight_fun(alpha = lin_alpha, in_test_1 = in_test_1)
    
  } else if(weight_type == "rawlsian"){
  
     rawlsian_weight_fun(pctile = pctile,
                         weight_below = weight_below,
                         weight_above = weight_above,
                         in_test_1 = in_test_1,
                         pctile_val = pctile_val)
    
  } else if(weight_type == "equal"){
    
    equal_weight_fun(in_test_1)
    
  } else if(weight_type == "v"){
    
    v_weight_fun(v_alpha, in_test_1)
    
  } else if(weight_type == "mr"){
    
    mr_weight_fun(mrpctile, mrdist, in_test_1)
    
  }
}





