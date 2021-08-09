# =========================================================================== #
# =========================== Weighting Functions =========================== #
# =========================================================================== #
# - Purpose of code:
#  - Calculate the welfare weights.


  
  
# =========================================================================== #
# ============================== Linear Weights ============================= #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param alpha How much weight to put on the minimum compared to the maximum,
  #'e.g. 2 would be twice as much weight on the bottom.
  #'@param in_test_1 Vector of all pre test scores.
  #'@param quantiles The percentiles at which we cap the weight.
  #'@param quant_val_l Optional pre test value below which we cap the weight.
  #'@param quant_val_h Optional pre test value above which we cap the weight.
  #'@details 
  #'@examples 

  # Start of the function.
  linear_weight_fun <- function(alpha, 
                                in_test_1,
                                quantiles   = c(.1,.9),
                                quant_val_l = NULL,
                                quant_val_h = NULL){
    
    # Check if we already passed the low and high caps, otherwise calculate.
    if(is.null(quant_val_l) | is.null(quant_val_h)) {
      quntile_lh <- quantile(in_test_1, probs = quantiles)
      quant_val_l <- quntile_lh["10%"]
      quant_val_h <- quntile_lh["90%"]
    }
  
    # Calculate the weight, capping outside of quantile values.
    weight <- alpha - (min(max(in_test_1, quant_val_l), quant_val_h) - quant_val_l)*
                (1/(quant_val_h - quant_val_l))*(alpha - 1)
                
    
    # Normalize the weights to sum to 1.
    weight <- weight/sum(weight)
    return(weight)
    
  } # End function.


  

# =========================================================================== #
# ============================= Rawlsian Weights ============================ #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param pctile The percentile below which we give weight_below and above which
  #'we give weight_above.
  #'@param weight_below The weight for those with pre test scores below that
  #'specified by pctile.
  #'@param weight_above The weight for those with pre test scores above that
  #'specified by pctile.
  #'@param in_test_1 Vector of pre test scores.
  #'@param pctile_val Optional the pre test value corresponding to the percentile
  #'specified in pctile.
  #'@details 
  #'@examples 
  
  # Start of the function.
  rawlsian_weight_fun <- function(pctile       = NULL,
                                  weight_below = 1.0001,
                                  weight_above = .0001,
                                  in_test_1    = NULL,
                                  pctile_val   = NULL){
    
    # Validate the inputs.
    if(is.null(pctile_val) & is.null(pctile)){
      stop("Need to specify a pctile or pctile_val")
    }
    
    if(!is.null(pctile_val) & !is.null(pctile)){
      stop("Need to specify a pctile or pctile_va, cannot have both")
    }
      
    
    # Calculate the weights with the given percentile cutoff and the data.
    if(!is.null(pctile)){
      weight <- ifelse(in_test_1 <= quantile(in_test_1, pctile), weight_below,
                       weight_above)
    }
    
    
    # Calculate the weights with a given pre test value.
    if(!is.null(pctile_val)){
      weight <- ifelse(in_test_1 <= pctile_val, weight_below, weight_above)
    }
    
    
    # Normalize the weights to sum to 1.
    weight <- weight/sum(weight)
    return(weight)
    
  } # End function.
  
  
  
  
# =========================================================================== #
# ============================== Equal Weights ============================== #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param in_test_1 Vector of all pre test scores.
  #'@details 
  #'@examples 
  
  # Start of the function.
  equal_weight_fun <- function(in_test_1) {
    
    # Make a vector of 1s.
    weight <- rep(1, length(in_test_1))
    
    # Normalize the weights to sum to 1.
    weight <- weight/sum(weight)
    return(weight)
    
  } # End function.


  
  
# =========================================================================== #
# ============================= V-shaped Weights ============================ #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param alpha Slope away from the mean.
  #'@param in_test_1 Vector of pre test scores.
  #'@param median_val Optional the pre test value corresponding to the median.
  #'@details 
  #'@examples 
  
  # Start of the function.
  v_weight_fun <- function(alpha, 
                           in_test_1, 
                           median_val = NULL){
    
    # Calculate the median if not specified.
    if(is.null(median_val)){
      median_val <- median(in_test_1)
    }
    
    
    # Calculate the weight, capping at 10th and 90th percentiles.
    weight <- alpha*abs(min(max(in_test_1, quantile(in_test_1, probs = .1)),
                            quantile(in_test_1, probs = .9)) - median_val)
    
    
    # Normalize the weights to sum to 1.
    weight <- weight/sum(weight)
    return(weight)
    
  } # End function.
  
  
  
  
# =========================================================================== #
# ============================ Mike Ricks Weights =========================== #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param pctile The percentile to give full weight to, between 0 and 1.
  #'@param dist Maximum distance away from pctile that observations get weight
  #'(in terms of percentiles, between 0 and 1). All outside of this distance get
  #'a weight of 0.0001.
  #'@param in_test_1 Vector of pre test scores.
  #'@param pctile_val Optional the pre test value corresponding to the percentile
  #'specified in pctile.
  #'@param min_score Optional the minimum pre test score getting weight.
  #'@param max_score Optional the maximum pre test score getting weight.
  #'@details 
  #'@examples 
  
  # Start of the function.
  mr_weight_fun <- function(pctile = NULL,
                            dist,
                            in_test_1,
                            pctile_val = NULL,
                            min_score  = NULL,
                            max_score = NULL){
    
    # Check if we passed the center, min, and max of the weight, otherwise calculate.
    if(is.null(pctile_val) | is.null(min_score) | is.null(max_score)){
      pctile_val <- quantile(in_test_1, pctile)
      min_score <- quantile(in_test_1, max(pctile - dist, 0))
      max_score <- quantile(in_test_1, min(pctile + dist, 100))
    }
    
    # Calculate the weight.
    weight <- (in_test_1 > min_score & in_test_1 <= pctile_val)*
      (in_test_1 - min_score)/(pctile_val - min_score) +
      (in_test_1 < max_score & in_test_1 > pctile_val)*
      (1 - (in_test_1-pctile_val)/(max_score-pctile_val)) + 0.0001
    
    # Renormalize the weight.
    weight <- weight/sum(weight)
    return(weight)
    
  } # End function.




# =========================================================================== #
# ========================= General Weight function ========================= #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param weight_type One of "linear", "rawlsian", "equal", "v", or "mr".
  #'@param in_test_1 Vector of all pre test scores.
  #'@param lin_alpha How much weight to put on the minimum compared to the maximum,
  #'e.g. 2 would be twice as much weight on the bottom. Linear weights.
  #'@param quant_val_l Optional pre test value below which we cap the weight.
  #'Linear weights.
  #'@param quant_val_h Optional pre test value above which we cap the weight.
  #'Linear weights.
  #'@param pctile The percentile below which we give weight_below and above which
  #'we give weight_above. Rawlsian weights.
  #'@param weight_below The weight for those with pre test scores below that
  #'specified by pctile. Rawlsian weights.
  #'@param weight_above The weight for those with pre test scores above that
  #'specified by pctile. Rawlsian weights.
  #'@param pctile_val Optional the pre test value corresponding to the percentile
  #'specified in pctile. Rawlsian weights.
  #'@param v_alpha Slope away from the mean. V-shaped weights.
  #'@param median_val Optional the pre test value corresponding to the median.
  #'V-shaped weights.
  #'@param mrpctile The percentile to give full weight to, between 0 and 1.
  #'Mike Ricks weights.
  #'@param mrdist Maximum distance away from pctile that observations get weight
  #'(in terms of percentiles, between 0 and 1). All outside of this distance get
  #'a weight of 0.0001. Mike Ricks weights.
  #'@param min_score Optional the minimum pre test score getting weight. Mike
  #'Ricks weights.
  #'@param max_score Optional the maximum pre test score getting weight. Mike
  #'Ricks weights.
  #'@details 
  #'@examples 
  
  # Start of the function.
  ww_general_fun <- function(weight_type  = NULL,
                             in_test_1    = NULL,
                             lin_alpha    = NULL,
                             quant_val_l  = NULL,
                             quant_val_h  = NULL,
                             pctile       = NULL,
                             pctile_val   = NULL,
                             weight_below = 1.0001,
                             weight_above = 0.0001,
                             v_alpha      = NULL,
                             median_val   = NULL,
                             mrpctile     = NULL, 
                             mrpctile_val   = NULL,
                             mrdist       = NULL,
                             min_score    = NULL,
                             max_score    = NULL) {
    
    # Check the weight type and use the appropriate function.
    if(weight_type == "linear"){
      
       linear_weight_fun(alpha       = lin_alpha, 
                         in_test_1   = in_test_1,
                         quantiles   = c(.1,.9),
                         quant_val_l = quant_val_l,
                         quant_val_h = quant_val_h)
      
    } else if(weight_type == "rawlsian") {
    
       rawlsian_weight_fun(pctile       = pctile,
                           weight_below = weight_below,
                           weight_above = weight_above,
                           in_test_1    = in_test_1,
                           pctile_val   = pctile_val)
      
    } else if(weight_type == "equal") {
      
      equal_weight_fun(in_test_1)
      
    } else if(weight_type == "v") {
      
      v_weight_fun(alpha      = v_alpha,
                   in_test_1  = in_test_1,
                   median_val = median_val)
      
    } else if(weight_type == "mr") {
      
      mr_weight_fun(pctile     = mrpctile,
                    dist       = mrdist,
                    in_test_1  = in_test_1,
                    pctile_val = mrpctile_val,
                    min_score  = min_score,
                    max_score  = max_score)
      
    }
    
  } # End function.
