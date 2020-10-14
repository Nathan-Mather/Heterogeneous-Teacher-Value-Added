#===============================#
# ==== VA Aggregation qtile ====
#===============================#

# # load sampel data just to test this out 
# test_data <- fread("file:///C:/Users/Nmath_000/Documents/data/Value Added/int_data/qtile_output.csv")
# coefs_dt <- fread("C:/Users/Nmath_000/Documents/data/Value Added/int_data/qtile_output_long.csv")


qtile_agg <- function(in_test_1  = NULL,
                      in_coefs = NULL,
                      weight_type =NULL,
                      lin_alpha    = NULL,
                      pctile       = NULL,
                      weight_below = NULL,
                      weight_above = NULL,
                      v_alpha      = NULL,
                      mrpctile     = NULL,
                      mrdist       = NULL){

  # current dependencies 
  # the variable name "tau" , "qtile_est", "se", "teacher_id"
  
  # first we need to fill in the quantiles with actual values 
  tau_xwalk <- data.table(tau = unique(in_coefs$tau))
  tau_xwalk[, tau_val := quantile(in_test_1, 
                                  probs = tau)]
  

  # Next we need to get the relevent points from the data to apply our weights 
  # then actually apply the weight 
  if(weight_type == "linear"){
    
    # need to get weight quantile values 
    quntile_lh <- quantile(in_test_1, probs = c(.1, .9))
    quant_val_l <- quntile_lh["10%"]
    quant_val_h <- quntile_lh["90%"]
    
    # now we can get weights for each quanitile 
    tau_xwalk[, weight := linear_weight_fun(in_test_1 = tau_val,
                                            alpha     = lin_alpha,
                                            quant_val_l = quant_val_l,
                                            quant_val_h = quant_val_h)]

  } else if(weight_type == "rawlsian"){
    
    # get weight percentile val 
    w_pctile_val <- quantile(in_test_1, pctile)
    
    # now we can get weights for each quanitile 
    tau_xwalk[, weight := rawlsian_weight_fun(in_test_1 = tau_val,
                                              pctile_val = w_pctile_val,
                                              weight_below = weight_below,
                                              weight_above = weight_above)]
    
  } else if(weight_type == "v"){
    
    median_val <- median(in_test_1)
    
    # now we can get weights for each quanitile 
    tau_xwalk[, weight := v_weight_fun(in_test_1 = tau_val,
                                       alpha     = v_alpha,
                                       median_val= median_val)]

  } else if(weight_type == "mr"){
    
    # get parameters from data 
    pctile_val <- quantile(in_test_1, pctile)
    min_score <- quantile(in_test_1, max(pctile - mrdist, 0))
    max_score <- quantile(in_test_1, min(pctile + mrdist, 100))
    
    # now we can get weights for each quanitile 
    tau_xwalk[, weight := mr_weight_fun(in_test_1  = tau_val,
                                        pctile     = mrpctile,
                                        dist       = mrdist, 
                                        pctile_val = pctile_val,
                                        min_score  = min_score,
                                        max_score  = max_score)]

  }

  

  # now we merge those on 
  w_coefs_dt <- merge(in_coefs, tau_xwalk, "tau")
  
  # aggregate estimates 
  tot_weight <- tau_xwalk[, sum(weight)]
  
  ww_qtile_va <- w_coefs_dt[, list(ww_va = sum(qtile_est*weight/tot_weight)),
                            teacher_id]
  
  
  # return the aggregate estiamtes 
  return(ww_qtile_va)

}
