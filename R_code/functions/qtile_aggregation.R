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
                      v_alpha      = NULL,
                      mrpctile     = NULL,
                      mrdist       = NULL){

  # current dependencies 
  # the variable name "tau" , "value", "se", "teacher_id"
  
  # first we need to fill in the quantiles with actual values 
  tau_xwalk <- data.table(tau = unique(coefs_dt$tau))
  tau_xwalk[, tau_val := quantile(in_test_1, 
                                  probs = tau)]
  

  # Next we need to get the relevent points from the data to apply our weights 
  if(weight_type == "linear"){
    
    # need to get weight quantile values 
    quntile_lh <- quantile(in_test_1, probs = c(.1, .9))
    quant_val_l <- quntile_lh["10%"]
    quant_val_h <- quntile_lh["90%"]

  } else if(weight_type == "rawlsian"){
    
    # get weight percentile val 
    w_pctile_val <- quantile(in_test_1, pctile)
    
  } else if(weight_type == "v"){
    
    v_weight_fun(v_alpha, in_test_1)
    
  } else if(weight_type == "mr"){
    
    mr_weight_fun(mrpctile, mrdist, in_test_1)
    
  }

  
  
  
  
  
  
  
  # now we can get weights for each quanitile 
  tau_xwalk[, weight := rawlsian_weight_fun(in_test_1 = tau_val,
                                  pctile_val = w_pctile_val)]
  
  
  # now we merge those on 
  w_coefs_dt <- merge(in_coefs, tau_xwalk, "tau")
  
  # aggregate estimates 
  tot_weight <- tau_xwalk[, sum(weight)]
  
  ww_qtile_va <- w_coefs_dt[, list(ww_qtile_va = sum(Value*weight/tot_weight)),
                            teacher_id]
  
  
  # return the aggregate estiamtes 
  return(ww_qtile_va)

}
