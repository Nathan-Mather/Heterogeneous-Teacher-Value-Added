
#=========================================#
#==== Calculate the Welfare Statistic ====#
#=========================================#

welfare_statistic <- function(in_dt           = NULL,
                              output          = NULL,
                              type            = NULL, # One of 'true', 'standard', bin', 'quant', 'semi'.
                              npoints         = 1000,
                              weight_type     = NULL, # Weight function parameters.
                              in_test_1       = NULL,
                              pctile          = NULL,
                              weight_below    = NULL,
                              weight_above    = NULL,
                              v_alpha         = NULL,
                              mrpctile        = NULL, 
                              mrdist          = NULL,
                              impact_type     = NULL, # Teacher impact parameters.
                              impact_function = NULL){
  
  
  # Generate a grid over which we can get the true welfare added.
  grid <- unlist(lapply(seq(-3, 3, length.out = npoints), rep, times =  length(unique(in_dt$teacher_id))))
  
  
  # Attach teacher ids.
  welfare <- unique(in_dt[, c('teacher_id', 'teacher_ability', 'teacher_center', 'teacher_max')])
  welfare <- do.call('rbind', replicate(npoints, welfare, simplify=FALSE))
  welfare <- welfare[, grid := grid]
  
  
  # Get the weights for each place in the grid. ################ Need to update weights other than Rawlsian. Also it cannot be optimal to recalculate this every time.
  welfare[, weight := ww_general_fun(weight_type  = weight_type,
                                     in_test_1    = grid,
                                     lin_alpha    = lin_alpha,
                                     pctile       = NULL,
                                     weight_below = weight_below,
                                     weight_above = weight_above,
                                     v_alpha      = v_alpha,
                                     mrpctile     = mrpctile, 
                                     mrdist       = mrdist,
                                     pctile_val   = quantile(in_dt$test_1, pctile))]
  
  
  # Renormalize the weights.
  tot_weight <- welfare[, sum(weight)]
  
  welfare[, weight := weight/tot_weight]
  
  
  # Calculate the appropriate welfare statistic.
  if (type == 'true') {
    # Get the teacher impact for the grid.
    welfare[, true_impact := teacher_impact(teacher_ability  = teacher_ability,
                                            teacher_center   = teacher_center,
                                            teacher_max      = teacher_max,
                                            stud_ability_1   = in_dt$stud_ability_1,
                                            other_data       = grid,
                                            type             = impact_type,
                                            func_num         = impact_function)]
    
    # Calculate and return the true welfare.
    welfare[, true_welfare := sum(true_impact*weight), by='teacher_id']
    
    return(unique(welfare[, c('teacher_id', 'true_welfare')]))
    
    
  } else if (type == 'standard') {
    
    # Merge on the estimate.
    welfare <- merge(welfare, output, 'teacher_id') #################### Check this. Does this keep all rows in 'welfare' and merge on the estimate?
    
    # Simply return the weighted sum.
    welfare[, standard_welfare := sum(estimate*weight), by='teacher_id']
    return(unique(welfare[, c('teacher_id', 'standard_welfare')]))
    
    
  } else if (type == 'bin') {
    
    # Get the numeric range for each category.
    output <- as.data.table(output)
    output[, range_low := as.numeric(sub('\\(', '', sapply(strsplit(category, ','), '[', 1)))]
    output[, range_high := as.numeric(sub('\\]', '', sapply(strsplit(category, ','), '[', 2)))]

    # Get the baseline estimate for each teacher.
    output[, baseline := .SD[1, estimate], by='teacher_id']
    
    
    # Make the overall minimum very low and the overall maximum very high to capture all.
    output[category != '', temp1 := min(range_low), by='teacher_id']
    output[category != '', temp2 := max(range_high), by='teacher_id']
    output[range_low == temp1, range_low := -100]
    output[range_high == temp2, range_high := 100]
    
    # Calculate the estimated teacher impact.
    welfare[, estimate := mapply((function(x, y) output[output$teacher_id == x & 
                                                          output$range_low < y &
                                                          output$range_high >= y, estimate] + 
                                                 output[output$teacher_id == x & 
                                                          output$range_low < y &
                                                          output$range_high >= y, baseline]), teacher_id, grid)]
    
    # Calculate and return the estimated welfare.
    welfare[, alternative_welfare := sum(estimate*weight), by='teacher_id']
    
    return(unique(welfare[, c('teacher_id', 'alternative_welfare')]))
    
    
  } else if (type == 'quant') {
    
    in_test_1 <- in_dt$test_1
    in_coefs <- output
    
    # current dependencies 
    # the variable name "tau" , "qtile_est", "se", "teacher_id"
    
    # first we need to fill in the quantiles with actual values 
    tau_xwalk <- data.table(tau = unique(in_coefs$tau))
    tau_xwalk[, tau_val := quantile(in_test_1, 
                                    probs = tau)]
    
    
    # Next we need to get the relevant points from the data to apply our weights 
    # then actually apply the weight 
    if(weight_type == "linear"){
      
      # need to get weight quantile values 
      quntile_lh <- quantile(in_test_1, probs = c(.1, .9))
      quant_val_l <- quntile_lh["10%"]
      quant_val_h <- quntile_lh["90%"]
      
      # now we can get weights for each quantile 
      tau_xwalk[, weight := linear_weight_fun(in_test_1 = tau_val,
                                              alpha     = lin_alpha,
                                              quant_val_l = quant_val_l,
                                              quant_val_h = quant_val_h)]
      
    } else if(weight_type == "rawlsian"){
      
      # get weight percentile val 
      w_pctile_val <- quantile(in_test_1, pctile)
      
      # now we can get weights for each quantile 
      tau_xwalk[, weight := rawlsian_weight_fun(in_test_1 = tau_val,
                                                pctile_val = w_pctile_val,
                                                weight_below = weight_below,
                                                weight_above = weight_above)]
      
    } else if(weight_type == "v"){
      
      median_val <- median(in_test_1)
      
      # now we can get weights for each quantile 
      tau_xwalk[, weight := v_weight_fun(in_test_1 = tau_val,
                                         alpha     = v_alpha,
                                         median_val= median_val)]
      
    } else if(weight_type == "mr"){
      
      # get parameters from data 
      pctile_val <- quantile(in_test_1, pctile)
      min_score <- quantile(in_test_1, max(pctile - mrdist, 0))
      max_score <- quantile(in_test_1, min(pctile + mrdist, 100))
      
      # now we can get weights for each quantile 
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
    
    ww_qtile_va <- w_coefs_dt[, list(alternative_welfare = sum(qtile_est*weight/tot_weight)),
                              teacher_id]
    
    
    # return the aggregate estimates 
    return(ww_qtile_va)
    
  } else if (type == 'semi') {
    
  }
  
}