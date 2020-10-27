# =========================================================================== #
# ===================== Calculate the Welfare Statistic ===================== #
# =========================================================================== #
# - Purpose of code:
#  - Calculate the welfare statistic for each of our methods.


  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param in_data The data table used for the estimation.
  #'@param output The results from the estimation.
  #'@param type The type of estimation used, one of "true", "standard", bin",
  #'"quant", or "semi".
  #'@param npoints Number of grid points to use.
  #'@param weight_type The weights to use, one of "equal", "rawlsian", "linear",
  #'"v", or "mr".
  #'@param in_test_1 Vector containing all pre test data.
  #'@param pctile The percentile to be used for the specified weight.
  #'@param weight_below The weight below the specified percentile for rawlsian.
  #'@param weight_above The weight above the specified percentile for rawlsian.
  #'@param v_alpha The alpha parameter for v weights.
  #'@param mrpctile The percentile value for mr weights.
  #'@param mrdist The spread for mr weights.
  #'@param impact_type This specifies the type of impact function that teachers
  #'will have for their students. Must by one of "MLRN" (Monotone, Linear,
  #'Rank Similar, No Heterogeneity), "MLR" (Monotone, Linear, Rank Similar),
  #'"ML" (Monotone, Linear, Not Rank Similar), "MNoR" (Monotone, Non-linear,
  #'Rank Similar), "MNo" (Monotone, Non-linear, Not Rank Similar), or "No" (Not
  #'Monotone, Non-linear, Not Rank Similar).
  #'@param impact_function Which function from the specified type we want for
  #'the true teacher impact.
  #'@details 
  #'@examples 
  
  
  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #
  
  # in_dt           = in_dt
  # output          = output
  # type            = 'bin'
  # npoints         = npoints
  # weight_type     = weight_type
  # in_test_1       = in_dt$test_1
  # pctile          = pctile
  # weight_below    = weight_above
  # weight_above    = weight_below
  # v_alpha         = v_alpha
  # mrpctile        = mrpctile
  # mrdist          = mrdist
  
  # # # parms from MC run
  # in_dt           = r_dt
  # type            = 'true'
  # npoints         = p_npoints
  # weight_type     = p_weight_type
  # in_test_1       = r_dt$test_1
  # pctile          = p_pctile
  # weight_below    = p_weight_below
  # weight_above    = p_weight_above
  # v_alpha         = p_v_alpha
  # mrpctile        = p_mrpctile
  # mrdist          = p_mrdist
  # impact_type     = p_impact_type
  # impact_function = p_impact_function

  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  welfare_statistic <- function(in_dt           = NULL,
                                output          = NULL,
                                type            = NULL,
                                npoints         = 1000,
                                weight_type     = NULL,
                                in_test_1       = NULL,
                                pctile          = NULL,
                                weight_below    = NULL,
                                weight_above    = NULL,
                                v_alpha         = NULL,
                                mrpctile        = NULL, 
                                mrdist          = NULL,
                                impact_type     = NULL,
                                impact_function = NULL){
    
    
    # =============================================================== #
    # ======================= Get weighted grid ==================== #
    # ============================================================= #
      
      # Generate a grid over which we can get the true welfare added.
      grid <- unlist(lapply(seq(-3, 3, length.out = npoints), rep,
                            times =length(unique(in_dt$teacher_id))))
      
      
      # Attach teacher ids.
      welfare <- unique(in_dt[, c('teacher_id', 'teacher_ability',
                                  'teacher_center', 'teacher_max')])
      welfare <- do.call('rbind', replicate(npoints, welfare, simplify=FALSE))
      welfare[, grid := grid]
      
      
      # Get the weights for each place in the grid.
      welfare[, weight := ww_general_fun(weight_type  = weight_type,
                                         in_test_1    = grid,
                                         lin_alpha    = lin_alpha,
                                         quant_val_l  = quantile(in_dt$test_1, probs = 0.1),
                                         quant_val_h  = quantile(in_dt$test_1, probs = 0.9),
                                         pctile       = NULL,
                                         weight_below = weight_below,
                                         weight_above = weight_above,
                                         v_alpha      = v_alpha,
                                         median_va    = median(in_dt$test_1),
                                         mrpctile     = mrpctile, 
                                         mrdist       = mrdist,
                                         min_score    = quantile(in_dt$test_1, max(pctile - mrdist, 0)),
                                         max_score    = quantile(in_dt$test_1, min(pctile + mrdist, 100)),
                                         pctile_val   = quantile(in_dt$test_1, pctile))]
      
      
      
      # Correct grid weights based on student population
      if (type == 'true') {
        
        # adjust weights for student population using true parameters 
        welfare[, weight := weight*dnorm(grid)]
        
      # for all other methods 
      }else{
        
        # adjust weights for student population using estiamted parameters 
        stud_mean <- mean(in_test_1)
        stud_sd   <- sd(in_test_1)
        welfare[, weight := weight*dnorm(grid,
                                         mean = stud_mean,
                                         sd = stud_sd)]
      }
      
      # Renormalize the weights. so each teacher's weight sums to 1
      welfare[, tot_weight := sum(weight), teacher_id]
      welfare[, weight := weight/tot_weight]
      welfare[, tot_weight := NULL]
      
      
    # =============================================================== #
    # ============ Calculate the welfare statistic ================= #
    # ============================================================= #
      
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
        return( welfare[, list(true_welfare = sum(true_impact*weight)), by='teacher_id'])
        
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
        
      }else if (type == 'np_hack') {
      
      # Take output as a j by npoints matrix of fitted values
      if (length(output$points)==npoints) {
        
        # add reshaped fitted values to data (should opperate column by column to match weights)
        welfare[  , fit := as.matrix(output$results[ , 1:npoints,],ncol(1))  ]
        
        # Approximate integration over weights
        welfare[  , WA_temp  := sum(weight*fit) , teacher_id]
        
        # Grab unique values for each teacher
        ww_np_hack_va <- unique(welfare[, c('teacher_id', 'WA_temp')])
        
        # Standardize to mean zero var one
        ww_np_hack_va[, WA := (WA_temp-mean(WA_temp))/sd(WA_temp)]
        
        
        # return the  estimates 
        return(ww_np_hack_va)    
      } else {
        stop("dimensions of fitted values and weights inconsistant")
      }
      
      
    } else if (type == 'semi') {
        
      }
    
  } # End function.
