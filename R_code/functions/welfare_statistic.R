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
  #'@param qc_flag Should we run qc? 
  #'@details 
  #'@examples 
  
  
  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #
  
  # in_dt           = in_dt
  # output          = qtile_res      # NOTE ONLY USE ONE OF THESE
  # output          = output          # NOTE ONLY USE ONE OF THESE 
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
                                lin_alpha       = NULL,
                                pctile          = NULL,
                                weight_below    = NULL,
                                weight_above    = NULL,
                                v_alpha         = NULL,
                                mrpctile        = NULL, 
                                mrdist          = NULL,
                                impact_type     = NULL,
                                impact_function = NULL,
                                qc_flag         = 0,
                                out_qc_path     = NULL,
                                run_id_qc       = NULL){
    
    
    #======================#
    # ==== Get weights ====
    #======================#
    # if we want the true stat we need to use laten parms and nothing from inputed test data 
    if (type == 'true') {
      
      # print out a message about the assumptions here 
      print("The Assumption here is the true student poplation is nomral(0,1)")
      
      # Generate a grid over which we can get the true welfare added.
      grid <- unlist(lapply(rnorm(n = npoints), rep,
                            times =length(unique(in_dt$teacher_id))))
        
      # Attach teacher ids, start by getting unique teahers
      welfare <- unique(in_dt[, c('teacher_id', 'teacher_ability',
                                  'teacher_center', 'teacher_max')])
      
      # replicate each teacher npoints number of times so each can get a grid
      welfare <- do.call('rbind', replicate(npoints, welfare, simplify=FALSE))
      
      welfare[, stud_ability_1 := grid]
      
      
      # Get the weights for each place in the grid. #note in functions/weighting_funcitons
      welfare[, weight := ww_general_fun(weight_type  = weight_type,
                                         in_test_1    = grid,
                                         lin_alpha    = lin_alpha,
                                         quant_val_l  = qnorm(.1),
                                         quant_val_h  = qnorm(.9),
                                         pctile       = NULL, # entering known value instead 
                                         pctile_val   = qnorm(pctile),
                                         weight_below = weight_below,
                                         weight_above = weight_above,
                                         v_alpha      = v_alpha,
                                         median_va    = 0,
                                         mrpctile     = NULL, # entering known vlaue instead 
                                         mrpctile_val   = qnorm(mrpctile),
                                         mrdist       = mrdist,
                                         min_score    = qnorm(max(mrpctile - mrdist, 0)),
                                         max_score    = qnorm(min(mrpctile + mrdist, 100)))]
      
      
    # if not looking for the truth 
    }else{
        
        # check that sampe is bigger than n points 
        if(length(in_test_1) < npoints ){
          
          warning("full sample smaller than npoints, Setting npoints to full sample size")
          grid <- in_test_1
        }else{
          
          # Generate a random sample of test data 
          grid <- sample(in_test_1, size = npoints)
          
        }
      

        
        # Attach teacher ids.
        welfare <- unique(in_dt[, c('teacher_id', 'teacher_ability',
                                    'teacher_center', 'teacher_max')])
        
        
        n_teacher <- nrow(welfare)
        
        welfare <- do.call('rbind', replicate(length(grid), welfare, simplify=FALSE))

        # sort this by teacher so I can add on a replicated grid 
        setorder(welfare, teacher_id)
        
        # add student test grid 
        welfare[, test_1 := rep(grid, times = n_teacher) ]
        
        welfare[, weight := ww_general_fun(weight_type  = weight_type,
                                           in_test_1    = welfare$test_1,
                                           lin_alpha    = lin_alpha,
                                           quant_val_l  = quantile(welfare$test_1, probs = 0.1),
                                           quant_val_h  = quantile(welfare$test_1, probs = 0.9),
                                           pctile       = NULL,
                                           weight_below = weight_below,
                                           weight_above = weight_above,
                                           v_alpha      = v_alpha,
                                           median_va    = median(welfare$test_1),
                                           mrpctile     = mrpctile, 
                                           mrdist       = mrdist,
                                           min_score    = quantile(welfare$test_1, max(pctile - mrdist, 0)),
                                           max_score    = quantile(welfare$test_1, min(pctile + mrdist, 100)),
                                           pctile_val   = quantile(welfare$test_1, pctile))]
        
        
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
                                                stud_ability_1   = NULL, # don't need this because I know true mean and SD 
                                                studmean         = 0,
                                                studsd           = 1,
                                                other_data       = stud_ability_1,
                                                type             = impact_type,
                                                func_num         = impact_function)]
        
        # do some qc if its flagged  
        if(qc_flag == 1){
          
          # create folder to save these in 
          full_out_path <- paste0(out_qc_path, "/run_", run_id_qc, "/truth_qc/")
          dir.create(paste0(full_out_path, "/interactive_data/"),
                     recursive = TRUE)
          
          # make plot attributes 
          plot_attributes <- theme_classic() + 
            theme(text = element_text(size= 20),
                  plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =25),
                  plot.subtitle = element_text(color = "black",hjust = 0.5, size =25),
                  legend.title = element_text(size=20),
                  legend.position="right")
          
          
          
          # pick 5 random teachers and graph their imapct 
          rand_teachers <- sample(in_dt[, unique(teacher_id)], 5)
          
          
          # loop over random selection of teachers 
          for(t_j in rand_teachers){
            
            # subset welfare to this teacher 
            welfare_sub <- welfare[teacher_id == t_j]
            
            fig_j <- ggplot(welfare_sub, aes(x = stud_ability_1, y = true_impact)) + 
              geom_point(size = 1) + 
              ggtitle(paste0("Teacher ", t_j," True Impact Distribution")) + 
              plot_attributes
            
            # Save the plots.
            ggsave(filename =paste0(full_out_path, "Teacher ", t_j," True Impact run ", run_id_qc, ".png"), 
                   plot     = fig_j, 
                   width    = 9, 
                   height   = 4)
            
            # save them as rdata 
            
            save( fig_j,
                 file = paste0(full_out_path,"/interactive_data/", "Teacher ", t_j," True Impact run ", run_id_qc, ".Rdata"))
            
            
          }
          
          # make average teacher impact figure 
          ave_impact <- welfare[, list(mean_impact = mean(true_impact)), stud_ability_1]
          
          ave_impact_plot <- ggplot(ave_impact, aes(x = stud_ability_1, y = mean_impact)) + 
            geom_point(size = 1) + 
            ggtitle(paste0("Average Teacher True Impact")) + 
            plot_attributes
          
          # Save the plots.
          ggsave(filename =paste0(full_out_path, "Average Teacher True Impact run ", run_id_qc, ".png"), 
                 plot     = ave_impact_plot, 
                 width    = 9, 
                 height   = 4)
          
          # save rdata version for shiny app 
          save( ave_impact_plot,
                file = paste0(full_out_path,"/interactive_data/", "Average Teacher True Impact run ", run_id_qc, ".Rdata"))
          
          
          weight_dt <- unique(welfare[, c("stud_ability_1", "weight")])
          
          # make a weight figure 
          weight_figure <- ggplot(weight_dt, aes(x = stud_ability_1, y = weight)) + 
            geom_point(size = 1) + 
            ggtitle(paste0("Weight Distribution")) + 
            plot_attributes
          
          ggsave(filename =paste0(full_out_path, "Weigh_Distribution_run ", run_id_qc, ".png"), 
                 plot     = weight_figure, 
                 width    = 9, 
                 height   = 4)
          
          # save rdata version for shiny app 
          save( weight_figure,
                file = paste0(full_out_path,"/interactive_data/", "Weigh_Distribution_run ", run_id_qc, ".Rdata"))
          
        
          return( welfare[, list(true_welfare = sum(true_impact*weight)), by='teacher_id'])
          
          # if no qc, just return data 
        }else{
        
        # Calculate and return the true welfare.
        return( welfare[, list(true_welfare = sum(true_impact*weight)), by='teacher_id'])
          
        }
        
      } else if (type == 'bin') {
        
        # Get the numeric range for each category.
        output <- copy(as.data.table(output))
        output[, range_low := as.numeric(sub('\\(', '', sapply(strsplit(category, ','), '[', 1)))]
        output[, range_high := as.numeric(sub('\\]', '', sapply(strsplit(category, ','), '[', 2)))]

        
        # Make the overall minimum very low and the overall maximum very high to capture all.
        output[category != '', temp1 := min(range_low), by='teacher_id']
        output[category != '', temp2 := max(range_high), by='teacher_id']
        output[range_low == temp1, range_low := -100]
        output[range_high == temp2, range_high := 100]
        
        # make cateogry xwalk 
        cat_xwalk <- unique(output[, c("range_low", "range_high")])
        cat_xwalk[, bin := .I]
        
        # loop through bin xwalk and fill out welfare data bins 
        #note more code, but faster than the way we had it before 
        for(i in 1:nrow(cat_xwalk)){
          
          low_i <- cat_xwalk[i, range_low]
          high_i <- cat_xwalk[i, range_high]
          bin_num_i <- cat_xwalk[i, bin]
          
          welfare[test_1 > low_i  &
                    test_1 <=  high_i, bin := bin_num_i ]
          
          
        }
        
        # now merge on estimates 
        output <- merge(output,cat_xwalk, c("range_low", "range_high"))
        output <- output[, c("teacher_id", "bin", "estimate")]
        welfare <- merge(welfare, output, c("teacher_id", "bin"))
        
        # Calculate and return the estimated welfare.
        # welfare[, estimate := as.numeric(estimate)]
        alt_welfare <- welfare[, .(alternative_welfare = sum(estimate*weight)), by='teacher_id']

        if(qc_flag == 0){
          
          return(alt_welfare[])
        }else{
          
          return(list(welfare_est = alt_welfare,
                      full_est    = output,
                      bin_xwalk   = cat_xwalk))
        }

        
      } else if (type == 'quant') {
        
        in_test_1 <- in_test_1
        in_coefs <- output
        
        # current dependencies 
        # the variable name "tau" , "qtile_est", "se", "teacher_id"
        
        # first we need to fill in the quantiles with actual values 
        tau_xwalk <- data.table(tau = unique(in_coefs$tau))
        tau_xwalk[, tau_val := quantile(in_test_1, 
                                        probs = tau)]
        
        
        # Get the weights for each place in the grid.
        tau_xwalk[, weight := ww_general_fun(weight_type  = weight_type,
                                           in_test_1    = tau_val,
                                           lin_alpha    = lin_alpha,
                                           quant_val_l  = quantile(in_test_1, probs = 0.1),
                                           quant_val_h  = quantile(in_test_1, probs = 0.9),
                                           pctile       = NULL,
                                           weight_below = weight_below,
                                           weight_above = weight_above,
                                           v_alpha      = v_alpha,
                                           median_va    = median(in_test_1),
                                           mrpctile     = mrpctile, 
                                           mrdist       = mrdist,
                                           min_score    = quantile(in_test_1, max(pctile - mrdist, 0)),
                                           max_score    = quantile(in_test_1, min(pctile + mrdist, 100)),
                                           pctile_val   = quantile(in_test_1, pctile))]
        
        
        # adjust weights for student population using estiamted parameters 
        stud_mean <- mean(in_test_1)
        stud_sd   <- sd(in_test_1)
        tau_xwalk[, weight := weight*dnorm(tau_val,
                                         mean = stud_mean,
                                         sd = stud_sd)]
        
        # Renormalize the weights. so each teacher's weight sums to 1
        tau_xwalk[, tot_weight := sum(weight),]
        tau_xwalk[, weight := weight/tot_weight]
        tau_xwalk[, tot_weight := NULL]
        
        
        # now we merge those on 
        qtile_constants <- in_coefs[teacher_id == 'test_1']
        in_coefs <- in_coefs[teacher_id != 'test_1', ]
        
        w_coefs_dt <- merge(in_coefs, tau_xwalk, "tau")
        
        # now standardize the estimates by quantile 
        
        w_coefs_dt[, qtile_est := (qtile_est - mean(qtile_est))/(sd(qtile_est)), tau]
        w_coefs_dt[, qtile_est := mapply((function(x, y)  y + qtile_constants[tau == x, qtile_est]), tau, qtile_est)]
        
        
        # aggregate estimates 
        tot_weight <- tau_xwalk[, sum(weight)]
        
        ww_qtile_va <- w_coefs_dt[, list(alternative_welfare = sum(qtile_est*weight/tot_weight)),
                                  teacher_id]
        
        
        # return the aggregate estimates 
        return(ww_qtile_va)
        
      }else if (type == 'np_hack') {

        # Take output as a j by npoints matrix of fitted values
        if (length(output$points)==npoints) {
          
          # add reshaped fitted values to data (should operate column by column to match weights)
          welfare[, fit := as.matrix(output$results[, 1:npoints,],ncol(1))]
          
          # Approximate integration over weights
          ww_np_hack_va <- welfare[, list(alternative_welfare = sum(weight*(fit))), teacher_id]

          # Grab unique values for each teacher
          # Standardize to mean zero var one
          #ww_np_hack_va[, alternative_welfare := (WA_temp-mean(WA_temp))/sd(WA_temp)]
          
          
          # return the  estimates 
          return(ww_np_hack_va)    
      } else {
        stop("dimensions of fitted values and weights inconsistant")
      }
      
      
    } else if (type == 'semi') {
        
      }
    
  } # End function.
