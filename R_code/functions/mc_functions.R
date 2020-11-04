# =========================================================================== #
# ========================== Monte Carlo Functions ========================== #
# =========================================================================== #
# - Purpose of code:
#  - Put together the functions used in the Monte Carlo code.




# =========================================================================== #
# ======================== Standard VA Stat Function ======================== #
# =========================================================================== #

  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param 
  #'@details 
  #'@examples

# #debug parms
# use the single iteration function parms below 

  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  standard_va_stat <- function(in_dt              = NULL,
                               single_run         = NULL,
                               weight_type        = NULL,
                               method             = NULL, 
                               lin_alpha          = NULL,
                               pctile             = NULL,
                               weight_below       = NULL,
                               weight_above       = NULL,
                               v_alpha            = NULL,
                               mrpctile           = NULL, 
                               mrdist             = NULL,
                               npoints            = NULL,
                               n_teacher          = NULL,
                               n_stud_per_teacher = NULL,
                               test_SEM           = NULL,
                               teacher_va_epsilon = NULL,
                               impact_type        = NULL,
                               impact_function    = NULL,
                               max_diff           = NULL,
                               covariates         = NULL,
                               peer_effects       = NULL,
                               stud_sorting       = NULL,
                               rho                = NULL,
                               ta_sd              = NULL,
                               sa_sd              = NULL) {
    
    # Run the standard VA.
    if (covariates == 0) {
      va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = in_dt)
    } else {
      va_out1 <- lm(test_2 ~ test_1 + teacher_id + school_av_test + stud_sex +
                      stud_frpl + stud_att - 1, data = in_dt) ###### Check this
    }
    
    # Clean results.
    va_tab1 <- data.table(broom::tidy(va_out1))
    va_tab1[, teacher_id := gsub("teacher_id", "", term)]
    
    # Return just the estimates and standard errors.
    if (!is.null(single_run)) {
      setnames(va_tab1, old=c('estimate', 'std.error'), new=c('mean_standard',
                                                              'sd_standard'))
      
      return(va_tab1[term %like% "teacher_id", c("teacher_id", "mean_standard",
                                                 "sd_standard")])
    } else {
      setnames(va_tab1, old=c('estimate'), new=c('standard_welfare'))
      
      return(va_tab1[term %like% "teacher_id", c("teacher_id",
                                                 "standard_welfare")])
    }
  
  } # End function.
  
  
  

# =========================================================================== #
# ========================= Binned VA Stat Function ========================= #
# =========================================================================== #

  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param 
  #'@details 
  #'@examples
  
  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  binned_va_stat <- function(in_dt              = NULL,
                             index              = NULL,
                             boot               = NULL,
                             weight_type        = NULL,
                             method             = NULL, 
                             lin_alpha          = NULL,
                             pctile             = NULL,
                             weight_below       = NULL,
                             weight_above       = NULL,
                             v_alpha            = NULL,
                             mrpctile           = NULL, 
                             mrdist             = NULL,
                             npoints            = NULL,
                             n_teacher          = NULL,
                             n_stud_per_teacher = NULL,
                             test_SEM           = NULL,
                             teacher_va_epsilon = NULL,
                             impact_type        = NULL,
                             impact_function    = NULL,
                             max_diff           = NULL,
                             covariates         = NULL,
                             peer_effects       = NULL,
                             stud_sorting       = NULL,
                             rho                = NULL,
                             ta_sd              = NULL,
                             sa_sd              = NULL) {
    
    # Allow the bootstrap to pick the sample if needed.
    if (!is.null(boot)) {
      in_dt <- in_dt[index] 
    }
    
    # Estimate the binned VA.
    if (covariates == 0) {
      output <- binned_va(in_data = in_dt)
    } else {
      output <- binned_va(in_data = in_dt,
                          reg_formula = paste0('test_2 ~ test_1',
                                               ' + teacher_id:categories',
                                               ' + school_av_test + stud_sex + ',
                                               'stud_frpl + stud_att - 1'))
    }
    
    # Fill in missing estimates with 0 (so that we end up with teacher_ability).
    output <- complete(output, teacher_id, category)
    for (i in seq_along(output)) set(output, i=which(is.na(output[[i]])), j=i,
                                     value=0)

    # Calculate the welfare statistic for each teacher.
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = output,
                                type            = 'bin', 
                                npoints         = npoints,
                                weight_type     = weight_type,
                                in_test_1       = in_dt$test_1,
                                lin_alpha       = lin_alpha,
                                pctile          = pctile,
                                weight_below    = weight_below,
                                weight_above    = weight_above,
                                v_alpha         = v_alpha,
                                mrpctile        = mrpctile, 
                                mrdist          = mrdist)
    
    # Return the full data if in the MC or just the estimates for the bootstrap.
    if (is.null(boot)) {
      return(output)
    } else {
      return(output$alternative_welfare)
    }
    
  } # End function.



  
# =========================================================================== #
# ======================== Quantile VA Stat Function ======================== #
# =========================================================================== #
  
  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param 
  #'@details 
  #'@examples
  
  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  quantile_va_stat <- function(in_dt              = NULL,
                               index              = NULL,
                               boot               = NULL,
                               weight_type        = NULL,
                               method             = NULL, 
                               lin_alpha          = NULL,
                               pctile             = NULL,
                               weight_below       = NULL,
                               weight_above       = NULL,
                               v_alpha            = NULL,
                               mrpctile           = NULL, 
                               mrdist             = NULL,
                               npoints            = NULL,
                               n_teacher          = NULL,
                               n_stud_per_teacher = NULL,
                               test_SEM           = NULL,
                               teacher_va_epsilon = NULL,
                               impact_type        = NULL,
                               impact_function    = NULL,
                               max_diff           = NULL,
                               covariates         = NULL,
                               peer_effects       = NULL,
                               stud_sorting       = NULL,
                               rho                = NULL,
                               ta_sd              = NULL,
                               sa_sd              = NULL) {
      
    # Allow the bootstrap to pick the sample if needed.
    if (!is.null(boot)) {
      in_dt <- in_dt[index] 
    }
    
    # Run quantile regression and get estimates for a grid of tau values.
    qtile_res <- qtilep_va(in_data       = in_dt,
                           in_teacher_id = "teacher_id",
                           in_pre_test   = "test_1",
                           in_post_test  = "test_2",
                           ptle          = seq(.02, .98, by=.04))
    
    
    # Calculate the welfare statistic for each teacher.
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = qtile_res,
                                type            = 'quant', 
                                npoints         = npoints,
                                weight_type     = weight_type,
                                in_test_1       = in_dt$test_1,
                                lin_alpha       = lin_alpha,
                                pctile          = pctile,
                                weight_below    = weight_below,
                                weight_above    = weight_above,
                                v_alpha         = v_alpha,
                                mrpctile        = mrpctile, 
                                mrdist          = mrdist)
    
    # Return the full data if in the MC or just the estimates for the bootstrap.
    if (is.null(boot)) {
      return(output)
    } else {
      return(output$alternative_welfare)
    }
    
  } # End function.



# =========================================================================== #
# ===================== Semiparametric VA Stat Function ===================== #
# =========================================================================== #





# =========================================================================== #
# ===================== Rough Nonparam VA Stat Function ===================== #
# =========================================================================== #


  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param 
  #'@details 
  #'@examples
  
  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  np_hack_va_stat <- function(in_dt              = NULL,
                              index              = NULL,
                              boot               = NULL,
                              weight_type        = NULL,
                              method             = NULL, 
                              lin_alpha          = NULL,
                              pctile             = NULL,
                              weight_below       = NULL,
                              weight_above       = NULL,
                              v_alpha            = NULL,
                              mrpctile           = NULL, 
                              mrdist             = NULL,
                              npoints            = NULL,
                              n_teacher          = NULL,
                              n_stud_per_teacher = NULL,
                              test_SEM           = NULL,
                              teacher_va_epsilon = NULL,
                              impact_type        = NULL,
                              impact_function    = NULL,
                              max_diff           = NULL,
                              covariates         = NULL,
                              peer_effects       = NULL,
                              stud_sorting       = NULL,
                              rho                = NULL,
                              ta_sd              = NULL,
                              sa_sd              = NULL,
                              weighted_average   = NULL) {
    
    # Allow the bootstrap to pick the sample if needed.
    if (!is.null(boot)) {
      in_dt <- in_dt[index] 
    }
    
    # Run quantile regression and get estimates for a grid of tau values.
    np_res <- np_hack_va(in_data          = in_dt,
                         in_teacher_id    = "teacher_id",
                         in_pre_test      = "test_1",
                         in_post_test     = "test_2",
                         npoints          =  npoints,
                         weighted_average = weighted_average)
    
    
    # Calculate the welfare statistic for each teacher.
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = np_res,
                                type            = 'np_hack', 
                                npoints         = npoints,
                                weight_type     = weight_type,
                                in_test_1       = in_dt$test_1,
                                lin_alpha       = lin_alpha,
                                pctile          = pctile,
                                weight_below    = weight_below,
                                weight_above    = weight_above,
                                v_alpha         = v_alpha,
                                mrpctile        = mrpctile, 
                                mrdist          = mrdist)
    
    print(output)

    
    # Return the full data if in the MC or just the estimates for the bootstrap.
    if (is.null(boot)) {
      return(output)
    } else {
      return(output$alternative_welfare)
    }
    
  } # End function.




# =========================================================================== #
# ======================== Single Iteration Function ======================== #
# =========================================================================== #

  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param 
  #'@details 
  #'@examples 


  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #

  # Run inside of MC loop up until the single_iteration_fun to get debug
  #  parms.
  
  # in_dt              = r_dt
  # weight_type        = p_weight_type
  # method             = p_method
  # lin_alpha          = p_lin_alpha
  # pctile             = p_pctile
  # weight_above       = p_weight_above
  # weight_below       = p_weight_below
  # v_alpha            = p_v_alpha
  # mrpctile           = p_mrpctile
  # mrdist             = p_mrpctile
  # npoints            = p_npoints
  # n_teacher          = p_n_teacher
  # n_stud_per_teacher = p_n_stud_per_teacher
  # test_SEM           = p_test_SEM
  # teacher_va_epsilon = p_teacher_va_epsilon
  # impact_type        = p_impact_type
  # impact_function    = p_impact_function
  # max_diff           = p_max_diff
  # covariates         = p_covariates
  # peer_effects       = p_peer_effects
  # stud_sorting       = p_stud_sorting
  # rho                = p_rho
  # ta_sd              = p_ta_sd
  # sa_sd              = p_sa_sd


  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #

  # Start of the function.
  single_iteration_fun <- function(in_dt               = NULL,
                                   weight_type         = NULL,
                                   method              = NULL, 
                                   lin_alpha           = NULL,
                                   pctile              = NULL,
                                   weight_below        = NULL,
                                   weight_above        = NULL,
                                   v_alpha             = NULL,
                                   mrpctile            = NULL, 
                                   mrdist              = NULL,
                                   npoints             = NULL,
                                   n_teacher           = NULL,
                                   n_stud_per_teacher  = NULL,
                                   test_SEM            = NULL,
                                   teacher_va_epsilon  = NULL,
                                   impact_type         = NULL,
                                   impact_function     = NULL,
                                   max_diff            = NULL,
                                   covariates          = NULL,
                                   peer_effects        = NULL,
                                   stud_sorting        = NULL,
                                   rho                 = NULL,
                                   ta_sd               = NULL,
                                   sa_sd               = NULL,
                                   center_ability_corr = NULL,
                                   weighted_average    = NULL) {
    
    # I need this for it to work on windows clusters since libraries are not
    #  loaded on every cluster.
    require(data.table)
    
    # Resample the student data.
    in_dt <- simulate_test_data(n_teacher           = n_teacher,
                                n_stud_per_teacher  = n_stud_per_teacher,
                                test_SEM            = test_SEM,
                                teacher_va_epsilon  = teacher_va_epsilon,
                                impact_type         = impact_type,
                                impact_function     = impact_function,
                                max_diff            = max_diff,
                                teacher_dt          = in_dt[, c("teacher_id",
                                                                "teacher_ability",
                                                                "teacher_center",
                                                                "teacher_max")],
                                covariates          = covariates,
                                peer_effects        = peer_effects,
                                stud_sorting        = stud_sorting,
                                rho                 = rho,
                                ta_sd               = ta_sd,
                                sa_sd               = sa_sd,
                                center_ability_corr = center_ability_corr)

    
    # Run the standard VA.
    va_tab1 <- standard_va_stat(in_dt              = in_dt,
                                weight_type        = weight_type,
                                method             = method, 
                                lin_alpha          = lin_alpha,
                                pctile             = pctile,
                                weight_below       = weight_below,
                                weight_above       = weight_above,
                                v_alpha            = v_alpha,
                                mrpctile           = mrpctile, 
                                mrdist             = mrdist,
                                npoints            = npoints,
                                n_teacher          = n_teacher,
                                n_stud_per_teacher = n_stud_per_teacher,
                                test_SEM           = test_SEM,
                                teacher_va_epsilon = teacher_va_epsilon,
                                impact_type        = impact_type,
                                impact_function    = impact_function,
                                max_diff           = max_diff,
                                covariates         = covariates,
                                peer_effects       = peer_effects,
                                stud_sorting       = stud_sorting,
                                rho                = rho,
                                ta_sd              = ta_sd,
                                sa_sd              = sa_sd)

    
    # Check method option.
    if (method=="bin") {
      
      # Run the binned VA.
      output <- binned_va_stat(in_dt              = in_dt,
                               weight_type        = weight_type,
                               method             = method, 
                               lin_alpha          = lin_alpha,
                               pctile             = pctile,
                               weight_below       = weight_below,
                               weight_above       = weight_above,
                               v_alpha            = v_alpha,
                               mrpctile           = mrpctile, 
                               mrdist             = mrdist,
                               npoints            = npoints,
                               n_teacher          = n_teacher,
                               n_stud_per_teacher = n_stud_per_teacher,
                               test_SEM           = test_SEM,
                               teacher_va_epsilon = teacher_va_epsilon,
                               impact_type        = impact_type,
                               impact_function    = impact_function,
                               max_diff           = max_diff,
                               covariates         = covariates,
                               peer_effects       = peer_effects,
                               stud_sorting       = stud_sorting,
                               rho                = rho,
                               ta_sd              = ta_sd,
                               sa_sd              = sa_sd)
      
    }
    
    
  if (method=="semip") {
    # put implementation here. Call output or rename that object everywhere 
    # not really a good name anyway 
    
    output <- semip_va(in_data = in_dt)
  }
  
  
  if (method=="np_hack") {

    # Run the NP VA.
    output <- np_hack_va_stat(in_dt              = in_dt,
                              weight_type        = weight_type,
                              method             = method, 
                              lin_alpha          = lin_alpha,
                              pctile             = pctile,
                              weight_below       = weight_below,
                              weight_above       = weight_above,
                              v_alpha            = v_alpha,
                              mrpctile           = mrpctile, 
                              mrdist             = mrdist,
                              npoints            = npoints,
                              n_teacher          = n_teacher,
                              n_stud_per_teacher = n_stud_per_teacher,
                              test_SEM           = test_SEM,
                              teacher_va_epsilon = teacher_va_epsilon,
                              impact_type        = impact_type,
                              impact_function    = impact_function,
                              max_diff           = max_diff,
                              covariates         = covariates,
                              peer_effects       = peer_effects,
                              stud_sorting       = stud_sorting,
                              rho                = rho,
                              ta_sd              = ta_sd,
                              sa_sd              = sa_sd,
                              weighted_average   = weighted_average)

  }  
  
    
    if (method=="qtle") {
      
      # Run the quantile VA.
      output <- quantile_va_stat(in_dt              = in_dt,
                                 weight_type        = weight_type,
                                 method             = method, 
                                 lin_alpha          = lin_alpha,
                                 pctile             = pctile,
                                 weight_below       = weight_below,
                                 weight_above       = weight_above,
                                 v_alpha            = v_alpha,
                                 mrpctile           = mrpctile, 
                                 mrdist             = mrdist,
                                 npoints            = npoints,
                                 n_teacher          = n_teacher,
                                 n_stud_per_teacher = n_stud_per_teacher,
                                 test_SEM           = test_SEM,
                                 teacher_va_epsilon = teacher_va_epsilon,
                                 impact_type        = impact_type,
                                 impact_function    = impact_function,
                                 max_diff           = max_diff,
                                 covariates         = covariates,
                                 peer_effects       = peer_effects,
                                 stud_sorting       = stud_sorting,
                                 rho                = rho,
                                 ta_sd              = ta_sd,
                                 sa_sd              = sa_sd)
      
    }
    
    
    # Merge on the standard VA
    va_tab2 <- merge(va_tab1, output, "teacher_id")
    
    
    # Return the estimates.
    return(va_tab2)
    
  } # End function.
