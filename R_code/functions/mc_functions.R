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
                             covariates         = NULL,
                             se_flag           = FALSE) {
  
  # Make formula 
  if (covariates == 0) {
    form <- as.formula("test_2 ~ test_1 - 1| teacher_id")
    
    
  } else {
    form <- as.formula("test_2 ~ test_1 + school_av_test + stud_sex +
                    stud_frpl + stud_att - 1| teacher_id")
  }
  
  # see if we need the standard errors, if yes use lfe 
  if(se_flag){
    
    # run regression 
    felm_out1 <- lfe::felm(form,
                           data = in_dt)
    
    # get fixed effects 
    estimates <- lfe::getfe(felm_out1,
                            se = TRUE)
    
    # format it 
    va_tab <- estimates[, c("idx", "effect", "se")]
    setnames(va_tab,  c("idx", "effect", "se"), c("teacher_id", "standard_welfare", "sd_standard"))

    # return it 
    return(va_tab[])
    
  # if we don't need SE we can use fixest
  }else{
    
    # set se to standard so its faster. NOt using them anyway 
    feols_out <- fixest::feols(form,
                               data = in_dt,
                               se = "standard")
    
    fixedEffects = fixest::fixef(feols_out)
    
    # format results 
    va_tab <- data.table(teacher_id = names(fixedEffects$teacher_id), standard_welfare = fixedEffects$teacher_id)
    
    # return results 
    return(va_tab[])
  }
  
  
  


}


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
                           lin_alpha          = NULL,
                           pctile             = NULL,
                           weight_below       = NULL,
                           weight_above       = NULL,
                           v_alpha            = NULL,
                           mrpctile           = NULL, 
                           mrdist             = NULL,
                           npoints            = NULL,
                           covariates         = NULL,
                           num_cats           = NULL,
                           qc_flag            = 0) {
  
  # Allow the bootstrap to pick the sample if needed.
  if (!is.null(boot)) {
    in_dt <- in_dt[index] 
  }
  
  # Estimate the binned VA.
  if (covariates == 0) {
    output <- binned_va(in_data = in_dt,
                        num_cats = num_cats)
  } else {
    output <- binned_va(in_data = in_dt,
                        reg_formula = paste0('test_2 ~ test_1',
                                             ' + teacher_id:categories',
                                             ' + school_av_test + stud_sex + ',
                                             'stud_frpl + stud_att - 1'),
                        num_cats = num_cats)
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
                              mrdist          = mrdist,
                              qc_flag         = qc_flag)
  
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
                             lin_alpha          = NULL,
                             pctile             = NULL,
                             weight_below       = NULL,
                             weight_above       = NULL,
                             v_alpha            = NULL,
                             mrpctile           = NULL, 
                             mrdist             = NULL,
                             npoints            = NULL,
                             covariates         = NULL) {
  
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
                            lin_alpha          = NULL,
                            pctile             = NULL,
                            weight_below       = NULL,
                            weight_above       = NULL,
                            v_alpha            = NULL,
                            mrpctile           = NULL, 
                            mrdist             = NULL,
                            npoints            = NULL,
                            covariates         = NULL,
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


## =========================================================================== #
# ======================== Single Iteration Function SDUSD  ================= #
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
single_iteration_fun <- function(teacher_ability_xwalk   = NULL,
                                       n_cohorts               = NULL,
                                       pretest_coef            = NULL,
                                       weight_type             = NULL,
                                       method                  = NULL,
                                       num_cats                = NULL,
                                       lin_alpha               = NULL,
                                       pctile                  = NULL,
                                       weight_below            = NULL,
                                       weight_above            = NULL,
                                       v_alpha                 = NULL,
                                       mrpctile                = NULL, 
                                       mrdist                  = NULL,
                                       npoints                 = NULL,
                                       test_SEM                = NULL,
                                       impact_type             = NULL,
                                       impact_function         = NULL,
                                       covariates              = NULL,
                                       peer_effects            = NULL,
                                       stud_sorting            = NULL,
                                       rho                     = NULL,
                                       weighted_average        = NULL) {
  
  # I need this for it to work on windows clusters since libraries are not
  #  loaded on every cluster.
  require(data.table)
  require(tidyr)
  
  # generate student data 
  r_student_dt <- simulate_sdusd_data(teacher_ability_xwalk   = teacher_ability_xwalk,
                                      n_cohorts               = n_cohorts,
                                      pretest_coef            = pretest_coef,
                                      impact_type             = impact_type,
                                      impact_function         = impact_function
                                      # test_SEM                 = 0.07,
                                      # covariates               = 0,
                                      # peer_effects             = 0,
                                      # stud_sorting             = 0,
                                      # rho                      = 0.2
  )
  
  
  # Run the standard VA.Don't need se's in the MC runs 
  va_tab1 <- standard_va_stat(in_dt              = r_student_dt,
                              covariates         = covariates,
                              se_flag            = FALSE)
  
  
  # Check method option.
  if (method %like% 'bin') {
    
    # Run the binned VA.
    output <- binned_va_stat(in_dt              = r_student_dt,
                             weight_type        = weight_type,
                             lin_alpha          = lin_alpha,
                             pctile             = pctile,
                             weight_below       = weight_below,
                             weight_above       = weight_above,
                             v_alpha            = v_alpha,
                             mrpctile           = mrpctile, 
                             mrdist             = mrdist,
                             npoints            = npoints,
                             covariates         = covariates,
                             num_cats           = num_cats)
    
    # Merge on the standard VA.
    va_tab1 <- merge(va_tab1,
                     setnames(output,
                              old=c('alternative_welfare'),
                              new=c('binned_welfare')),
                     'teacher_id')
    
  }
  
  
  if (method %like% 'semip') {
    # put implementation here. Call output or rename that object everywhere 
    # not really a good name anyway 
    
    output <- semip_va(in_data = r_student_dt)
    
    # Merge on the standard VA.
    va_tab1 <- merge(va_tab1,
                     setnames(output,
                              old=c('semip_welfare'),
                              new=c('binned_welfare')),
                     'teacher_id')
  }
  
  
  if (method %like% 'np') {
    
    # Run the NP VA.
    output <- np_hack_va_stat(in_dt              = r_student_dt,
                              weight_type        = weight_type,
                              lin_alpha          = lin_alpha,
                              pctile             = pctile,
                              weight_below       = weight_below,
                              weight_above       = weight_above,
                              v_alpha            = v_alpha,
                              mrpctile           = mrpctile, 
                              mrdist             = mrdist,
                              npoints            = npoints,
                              covariates         = covariates,
                              weighted_average   = weighted_average)
    
    # Merge on the standard VA.
    va_tab1 <- merge(va_tab1,
                     setnames(output,
                              old=c('alternative_welfare'),
                              new=c('np_welfare')),
                     'teacher_id')
  }  
  
  
  if (method %like% 'quantile') {
    
    # Run the quantile VA.
    output <- quantile_va_stat(in_dt              = r_student_dt,
                               weight_type        = weight_type,
                               lin_alpha          = lin_alpha,
                               pctile             = pctile,
                               weight_below       = weight_below,
                               weight_above       = weight_above,
                               v_alpha            = v_alpha,
                               mrpctile           = mrpctile, 
                               mrdist             = mrdist,
                               npoints            = npoints,
                               covariates         = covariates)
    
    # Merge on the standard VA.
    va_tab1 <- merge(va_tab1,
                     setnames(output,
                              old=c('alternative_welfare'),
                              new=c('quantile_welfare')),
                     'teacher_id')
  }
  
  
  # Return the estimates.
  return(va_tab1)
  
} # End function.


