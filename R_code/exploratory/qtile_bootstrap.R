#=============================#
# ==== Bootstrap quantile ====
#=============================#

# load example data 
example_data <- fread("C:/Users/Nmath_000/Documents/data/Value Added/int_data/reg_input_example.csv")
# weight_type <-"rawlsian" # "linear" or "rawlsian" or "equal" or "v" or "mr"
# lin_alpha = 2 # For linear weights
# pctile = .4 # For rawlsian weights
# v_alpha = 1 # For v weights
# mrpctile = .3 # For mr weights
# mrdist = .2 # for mr weights
# method =  "wls" # "wls" or "semip" or "qtle"
# weight_below = .8
# weight_above = .2
# data = example_data
# index <- sample(c(1:nrow(data)))

# function to sample from 
to_boot_qitle <- function(data         = NULL, 
                          index        = NULL,
                          in_coefs     = qtile_res,
                          weight_type  = weight_type,
                          lin_alpha    = lin_alpha,
                          pctile       = pctile,
                          weight_below = weight_below,
                          weight_above = weight_above,
                          v_alpha      = v_alpha,
                          mrpctile     = mrpctile,
                          mrdist       = mrdist){
  
  n_data <- data[index]
  
  qtile_res <- qtilep_va(in_data = n_data,
                         in_teacher_id = "teacher_id",
                         in_pre_test   = "test_1",
                         in_post_test  = "test_2",
                         ptle = seq(.05,.95,by=.1))
  
  
  # Now aggregate them 
  output    <- qtile_agg(in_test_1   = n_data$test_1,
                         in_coefs     = qtile_res,
                         weight_type  = weight_type,
                         lin_alpha    = lin_alpha,
                         pctile       = pctile,
                         weight_below = weight_below,
                         weight_above = weight_above,
                         v_alpha      = v_alpha,
                         mrpctile     = mrpctile,
                         mrdist       = mrdist)
  
  return(output$ww_qtile_est)
  
}

boot_res <- boot(data         = example_data,
                 statistic    = to_boot_qitle,
                 R            = 10,
                 in_coefs     = qtile_res,
                 weight_type  = weight_type,
                 lin_alpha    = lin_alpha,
                 pctile       = pctile,
                 weight_below = weight_below,
                 weight_above = weight_above,
                 v_alpha      = v_alpha,
                 mrpctile     = mrpctile,
                 mrdist       = mrdist)
