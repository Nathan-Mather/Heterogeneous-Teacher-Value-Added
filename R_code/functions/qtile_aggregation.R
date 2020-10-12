#===============================#
# ==== VA Aggregation qtile ====
#===============================#

# load sampel data just to test this out 
test_data <- fread("file:///C:/Users/Nmath_000/Documents/data/Value Added/int_data/qtile_output.csv")
coefs_dt <- fread("C:/Users/Nmath_000/Documents/data/Value Added/int_data/qtile_output_long.csv")

# load model_xwalk to see what parms I should use for the example 
model_xwalk <- data.table(read_excel(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk.xlsx")))

in_data  = r_dt
in_coefs = coefs_dt
lin_alpha    = lin_alpha
pctile       = pctile
v_alpha      = v_alpha
mrpctile     = mrpctile
mrdist       = mrdist

# current dependencies 
# the variable name "tau" , "value", "se", "teacher_id" 


#===============================================#
# ==== get weights for the estimated points ====
#===============================================#

# first need to get the relevent percentile point 
w_pctile_val <- quantile(in_data$test_1, pctile)

# next need to fill in the quantiles with actual values 
tau_xwalk <- data.table(tau = unique(coefs_dt$tau))
tau_xwalk[, tau_val := quantile(in_data$test_1, 
                                probs = tau)]

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

