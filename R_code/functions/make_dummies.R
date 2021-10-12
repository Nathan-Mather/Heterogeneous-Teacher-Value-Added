###################
# dummy function #
################## 

#  need a function to make dummy variables for teacher value added matrices 
# it will work by reference on the data.table to avoid copying a large data set 
# but, that means it wont return anything and will just alter in_data 


make_dummies <- function(in_data       = NULL,
                    to_dummy_var = NULL ){
  
  
  # find unique variable categories
  dummy_values <- in_data[, unique(get(to_dummy_var))]
  
  # loop over these valies 
  for(dum_val_i in dummy_values){
    
    
    # create category column in dataset
    in_data[, paste0("d_", to_dummy_var,"_",dum_val_i) := 0]
    
    # add 1 in column for rows where category applies
    in_data[ get(to_dummy_var) == dum_val_i, paste0("d_",to_dummy_var,"_", dum_val_i) := 1]
  }
  me_dt[]
  return("Dummy Variables added to in_data")
  
}
