#====================================#
# ==== set dir and load packages ====
#====================================#

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages and our functions 
library(data.table)
library(ggplot2)
library(xtable)

# load font things.  Must download Lato Regular 400 first: https://fonts.google.com/specimen/Lato
#install.packages("extrafont")
library(extrafont)
font_import()
loadfonts()


# set directories 
# check users
my_wd <- getwd()

# set paths based on users 
if(my_wd %like% "Nmath_000"){
  
  # set path for stress test data 
  stress_path <- "c:/Users/Nmath_000/Documents/data/Value Added/MC_stress_test/"
  #set path for plots to save 
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  

}else{
  stress_path <- '/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/stress_test/'
  out_plot <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/functions/"
}

#===========================#
# ==== load stress data ====
#===========================#

#note: in future iterations we can do this on the simulations.


plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 65),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =80),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =80))

# make sure that all the stress test data is alone in one folder 
# this lists all the fiels 
file_list <- list.files(stress_path)

# initalize list for data. It's most efficient if you set the length and I'm naming them for reference
stress_list <- setNames(vector("list", length = length(file_list)), file_list)

# load up files and add type indicators 
stress_test_readR <- function(file_name){
  # laod the data 
  out_dt <- fread(paste0(stress_path, file_name))
  
  # grab everything before the "_"
  file_info <- strsplit(file_name, "_")[[1]][1]
  
  # split it up by things before and after the first digit
  temp <- strsplit(file_info, "[[:digit:]]")[[1]]
  temp <- subset(temp, temp != "")
  
  # grab the info we need 
  weight_type <- temp[[1]]
  statistic <- temp[[2]]
  value   <- paste0(stringr::str_extract_all(file_info, "[[:digit:]]" )[[1]], collapse = "")
  
  # put info into data.table 
  out_dt[, weight_type := weight_type]
  out_dt[, statistic := statistic]
  out_dt[, value := value]
  # stor it in list 
  stress_list[[file_name]] <- out_dt
  
}

# now run this reader funciton on all the file paths 
stress_data_ls <- lapply(file_list, stress_test_readR)  
stress_data_dt <- rbindlist(stress_data_ls)

#=======================#
# ==== normalize it ====
#=======================#

# Renormalize everything so they have the same mean and variance
stress_data_dt[, mean_weighted_norm := (mean_weighted - mean(mean_weighted))/sd(mean_weighted), by = c("weight_type", "statistic", "value")]
stress_data_dt[, mean_standard_norm := (mean_standard - mean(mean_standard))/sd(mean_standard), by = c("weight_type", "statistic", "value")]
stress_data_dt[, true_ww := (true_ww - mean(true_ww))/sd(true_ww), by = c("weight_type", "statistic", "value")]

# now renormalize standard deviations 
stress_data_dt[, sd_weighted_norm := sd_weighted/sd(mean_weighted), by = c("weight_type", "statistic", "value")]
stress_data_dt[, sd_standard_norm := sd_standard/sd(mean_standard), by = c("weight_type", "statistic", "value")]

# function to make WW caterpillar plots and save them 
cat_plottR_st <- function(in_data){
  setorder(in_data, mean_weighted_norm)
  in_data[, ww_id :=.I]
  in_data[, ww_lc := mean_weighted_norm - 1.96*sd_weighted_norm]
  in_data[, ww_uc := mean_weighted_norm + 1.96*sd_weighted_norm]
  
  in_data[, st_id :=.I]
  in_data[, st_lc := mean_standard_norm - 1.96*sd_standard_norm]
  in_data[, st_uc := mean_standard_norm + 1.96*sd_standard_norm]
  
  weight_type <- unique(in_data$weight_type)
  statistic <- unique(in_data$statistic)
  value <- unique(in_data$value)
  
  test <- melt(in_data, id.vars = 'tid', measure.vars = c('mean_standard_norm', 'mean_weighted_norm', 'true_ww'))
  
  ww_cat_plot <- ggplot(test, aes(x = tid, y = value, color = variable)) + geom_point(size = 3) +
    ggtitle(paste0(weight_type, " Weight ", statistic, " = ", value)) + 
    ylab("Welfare Added") + 
    xlab("True Teacher Order") +
    ylim(-6,6) +
    theme(legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key.size = unit(.5, "cm")) + 
    scale_color_manual(labels = c('Standard', 'Weighted', 'True Value'), values = c("#77aadd", "#ffaabb", "black"))
  
  # save plot  
  ggsave(paste0(out_plot, "ww_", weight_type, "_caterpillar", statistic, value, ".pdf"), ww_cat_plot, width=4, height=4)
  
  return(ww_cat_plot)
  
}

#cat_plottR_st(stress_data_dt[value == 15, ])
#cat_plottR_st(stress_data_dt[value == 30, ])
#cat_plottR_st(stress_data_dt[value == 60, ])
#cat_plottR_st(stress_data_dt[value == 100, ])
#cat_plottR_st(stress_data_dt[value == 140, ])
#cat_plottR_st(stress_data_dt[value == 200, ])

cat_plottR_st(stress_data_dt[value == 5, ])
cat_plottR_st(stress_data_dt[value == 10, ])
cat_plottR_st(stress_data_dt[value == 15, ])
cat_plottR_st(stress_data_dt[value == 20, ])
cat_plottR_st(stress_data_dt[value == 25, ])
cat_plottR_st(stress_data_dt[value == 30, ])

