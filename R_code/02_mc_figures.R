# ======================================= #
# ==== Generate the Caterpillar Plot ==== #
# ======================================= ## clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
# no scientific notation 
options(scipen = 999)
# clean console history 
cat("\f")

#==================================#
# ==== File paths and packages ====
#==================================#

# set seeds 
set.seed(42)

# load packages 
library(data.table)
library(ggplot2)



# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  in_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  in_data <- "~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code"
  
}

#====================#
# ==== load data ====
#====================#

# get the most recent data 
files <- list.files(in_data)
files <- grep("mc_results_",files, value = TRUE)
files <- sort(files,decreasing = TRUE)

# load most recent data 
res_dt <- fread(paste0(in_data, files[[1]]))


#==============================#
# ==== Difference measures ====
#==============================#


# Calculate the mean squared distance from the rank of the truth
res_dt <- res_dt[order(mean_standard_norm)]
res_dt[, baseline := (.I - teacher_id)^2]
res_dt[, baseline_count := (.I - teacher_id != 0)]
res_dt[, baseline_count_num := abs(.I - teacher_id)]

res_dt <- res_dt[order(mean_weighted)]
res_dt[, weighted := (.I - teacher_id)^2]
res_dt[, weighted_count := (.I - teacher_id != 0)]
res_dt[, weighted_count_num := abs(.I - teacher_id)]

# Display the mean squared distance for both
sum(res_dt$baseline)
sum(res_dt$weighted)

# Display the number of rank inversions for both
sum(res_dt$baseline_count)
sum(res_dt$weighted_count)

#==================#
# ==== Figures ====
#==================#

# Histogram of the density and distance of rank inversions
c1 <- rgb(0, 0, 255,max = 255, alpha = 80, names = "blue")
c2 <- rgb(0, 255, 0, max = 255, alpha = 80, names = "green")

h1 <- hist(res_dt$baseline_count_num, breaks=seq(0,max(res_dt$baseline_count_num),l=15))
h2 <- hist(res_dt$weighted_count_num, breaks=seq(0,max(res_dt$weighted_count_num),l=15))

plot(h1, col = c1)
plot(h2, col = c2, add = TRUE) # Blue is the baseline, green the weighted

#plot(r_dt$stud_ability_1, r_dt$weights_true)

# differences in correlation 
res_dt[, cor(mean_standard, true_ww)]
res_dt[, cor(mean_weighted, true_ww)]

# Let go of the processors
stopCluster(myCluster)

#===================#
# ==== MC plots ====
#===================#

# fill in all the weight types 
mc_linear[, weight_type := "Linear"]
mc_kernal[, weight_type := "Kernal"]
mc_rawlsian[, weight_type := "rawlsian"]

#===============================#
# ==== normalize everything ====
#===============================#

for( in_data in list(mc_linear,mc_kernal, mc_rawlsian)){
  # Renormalize everything so they have the same mean and variance
  in_data[, mean_weighted_norm := (mean_weighted - mean(mean_weighted))/sd(mean_weighted)]
  in_data[, mean_standard_norm := (mean_standard - mean(mean_standard))/sd(mean_standard)]
  in_data[, true_ww := (true_ww - mean(true_ww))/sd(true_ww)]
  
  # now renormalize standard deviations 
  in_data[, sd_weighted_norm := sd_weighted/sd(mean_weighted)]
  in_data[, sd_standard_norm := sd_standard/sd(mean_standard)]
}

#===============================#
# ==== standard caterpillar ====
#===============================#

# function to make standard caterpillar plots and save them 
cat_plottR_st <- function(in_data){
  setorder(in_data, mean_standard_norm)
  in_data[, standard_id :=.I]
  in_data[, standard_lc := mean_standard_norm - 1.96*sd_standard_norm]
  in_data[, standard_uc := mean_standard_norm + 1.96*sd_standard_norm]
  weight_type <- unique(in_data$weight_type)
  
  standard_cat_plot <- ggplot(in_data, aes(x = tid, y = mean_standard_norm)) +
    geom_point(size = 5, aes(color = "Standard VA"), alpha = 1) + 
    geom_point(aes( y = true_ww,  color = "Truth"),size = 4, alpha = .4) +
    scale_color_manual(values= c("#ffaabb", "#77AADD")) +
    geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width= 1, color = "#ffaabb") +
    ylab("Traditional Value Added") + 
    xlab("True Teacher Order") +
    ylim(-6,6)+
    plot_attributes + 
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  
  # save plot  
  png(paste0(paste0(out_plot,"standard_", weight_type, "_caterpillar.png")),
      height = 1500, width = 1500, type = "cairo")
  print(standard_cat_plot)
  dev.off()  
  
  return(standard_cat_plot)
  
}

# run this. Would be easier to lapply over a list but already set up with way...
cat_plottR_st(mc_linear)
cat_plottR_st(mc_kernal)
cat_plottR_st(mc_rawlsian)


#=========================#
# ==== ww caterpillar ====
#=========================#
# function to make WW caterpillar plots and save them 
cat_plottR_ww <- function(in_data){
  setorder(in_data, mean_weighted_norm)
  in_data[, ww_id :=.I]
  in_data[, ww_lc := mean_weighted_norm - 1.96*sd_weighted_norm]
  in_data[, ww_uc := mean_weighted_norm + 1.96*sd_weighted_norm]
  weight_type <- unique(in_data$weight_type)
  
  ww_cat_plot <- ggplot(in_data, aes(x = tid, y = mean_weighted_norm)) +
    geom_point(aes(color = "Weighted VA"), size = 5,  alpha = 1) + 
    geom_point(aes( y = true_ww,  color = "Truth"),size = 4, alpha = .4) +
    scale_color_manual(values= c("#77AADD", "#ffaabb"),
                       guide=guide_legend(reverse=TRUE)) +
    geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width= 1, color = "#ffaabb") +
    ylab("WLS Welfare Added") + 
    xlab("True Teacher Order") +
    ylim(-6,6)+
    xlim(0,150)+
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  # save plot  
  png(paste0(paste0(out_plot,"ww_", weight_type, "_caterpillar.png")),
      height = 1500, width = 1500, type = "cairo")
  print(ww_cat_plot)
  dev.off()  
  
  return(ww_cat_plot)
  
}

# run this. Would be easier to lapply over a list but already set up with way...
cat_plottR_ww(mc_linear)
cat_plottR_ww(mc_kernal)
cat_plottR_ww(mc_rawlsian)

#======================================#
# ==== histogram and summary stats ====
#======================================#

histogram_sum_Stat_fun <- function(in_data){
  
  # grab weight type 
  weight_type <- unique(in_data$weight_type)
  
  # Calculate the mean squared distance from the rank of the truth
  setorder(in_data, mean_standard_norm)
  in_data[, baseline := (.I - tid)^2]
  in_data[, baseline_count := (.I - tid != 0)]
  in_data[, baseline_count_num := abs(.I - tid)]
  
  setorder(in_data, mean_weighted_norm)
  in_data[, weighted := (.I - tid)^2]
  in_data[, weighted_count := (.I - tid != 0)]
  in_data[, weighted_count_num := abs(.I - tid)]
  
  # start a data.table of results 
  sum_stats <- list()
  sum_stats[[1]] <- data.table(Statistic = "Mean Squared Distance", 
                               Standard = mean(in_data$baseline),
                               Weighted = mean(in_data$weighted))
  
  sum_stats[[2]] <- data.table(Statistic = "Mean Absolute Distance", 
                               Standard =  mean(in_data$baseline_count_num),
                               Weighted =  mean(in_data$weighted_count_num))
  
  
  sum_stats[[3]] <- data.table(Statistic = "Correlation to Truth", 
                               Standard =  cor(in_data$mean_standard_norm, in_data$true_ww),
                               Weighted = cor(in_data$mean_weighted_norm, in_data$true_ww))
  
  
  out_sum_stats <- rbindlist(sum_stats)
  # save summ stats as latex table 
  print(xtable(out_sum_stats, type = "latex"),
        file = paste0(out_plot,"sum_stats_", weight_type, ".tex"),
        include.rownames = FALSE,
        floating = FALSE)
  
  # Histogram of the density and distance of rank inversions
  # set binwidth parm
  b_width <- 3
  
  out_histogram <- ggplot(in_data) + 
    geom_histogram( aes(baseline_count_num, fill = "Standard"), alpha = .4, colour="black", binwidth = b_width) +
    geom_histogram( aes(weighted_count_num, fill = "Weighted"), alpha = .4, colour="black", binwidth = b_width) +
    ylab("Number of Teachers") +
    xlab("Difference in Rank From Truth")+
    scale_fill_manual(values= c("#77AADD", "#EE8866")) +
    plot_attributes + 
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8),
          legend.key.size = unit(4, "cm"))
  
  # save plot  
  png(paste0(paste0(out_plot,"Histrogram_", weight_type, ".png")),
      height = 1100, width = 2300, type = "cairo")
  print(out_histogram)
  dev.off()  
  
  # LIST OF OUTPUT 
  out_list <- list()
  out_list[["hist"]] <- out_histogram
  out_list[["sum_stats"]] <- out_sum_stats
}


# run on different simulations 
histogram_sum_Stat_fun(mc_linear)
histogram_sum_Stat_fun(mc_kernal)
histogram_sum_Stat_fun(mc_rawlsian)

#============================#
# ==== stress test plots ====
#============================#

#===========================#
# ==== load stress data ====
#===========================#

#note: in future iterations we can do this on the simulations.

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


#=================================#
# ==== stress test histograms ====
#=================================#

# make histograms for various student levels 
histogram_stress_fun <- function(in_data){
  
  # grab data info
  weight_type <- unique(in_data$weight_type)
  statistic <- unique(in_data$statistic)
  value <- unique(in_data$value)
  
  # Calculate the mean squared distance from the rank of the truth
  setorder(in_data, mean_standard_norm)
  in_data[, baseline := (.I - tid)^2]
  in_data[, baseline_count := (.I - tid != 0)]
  in_data[, baseline_count_num := abs(.I - tid)]
  
  setorder(in_data, mean_weighted_norm)
  in_data[, weighted := (.I - tid)^2]
  in_data[, weighted_count := (.I - tid != 0)]
  in_data[, weighted_count_num := abs(.I - tid)]
  
  
  # Histogram of the density and distance of rank inversions
  # set binwidth parm
  b_width <- 3
  
  out_histogram <- ggplot(in_data) + 
    geom_histogram( aes(baseline_count_num, fill = "Standard"), alpha = .4, colour="black", binwidth = b_width) +
    geom_histogram( aes(weighted_count_num, fill = "Weighted"), alpha = .4, colour="black", binwidth = b_width) +
    ggtitle(paste0(value," ", statistic)) + 
    ylab("Number of Teachers") +
    xlab("Difference in Rank From Truth")+
    scale_fill_manual(values= c("#77AADD", "#EE8866")) +
    scale_x_continuous(limits = c(-3,130)) +
    ylim(0,70) +
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8),
          legend.key.size = unit(4, "cm"))
  
  
  # save plot  
  png(paste0(paste0(out_plot,"Histrogram_", weight_type, "_",statistic, "_",value, ".png")),
      height = 1200, width = 2100, type = "cairo")
  print(out_histogram)
  dev.off()  
  # LIST OF OUTPUT 
  return(out_histogram)
  
}

# split data. can't use previous list since I normalized the stacked dt 
stress_data_ls<- split(stress_data_dt[statistic == "Students"], by = c("weight_type", "statistic", "value"))

# now apply function to list 
res <- lapply(stress_data_ls, histogram_stress_fun)


#==================================#
# ==== stress test catarpillar ====
#==================================#
# These should really be combined with the functions above but THIS IS LASY MINUTE STUF 
# function to make WW caterpillar plots and save them 
cat_plottR_ww_stress <- function(in_data){
  
  # grab data info
  weight_type <- unique(in_data$weight_type)
  statistic <- unique(in_data$statistic)
  value <- unique(in_data$value)
  
  setorder(in_data, mean_weighted_norm)
  in_data[, ww_id :=.I]
  in_data[, ww_lc := mean_weighted_norm - 1.96*sd_weighted_norm]
  in_data[, ww_uc := mean_weighted_norm + 1.96*sd_weighted_norm]
  
  ww_cat_plot <- ggplot(in_data, aes(x = tid, y = mean_weighted_norm)) +
    geom_point(aes(color = "Weighted VA"), size = 5,  alpha = 1) + 
    geom_point(aes( y = true_ww,  color = "Truth"),size = 4, alpha = .4) +
    scale_color_manual(values= c("#77AADD", "#ffaabb"),
                       guide=guide_legend(reverse=TRUE)) +
    geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width= 1, color = "#ffaabb") +
    ylab("WLS Welfare Added") + 
    xlab("True Teacher Order") +
    ggtitle(paste0(value," ", statistic))+
    ylim(-6,6)+
    xlim(0,150)+
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  # save plot  
  png(paste0(paste0(out_plot,"ww_", weight_type, "_",statistic, "_",value, "_caterpillar.png")),
      height = 1500, width = 1500, type = "cairo")
  print(ww_cat_plot)
  dev.off()  
  
  return(ww_cat_plot)
  
}
res_ww_cat <- lapply(stress_data_ls, cat_plottR_ww_stress)


cat_plottR_st_stress <- function(in_data){
  
  # grab data info
  weight_type <- unique(in_data$weight_type)
  statistic <- unique(in_data$statistic)
  value <- unique(in_data$value)
  
  setorder(in_data, mean_standard_norm)
  in_data[, standard_id :=.I]
  in_data[, standard_lc := mean_standard_norm - 1.96*sd_standard_norm]
  in_data[, standard_uc := mean_standard_norm + 1.96*sd_standard_norm]
  weight_type <- unique(in_data$weight_type)
  
  ww_cat_plot <- ggplot(in_data, aes(x = tid, y = mean_standard_norm)) +
    geom_point(aes(color = "Standard VA"), size = 5,  alpha = 1) + 
    geom_point(aes( y = true_ww,  color = "Truth"),size = 4, alpha = .4) +
    scale_color_manual(values= c("#ffaabb", "#77AADD")) +
    geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width= 1, color = "#ffaabb") +
    ylab("Value Added") + 
    xlab("True Teacher Order") +
    ggtitle(paste0(value," ", statistic))+
    ylim(-6,6)+
    xlim(0,150)+
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  # save plot  
  png(paste0(paste0(out_plot,"st_", weight_type, "_",statistic, "_",value, "_caterpillar.png")),
      height = 1500, width = 1500, type = "cairo")
  print(ww_cat_plot)
  dev.off()  
  
  return(ww_cat_plot)
  
}
res_st_cat <- lapply(stress_data_ls, cat_plottR_st_stress)
