

#=====================#
# ==== MC figures ====
#=====================#

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
library(gridExtra)


# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  in_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
  # path for plots
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_plots/"
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  in_data <- "~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code"
  
}


#=========================#
# ==== set plot style ====
#=========================#

plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 20),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =25),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =25),
        legend.title = element_text(size=10))


#====================#
# ==== load data ====
#====================#

# get the most recent data 
files <- list.files(in_data)
res_files <- grep("mc_results_",files, value = TRUE)
res_files <- sort(res_files,decreasing = TRUE)

# load most recent data 
res_dt <- fread(paste0(in_data, res_files[[1]]))

# load the most recent xwalk corresponding to that data 
xwalk_files <- grep("mc_xwalk_",files, value = TRUE)
xwalk_files <- sort(xwalk_files,decreasing = TRUE)

# load most recent xwalk 
model_xwalk <- fread(paste0(in_data, xwalk_files[[1]]))

# check that the xwalk and data match 
date_1 <- gsub("mc_xwalk_", "", xwalk_files[[1]])
date_2 <- gsub("mc_results_", "",  res_files[[1]])
if(date_1 != date_2) warning("xwalk date and data date don't match. Make sure they actually go together so you dont mislabel graphs")

if(nrow(model_xwalk) != length(unique(res_dt$run_id))) stop("The Xwalk information does not match up to the data you loaded in")
  
#===============================#
# ==== normalize everything ====
#===============================#

# Renormalize everything so they have the same mean and variance
res_dt[, mean_ww_norm := (mean_ww - mean(mean_ww))/sd(mean_ww), by = run_id]
res_dt[, mean_standard_norm := (mean_standard - mean(mean_standard))/sd(mean_standard), by = run_id]
res_dt[, true_ww_impact := (true_ww_impact - mean(true_ww_impact))/sd(true_ww_impact), by = run_id]

# now renormalize standard deviations 
res_dt[, sd_ww_norm := sd_ww/sd(mean_ww), by = run_id]
res_dt[, sd_standard_norm := sd_standard/sd(mean_standard), by = run_id]

#====================#
# ==== get ranks ====
#====================#

setorder(res_dt, mean_standard_norm)
res_dt[, standard_rank := 1:.N, run_id]
res_dt[, standard_lc := mean_standard_norm - 1.96*sd_standard_norm]
res_dt[, standard_uc := mean_standard_norm + 1.96*sd_standard_norm]

setorder(res_dt, mean_ww_norm)
res_dt[, ww_rank := 1:.N, run_id]
res_dt[, ww_lc := mean_ww_norm - 1.96*sd_ww_norm]
res_dt[, ww_uc := mean_ww_norm + 1.96*sd_ww_norm]

setorder(res_dt, true_ww_impact)
res_dt[, true_ww_rank :=1:.N, run_id]

#================================#
# ==== get distance measures ====
#================================#
# Calculate the mean squared distance from the rank of the truth
res_dt[, standard_MSE := (standard_rank - true_ww_rank)^2]
res_dt[, standard_MAE := abs(standard_rank - true_ww_rank)]

res_dt[, ww_MSE := (ww_rank - true_ww_rank)^2]
res_dt[, ww_MAE := abs(ww_rank - true_ww_rank)]


#==========================#
# ==== loop over xwalk ====
#==========================#


for(i in 1:nrow(model_xwalk)){
  
  # grab run_id 
  run_id_i <- model_xwalk[i, run_id]
  
  # subset data to this mc run 
  res_sub <- res_dt[run_id == run_id_i]
  
  # get table of run info 
  # make table of parameters 
  parms_tab <- melt.data.table(model_xwalk[i], measure.vars = colnames(model_xwalk))
  parms_tab <- parms_tab[!is.na(value)]
  
  # put parameters in grob
  parms_tbl <- tableGrob(parms_tab, rows=NULL, cols = NULL, theme = ttheme_default(base_size = 9))

  #===============================#
  # ==== standard caterpillar ====
  #===============================#
  

    standard_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_standard_norm)) +
      geom_point(size = 1.5, aes(color = "Standard VA"), alpha = 1) + 
      geom_point(aes( y = true_ww_impact,  color = "Truth"),size = 1, alpha = .4) +
      scale_color_manual(values= c("#ffaabb", "#77AADD")) +
      geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width= 1, color = "#ffaabb") +
      ylab("Impact") + 
      xlab("True Teacher Order") +
      ylim(-6,6)+
      plot_attributes + 
      theme(legend.title = element_blank(),
            legend.position = c(0.8, 0.8))
    
  # add parameters 
  standard_cat_plot2 <- grid.arrange(standard_cat_plot, parms_tbl,
                             layout_matrix = rbind(c(1, 1, 1, 2)))


  # save it 
  ggsave(filename = paste0(out_plot,"standard_cat_run_",  run_id_i, ".png"), 
         plot     = standard_cat_plot2, 
         width    = 9, 
         height   = 4)
  

  
  #=========================#
  # ==== ww caterpillar ====
  #=========================#
  

    ww_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_ww_norm)) +
      geom_point(aes(color = "Weighted VA"), size = 2,  alpha = 1) + 
      geom_point(aes( y = true_ww_impact,  color = "Truth"),size = 1, alpha = .4) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width= 1, color = "#ffaabb") +
      ylab("Impact") + 
      xlab("True Teacher Order") +
      ylim(-6,6)+
      xlim(0,150)+
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = c(0.2, 0.8))
  # add parameters 
  ww_cat_plot2 <- grid.arrange(ww_cat_plot, parms_tbl,
                                     layout_matrix = rbind(c(1, 1, 1, 2)))
  # save plot  
  ggsave(filename = paste0(out_plot,"ww_cat_run_",  run_id_i, ".png"), 
         plot     = ww_cat_plot2, 
         width    = 9, 
         height   = 4)


  #======================================#
  # ==== histogram and summary stats ====
  #======================================#
  

    # start a data.table of results 
    sum_stats <- list()
    sum_stats[[1]] <- data.table(Statistic = "Mean Squared Distance", 
                                 Standard = mean(res_sub$standard_MSE),
                                 Weighted = mean(res_sub$ww_MSE))
    
    sum_stats[[2]] <- data.table(Statistic = "Mean Absolute Distance", 
                                 Standard =  mean(res_sub$standard_MAE),
                                 Weighted =  mean(res_sub$ww_MAE))
    
    
    sum_stats[[3]] <- data.table(Statistic = "Correlation to Truth", 
                                 Standard =  cor(res_sub$mean_standard_norm, res_sub$true_ww_rank, method  = "kendall" , use="pairwise"),
                                 Weighted = cor(res_sub$mean_ww_norm, res_sub$true_ww_rank, method  = "kendall" , use="pairwise"))

    out_sum_stats <- rbindlist(sum_stats)
    
    # put sum stats in a grob
    out_sum_stats_tbl <- tableGrob(out_sum_stats, rows=NULL, theme = ttheme_default(base_size = 8))
    
    # Histogram of the density and distance of rank inversions
    # set binwidth parm
    b_width <- 3
    
    out_histogram <- ggplot(res_sub) + 
      geom_histogram( aes(standard_MAE, fill = "Standard"), alpha = .4, colour="black", binwidth = b_width) +
      geom_histogram( aes(ww_MAE, fill = "Weighted"), alpha = .4, colour="black", binwidth = b_width) +
      ylab("Number of Teachers") +
      xlab("Difference in Rank From Truth")+
      scale_fill_manual(values= c("#77AADD", "#EE8866")) +
      plot_attributes + 
      theme(legend.title = element_blank(),
            legend.position = c(0.8, 0.8),
            legend.key.size = unit(.5, "cm"))
    
    
    # add parameters 
    out_histogram2 <- grid.arrange(out_histogram, parms_tbl, out_sum_stats_tbl,
                                 layout_matrix = rbind(c(1, 1, 1, 2),
                                                       c(1, 1, 1, 2),
                                                       c(1, 1, 1, 2),
                                                       c(3, 3, 3, 3)))
    
    # save plot  
    ggsave(filename = paste0(out_plot,"hist_run_",  run_id_i, ".png"), 
           plot     = out_histogram2, 
           width    = 9, 
           height   = 4)
    
}# close for loop 

