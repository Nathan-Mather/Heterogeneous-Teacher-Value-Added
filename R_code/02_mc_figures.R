

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
  base_path <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  in_data <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/"
  
  # path for plots
  out_plot <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/Figures/"
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
res_dt[, true_welfare_norm := (true_welfare - mean(true_welfare))/sd(true_welfare), by = run_id]

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

setorder(res_dt, true_welfare_norm)
res_dt[, true_ww_rank :=1:.N, run_id]

setorder(res_dt, teacher_center)
res_dt[, cent :=1:.N, run_id]

#================================#
# ==== get distance measures ====
#================================#
# Calculate the mean squared distance from the rank of the truth
res_dt[, standard_MSE := (standard_rank - true_ww_rank)^2]
res_dt[, standard_MAE := abs(standard_rank - true_ww_rank)]

res_dt[, ww_MSE := (ww_rank - true_ww_rank)^2]
res_dt[, ww_MAE := abs(ww_rank - true_ww_rank)]


#==============================#
# ==== plots for every run ====
#==============================#

for(i in 1:nrow(model_xwalk)){
  
  # grab run_id 
  run_id_i <- model_xwalk[i, run_id]
  
  # subset data to this mc run 
  res_sub <- res_dt[run_id == run_id_i]
  
  # get table of run info 
  # make table of parameters 
  parms_tab <- melt.data.table(model_xwalk[i], measure.vars = colnames(model_xwalk))
  parms_tab <- parms_tab[!is.na(value)]
  parms_tab <- parms_tab[variable != "run_id"]
  parms_tab <- parms_tab[variable != "single_run"]
  
  # put parameters in grob
  parms_tbl <- tableGrob(parms_tab, rows=NULL, cols = NULL, theme = ttheme_default(base_size = 9))

  #===============================#
  # ==== standard caterpillar ====
  #===============================#
  
  truth_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = true_welfare_norm)) +
    geom_point(size = 1.5, aes(color = "Truth"), alpha = 1) + 
    scale_color_manual(values= c("#77AADD")) +
    ylab("Impact") + 
    xlab("True Teacher Order") +
    ylim(-6,6)+
    plot_attributes + 
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))

  standard_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_standard_norm)) +
    geom_point(size = 1.5, aes(color = "Standard VA"), alpha = 1) + 
    geom_point(aes( y = true_welfare_norm,  color = "Truth"),size = 1, alpha = .4) +
    scale_color_manual(values= c("#ffaabb", "#77AADD")) +
    geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width= 1, color = "#ffaabb") +
    ylab("Impact") + 
    xlab("True Teacher Order") +
    ylim(-6,6)+
    plot_attributes + 
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  print(standard_cat_plot)
    
  standard_center_plot <- ggplot(res_sub, aes(x = teacher_center, y = mean_standard_norm)) +
    geom_point(size = 1.5, aes(color = "Standard VA"), alpha = 1) +
    geom_point(aes( y = true_welfare_norm, color = "Truth"),size = 1, alpha = .4) +
    scale_color_manual(values= c("#ffaabb", "#77AADD")) +
    ylab("Impact") +
    xlab("True Center") +
    ylim(-6,6)+
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  
  welfare_center_plot <- ggplot(res_sub, aes(x = teacher_center, y = mean_ww_norm)) +
    geom_point(size = 1.5, aes(color = "Welfare VA"), alpha = 1) +
    geom_point(aes( y = true_welfare_norm, color = "Truth"),size = 1, alpha = .4) +
    scale_color_manual(values= c("#77AADD", "#ffaabb"),
    guide=guide_legend(reverse=TRUE)) +
    ylab("Impact") +
    xlab("True Center") +
    ylim(-6,6)+
    plot_attributes +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))

  # save it 
  ggsave(filename = paste0(out_plot,"truth_cat_run_",  run_id_i, ".png"), 
         plot     = truth_cat_plot, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot,"standard_cat_run_",  run_id_i, ".png"), 
         plot     = standard_cat_plot, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot,"standard_cent_run_",  run_id_i, ".png"), 
         plot     = standard_center_plot, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot,"welfare_cent_run_",  run_id_i, ".png"), 
         plot     = welfare_center_plot, 
         width    = 9, 
         height   = 4)  

  
  #=========================#
  # ==== ww caterpillar ====
  #=========================#
  

    ww_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_ww_norm)) +
      geom_point(aes(color = "Weighted VA"), size = 2,  alpha = 1) + 
      geom_point(aes( y = true_welfare_norm,  color = "Truth"),size = 1, alpha = .4) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width= 1, color = "#ffaabb") +
      ylab("Impact") + 
      xlab("True Teacher Order") +
      ylim(-6,6)+
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = c(0.2, 0.8))
  print(ww_cat_plot)
  # add parameters 
  #ww_cat_plot2 <- grid.arrange(ww_cat_plot, parms_tbl,
  #                                   layout_matrix = rbind(c(1, 1, 1, 2)))
  # save plot  
  ggsave(filename = paste0(out_plot,"ww_cat_run_",  run_id_i, ".png"), 
         plot     = ww_cat_plot, 
         width    = 9, 
         height   = 4)


  #======================================#
  # ==== histogram and summary stats ====
  #======================================#
  

    # start a data.table of results 
    sum_stats <- list()
    sum_stats[[1]] <- data.table(Statistic = "Mean Squared Distance", 
                                 Standard = round(mean(res_sub$standard_MSE), digits=2),
                                 Weighted = round(mean(res_sub$ww_MSE), digits=2))
    
    sum_stats[[2]] <- data.table(Statistic = "Mean Absolute Distance", 
                                 Standard =  round(mean(res_sub$standard_MAE), digits=2),
                                 Weighted =  round(mean(res_sub$ww_MAE), digits=2))
    
    
    sum_stats[[3]] <- data.table(Statistic = "Correlation to Truth", 
                                 Standard =  round(cor(res_sub$standard_rank, res_sub$true_ww_rank, method  = "kendall" , use="pairwise"), digits=2),
                                 Weighted = round(cor(res_sub$ww_rank, res_sub$true_ww_rank, method  = "kendall" , use="pairwise"), digits=2))

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
    print(out_histogram)
    
    
    # add parameters 
    out_histogram2 <- grid.arrange(out_histogram, out_sum_stats_tbl,
                                 layout_matrix = rbind(c(1, 1, 1),
                                                       c(1, 1, 1),
                                                       c(1, 1, 1),
                                                       c(2, 2, 2)))
    
    # save plot  
    ggsave(filename = paste0(out_plot,"hist_run_",  run_id_i, ".png"), 
           plot     = out_histogram2, 
           width    = 9, 
           height   = 5)
    
}# close for loop 




#============================#
# ==== stress test plots ====
#============================#

# I couldn't think of a smart way to do this so I just wrote it all out :( 

# get the kendal correlations for each run 
cor_tab <- res_dt[, list(standard_cor = cor(standard_rank, true_ww_rank, method  = "kendall" , use="pairwise"),
                         ww_cor       =  cor(ww_rank, true_ww_rank, method  = "kendall" , use="pairwise"),
                         mean_sd_standard  = mean(sd_standard, na.rm = TRUE),
                         mean_sd_ww        = mean(sd_ww, na.rm= TRUE)),
                  run_id]

  #==========================#
  # ==== max diff stress ====
  #==========================#

    #==================#
    # ==== np_hack ====
    #==================#
    
    xwalk_sub <- model_xwalk[method == "np_hack" & ta_sd == 0.1  & stud_sorting == 0 & n_stud_per_teacher == "150"]

    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
    
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "max_diff")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = max_diff, y = ww_cor)) +
      geom_point(aes(color = "Weighted Nonparametric VA", y = ww_cor), size = 3) + 
      geom_line(aes(color = "Weighted Nonparametric VA", y = ww_cor), size = 1) + 
      geom_point(aes( y = standard_cor,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = standard_cor,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Teacher Heterogeneity") +
      ylab("Rank Correlation to Truth") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_np_max_diff", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    

    #==============#
    # ==== bin ====
    #==============#
    
    
    xwalk_sub <- model_xwalk[method == "bin" & ta_sd == 0.1  & stud_sorting == 0 & n_stud_per_teacher == "150"]
    
    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
    
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "max_diff")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = max_diff, y = ww_cor)) +
      geom_point(aes(color = "Weighted Binned VA", y = ww_cor), size = 3) + 
      geom_line(aes(color = "Weighted Binned VA", y = ww_cor), size = 1) + 
      geom_point(aes( y = standard_cor,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = standard_cor,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Teacher Heterogeneity") +
      ylab("Rank Correlation to Truth") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_bin_max_diff", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    

  #=========================#
  # ==== student stress ====
  #=========================#
    #==================#
    # ==== np_hack ====
    #==================#
    
    xwalk_sub <- model_xwalk[method == "np_hack" & ta_sd == 0.1 & max_diff ==1  & stud_sorting == 0]
    
    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
    
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "n_stud_per_teacher")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = n_stud_per_teacher, y = mean_sd_ww)) +
      geom_point(aes(color = "Weighted Nonparametric VA", y = mean_sd_ww), size = 3) + 
      geom_line(aes(color = "Weighted Nonparametric VA", y = mean_sd_ww), size = 1) + 
      geom_point(aes( y = mean_sd_standard,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = mean_sd_standard,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Number of Students") +
      ylab("Mean Standard Deviation") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_np_n_stud", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    
    
    #==================#
    # ==== bin ====
    #==================#
    
    xwalk_sub <- model_xwalk[method == "bin" & ta_sd == 0.1 & max_diff ==1  & stud_sorting == 0]
    
    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
    
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "n_stud_per_teacher")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = n_stud_per_teacher, y = mean_sd_ww)) +
      geom_point(aes(color = "Weighted Binned VA", y = mean_sd_ww), size = 3) + 
      geom_line(aes(color = "Weighted Binned VA", y = mean_sd_ww), size = 1) + 
      geom_point(aes( y = mean_sd_standard,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = mean_sd_standard,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Number of Students") +
      ylab("Mean Standard Deviation") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_bin_n_stud", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    

  #=================================#
  # ==== Teacher ability stress ====
  #=================================#

    #==================#
    # ==== np_hack ====
    #==================#

    xwalk_sub <- model_xwalk[method == "np_hack" & stud_sorting == 0 & n_stud_per_teacher == 150 & max_diff ==1]
  
    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
  
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "ta_sd")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = ta_sd, y = ww_cor)) +
      geom_point(aes(color = "Weighted Nonparametric VA", y = ww_cor), size = 3) + 
      geom_line(aes(color = "Weighted Nonparametric VA", y = ww_cor), size = 1) + 
      geom_point(aes( y = standard_cor,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = standard_cor,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Teacher Ability Variance") +
      ylab("Rank Correlation to Truth") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_np_ta_sd", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    
    #===============#
    # ==== bins ====
    #===============#

    xwalk_sub <- model_xwalk[method == "bin" & stud_sorting == 0 & n_stud_per_teacher == 150 & max_diff ==1]
    
    # subset to the runs we need 
    cor_tab_sub <- cor_tab[run_id %in% xwalk_sub$run_id]
    
    # merge on the thing that changes 
    cor_tab_sub <- merge(cor_tab_sub, xwalk_sub[, c("run_id", "ta_sd")], "run_id" )
    
    # make the plot 
    stress_plot <- ggplot(cor_tab_sub, aes(x = ta_sd, y = ww_cor)) +
      geom_point(aes(color = "Weighted Binned VA", y = ww_cor), size = 3) + 
      geom_line(aes(color = "Weighted Binned VA", y = ww_cor), size = 1) + 
      geom_point(aes( y = standard_cor,  color = "Standard VA"),size = 3) +
      geom_line(aes( y = standard_cor,  color = "Standard VA"),size = 1) +
      scale_color_manual(values= c("#77AADD", "#ffaabb"),
                         guide=guide_legend(reverse=TRUE)) +
      xlab("Teacher Ability Variance") +
      ylab("Rank Correlation to Truth") +
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
    print(stress_plot)
    
    ggsave(filename = paste0(out_plot,"stress_bin_ta_sd", ".png"), 
           plot     = stress_plot, 
           width    = 9, 
           height   = 5)
    

  