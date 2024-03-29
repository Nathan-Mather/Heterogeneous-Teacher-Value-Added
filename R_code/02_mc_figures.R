# =========================================================================== #
# ========================== Monte Carlo Figures ============================ #
# =========================================================================== #
# - Purpose of code:
#  - Make a few figures for the Monte Carlo results.

# Clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

# No scientific notation. 
options(scipen = 999)

# Clean console history.
cat("\f")

# Set seed. 
set.seed(42)

# Define a notin function.
`%notin%` <- Negate(`%in%`)




# =========================================================================== #
# ========================= File paths and packages ========================= #
# =========================================================================== #

# Load packages. 
library(data.table)
library(ggplot2)
library(gridExtra)

# Check users to set directory.
#  (NOTE TO MIKE, add something unique to your base working directory to detect
#   when it is your computer)
my_wd <- getwd()

if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  in_data <- "C:/Users/Nmath_000/Documents/Research/Value_added_local/results/"
  
  # path for plots
  out_plot <- "C:/Users/Nmath_000/Documents/Research/Value_added_local/results/figures/"
  
}else{
  # base directory 
  base_path <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  in_data <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/"
  
  # path for plots
  out_plot <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/Figures/"
}

# Set plot attributes.
plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 20),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =25),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =25),
        legend.title = element_text(size=10))




# =========================================================================== #
# ========================= Load results and xwalk ========================== #
# =========================================================================== #

# Load the results.
res_dt <- fread(paste0(in_data, 'results.csv'))

# Load model_xwalk.
model_xwalk <- fread(paste0(in_data,
                            "/xwalk.csv"))

# # Figure out which figures already exist.
# figs <- list.files(path=out_plot)
# figs <- tail(figs[figs %like% 'truth_cat_run_'], n=1)
# 
# if (length(figs) == 0) {
#   last_num <- 0
# } else {
#   last_num <- as.numeric(gsub('^.*([0-9]+).*$', '\\1', figs))
# }





# =========================================================================== #
# ========================== Normalize everything =========================== #
# =========================================================================== #

# Renormalize truth and standard so they have the same mean and variance.
res_dt[, mean_standard_norm := (mean_standard - mean(mean_standard))/sd(mean_standard), by = run_id]
res_dt[, true_welfare_norm := (true_welfare - mean(true_welfare))/sd(true_welfare), by = run_id]
res_dt[, true_standard_norm := (true_standard - mean(true_standard))/sd(true_standard), by = run_id]

# Renormalize the standard deviations.
res_dt[, sd_standard_norm := sd_standard/sd(mean_standard), by = run_id]

# Renormalize each of the other estimation results.
res_dt[, mean_binned_norm := (mean_bin - mean(mean_bin))/sd(mean_bin), by = run_id]
res_dt[, sd_binned_norm := sd_bin/sd(mean_bin), by = run_id]

res_dt[, mean_np_norm := (mean_np - mean(mean_np))/sd(mean_np), by = run_id]
res_dt[, sd_np_norm := sd_np/sd(mean_np), by = run_id]

res_dt[, mean_quantile_norm := (mean_quantile - mean(mean_quantile))/sd(mean_quantile), by = run_id]
res_dt[, sd_quantile_norm := sd_quantile/sd(mean_quantile), by = run_id]




# =========================================================================== #
# =============================== Get ranks ================================= #
# =========================================================================== #

# Sort standard, get the ranks.
setorder(res_dt, mean_standard_norm)
res_dt[, standard_rank := 1:.N, run_id]

# Get confidence interval for the standard.
res_dt[, standard_lc := mean_standard_norm - 1.96*sd_standard_norm]
res_dt[, standard_uc := mean_standard_norm + 1.96*sd_standard_norm]

# Sort binned, get the ranks.
setorder(res_dt, mean_binned_norm)
res_dt[, binned_rank := 1:.N, run_id]

# Get confidence interval for the binned estimate.
res_dt[, binned_lc := mean_binned_norm - 1.96*sd_binned_norm]
res_dt[, binned_uc := mean_binned_norm + 1.96*sd_binned_norm]

# Sort np, get the ranks.
setorder(res_dt, mean_np_norm)
res_dt[, np_rank := 1:.N, run_id]

# Get confidence interval for the np estimate.
res_dt[, np_lc := mean_np_norm - 1.96*sd_np_norm]
res_dt[, np_uc := mean_np_norm + 1.96*sd_np_norm]

# Sort quantile, get the ranks.
setorder(res_dt, mean_quantile_norm)
res_dt[, quantile_rank := 1:.N, run_id]

# Get confidence interval for the quantile estimate.
res_dt[, quantile_lc := mean_quantile_norm - 1.96*sd_quantile_norm]
res_dt[, quantile_uc := mean_quantile_norm + 1.96*sd_quantile_norm]

# Sort truth, get the ranks.
setorder(res_dt, true_welfare_norm)
res_dt[, true_ww_rank :=1:.N, run_id]

# Sort teacher center, get the ranks.
setorder(res_dt, teacher_center)
res_dt[, cent :=1:.N, run_id]




# =========================================================================== #
# ========================== Get distance measures ========================== #
# =========================================================================== #

# Calculate the mean squared distance from the rank of the truth.
res_dt[, standard_MSE := (standard_rank - true_ww_rank)^2]
res_dt[, standard_MAE := abs(standard_rank - true_ww_rank)]

res_dt[, binned_MSE := (binned_rank - true_ww_rank)^2]
res_dt[, binned_MAE := abs(binned_rank - true_ww_rank)]

res_dt[, np_MSE := (np_rank - true_ww_rank)^2]
res_dt[, np_MAE := abs(np_rank - true_ww_rank)]

res_dt[, quantile_MSE := (quantile_rank - true_ww_rank)^2]
res_dt[, quantile_MAE := abs(quantile_rank - true_ww_rank)]




# =========================================================================== #
# =========================== Plots for every run =========================== #
# =========================================================================== #

last_num <- 0
single_run_i <- 0

# Loop over rows in the xwalk.
for (i in 1:nrow(model_xwalk)) {
print(i)
  # Grab run_id. 
  run_id_i <- model_xwalk[i, run_id]
  # single_run_i <- model_xwalk[i, single_run]
  # 
  if ((as.numeric(run_id_i) <= last_num) | (as.numeric(single_run_i) == 1)) {
    next()
  }

  # subset data to this mc run 
  res_sub <- res_dt[run_id == run_id_i]
  
  # Get table of run parameters.
  parms_tab <- melt.data.table(model_xwalk[i], measure.vars = colnames(model_xwalk))
  parms_tab <- parms_tab[!is.na(value)]
  parms_tab <- parms_tab[variable != "run_id"]
  parms_tab <- parms_tab[variable != "single_run"]
  
  # Put parameters in grob.
  parms_tbl <- tableGrob(parms_tab, rows=NULL, cols = NULL, theme = ttheme_default(base_size = 9))
  
  
  # ========================================================================= #
  # ============================= Standard plots ============================ #
  # ========================================================================= #
  
  # Just the truth.
  # truth_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = true_welfare_norm)) +
  #   geom_point(size = 1.5, aes(color = "Welfare-Weighted VA"), alpha = 1) + 
  #   scale_color_manual(values= c("#77AADD")) +
  #   ylab("Teacher Impact") + 
  #   xlab("Teacher Impact Rank Order (Low to High)") +
  #   ylim(-6,6)+
  #   plot_attributes + 
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0.8, 0.8))
  
  # Standard caterpillar plot.
  standard_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_standard_norm)) +
    geom_point(size = .85, aes(color = "Standard VA Estimate"), alpha = .8) + 
    geom_point(aes( y = true_welfare_norm,  color = "Welfare-Weighted VA"),size = .85, alpha = 1) +
    scale_color_manual(values= c("#ffaabb", "#77AADD")) +
    geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width= 1, color = "#ffaabb", alpha = .3) +
    ylab("Teacher Impact") + 
    xlab("Teacher Impact Rank Order (Low to High)") +
    ylim(-6,6)+
    plot_attributes + 
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.8))
  
  # # Standard teacher center.
  # standard_est_center_plot <- ggplot(res_sub, aes(x = teacher_center, y = mean_standard_norm)) +
  #   geom_point(size = 1.5, aes(color = "Standard VA Estimate"), alpha = 1) +
  #   geom_point(aes( y = true_welfare_norm, color = "Welfare-Weighted VA"),size = 1, alpha = .4) +
  #   scale_color_manual(values= c("#ffaabb", "#77AADD")) +
  #   ylab("Teacher Impact") +
  #   xlab("Optimal Student Ability Match") +
  #   ylim(-6,6)+
  #   plot_attributes +
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0.8, 0.8))
  # 
  # # the point of this is to see any correlation of teacher center and overall techer ability 
  # standard_center_plot <- ggplot(res_sub, aes(x = teacher_center, y = true_standard_norm)) +
  #   geom_point(size = 1.5, aes(color = "Standard VA"), alpha = 1) +
  #   geom_point(aes( y = true_welfare_norm, color = "Welfare-Weighted VA"),size = 1, alpha = .4) +
  #   scale_color_manual(values= c("#ffaabb", "#77AADD")) +
  #   ylab("Teacher Impact") +
  #   xlab("Optimal Student Ability Match") +
  #   ylim(-6,6)+
  #   plot_attributes +
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0.8, 0.8))
  
  # standard_center_plot1 <- ggplot(res_sub, aes(x = teacher_center, y = true_standard_norm)) +
  #   geom_point(size = 1.5, aes(color = "Standard VA"), alpha = 1) +
  #   scale_color_manual(values= c("#ffaabb", "#77AADD")) +
  #   ylab("Teacher Impact") +
  #   xlab("Optimal Student Ability Match") +
  #   ylim(-6,6)+
  #   plot_attributes +
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0.8, 0.8))
  
  # Save the plots.
  # ggsave(filename = paste0(out_plot, "truth_cat_run_",  run_id_i, ".png"), 
  #        plot     = truth_cat_plot, 
  #        width    = 9, 
  #        height   = 4)
  
  ggsave(filename = paste0(out_plot, "standard_cat_run_",  run_id_i, ".png"), 
         plot     = standard_cat_plot, 
         width    = 9, 
         height   = 4)
  
  # ggsave(filename = paste0(out_plot, "standard_est_cent_run_",  run_id_i, ".png"), 
  #        plot     = standard_est_center_plot, 
  #        width    = 9, 
  #        height   = 4)
  
  # ggsave(filename = paste0(out_plot, "standard_cent_run_",  run_id_i, ".png"), 
  #        plot     = standard_center_plot, 
  #        width    = 9, 
  #        height   = 4)
  
  # ggsave(filename = paste0(out_plot, "standard_cent_run_just_stand_",  run_id_i, ".png"),
  #        plot     = standard_center_plot1, 
  #        width    = 9, 
  #        height   = 4)

  dir.create(paste0(out_plot, "interactive_data/"))
  
  # # save rdata versions 
  # save(truth_cat_plot,
  #      file = paste0(out_plot, "interactive_data/", "truth_cat_run_",  run_id_i, ".Rdata"))
  # 
  save(standard_cat_plot,
       file = paste0(out_plot, "interactive_data/", "standard_cat_run_",  run_id_i, ".Rdata"))
  # 
  # save(standard_est_center_plot,
  #      file = paste0(out_plot, "interactive_data/", "standard_est_cent_run_",  run_id_i, ".Rdata"))
  # 
  # save(standard_center_plot,
  #      file = paste0(out_plot, "interactive_data/", "standard_cent_run_",  run_id_i, ".Rdata"))

  # ========================================================================= #
  # ============================ Loop over methods ========================== #
  # ========================================================================= #
  
  # Loop over the methods.
  for (method in c('binned', 'np', 'quantile')) {
    
    # Skip if we didn't use the particular method in our set of results.
    if (sum(is.na(res_sub[, get(paste0('mean_', method, '_norm'))])) > 0) {
      next
    } else {
      # Make generic columns to fit former code.
      res_sub[, mean_ww_norm := get(paste0('mean_', method, '_norm'))]
      res_sub[, sd_ww_norm := get(paste0('sd_', method, '_norm'))]
      res_sub[, ww_rank := get(paste0(method, '_rank'))]
      res_sub[, ww_lc := get(paste0(method, '_lc'))]
      res_sub[, ww_uc := get(paste0(method, '_uc'))]
      res_sub[, ww_MSE := get(paste0(method, '_MSE'))]
      res_sub[, ww_MAE := get(paste0(method, '_MAE'))]
    }


    # ========================================================================= #
    # ========================= Teacher Center Figures ======================== #
    # ========================================================================= #
    
    # # Alternative teacher center.
    # welfare_center_plot <- ggplot(res_sub, aes(x = teacher_center, y = mean_ww_norm)) +
    #   geom_point(size = 1.5, aes(color = "Alternative VA Estimate"), alpha = 1) +
    #   geom_point(aes( y = true_welfare_norm, color = "Welfare-Weighted VA"),size = 1, alpha = .4) +
    #   scale_color_manual(values= c("#ffaabb", "#77AADD"),
    #   guide=guide_legend(reverse=TRUE)) +
    #   ylab("Teacher Impact") +
    #   xlab("Optimal Student Ability Match") +
    #   ylim(-6,6)+
    #   plot_attributes +
    #   theme(legend.title = element_blank(),
    #         legend.position = c(0.8, 0.8))

# 
#     # Save the figures.
#     ggsave(filename = paste0(out_plot, method, "_welfare_cent_run_",  run_id_i, ".png"), 
#            plot     = welfare_center_plot, 
#            width    = 9, 
#            height   = 4)  
#   
#     # save rdata version for shiny app 
#     save(welfare_center_plot,
#          file = paste0(out_plot,  "interactive_data/", method, "_welfare_cent_run_",  run_id_i, ".Rdata"))
#     
    # ========================================================================= #
    # ========================= Alternative Caterpillar ======================= #
    # ========================================================================= #
    
    # Alternative caterpillar plot.
    ww_cat_plot <- ggplot(res_sub, aes(x = true_ww_rank, y = mean_ww_norm)) +
      geom_point(aes(color = "Alternative VA Estimate"), size = .85,  alpha = 1) + 
      geom_point(aes( y = true_welfare_norm,  color = "Welfare-Weighted VA"),size = .85, alpha = .4) +
      scale_color_manual(values= c("#ffaabb", "#77AADD"),
                         guide=guide_legend(reverse=TRUE)) +
      geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width= 1, color = "#ffaabb", alpha = .1) +
      ylab("Teacher Impact") + 
      xlab("Teacher Impact Rank Order (Low to High)") +
      ylim(-6,6)+
      plot_attributes +
      theme(legend.title = element_blank(),
            legend.position = c(0.2, 0.8))
  
    # Save the plot. 
    ggsave(filename = paste0(out_plot, method, "_ww_cat_run_",  run_id_i, ".png"), 
           plot     = ww_cat_plot, 
           width    = 9, 
           height   = 4)
  
    # save rdata version for shiny app 
    save(ww_cat_plot,
         file = paste0(out_plot,  "interactive_data/", method, "_ww_cat_run_",  run_id_i, ".Rdata"))
    
    # ========================================================================= #
    # ======================= Histogram and Summary Stats ===================== #
    # ========================================================================= #
    
    # Start a data.table of results.
    sum_stats <- list()
    # sum_stats[[1]] <- data.table(Statistic = "Mean Squared Distance", 
    #                              Standard = round(mean(res_sub$standard_MSE), digits=2),
    #                              Weighted = round(mean(res_sub$ww_MSE), digits=2))
    
    sum_stats[[1]] <- data.table(Statistic = "Correlation to Welfare-Weighted VA Rank", 
                                 Standard =  round(cor(res_sub$standard_rank, res_sub$true_ww_rank, method="kendall" , use="pairwise"), digits=2),
                                 Weighted = round(cor(res_sub$ww_rank, res_sub$true_ww_rank, method="kendall" , use="pairwise"), digits=2))
    
    sum_stats[[2]] <- data.table(Statistic = "Mean Absolute Rank Inversion", 
                                 Standard =  round(mean(res_sub$standard_MAE), digits=2),
                                 Weighted =  round(mean(res_sub$ww_MAE), digits=2))
    
    

    out_sum_stats <- rbindlist(sum_stats)
    
    # Put sum stats in a grob.
    out_sum_stats_tbl <- tableGrob(out_sum_stats, rows=NULL, theme = ttheme_default(base_size = 8))
  
    # Set bin width parameter.
    b_width <- 20
    
    # Make the histogram.
    out_histogram <- ggplot(res_sub) + 
      geom_histogram( aes(standard_MAE, fill = "Standard VA Estimate"), alpha = .4, colour="black", binwidth = b_width) +
      geom_histogram( aes(ww_MAE, fill = "Alternative VA Estimate"), alpha = .4, colour="black", binwidth = b_width) +
      ylab("Number of Teachers") +
      xlab("Difference in Rank From Welfare-Weighted VA")+
      scale_fill_manual(values= c("#77AADD", "#EE8866")) +
      plot_attributes + 
      theme(legend.title = element_blank(),
            legend.position = c(0.8, 0.8),
            legend.key.size = unit(.5, "cm"))
  
    # Add parameters.
    out_histogram2 <- grid.arrange(out_histogram, out_sum_stats_tbl,
                                 layout_matrix = rbind(c(1, 1, 1),
                                                       c(1, 1, 1),
                                                       c(1, 1, 1),
                                                       c(2, 2, 2)))
    
    # Save the plot.
    ggsave(filename = paste0(out_plot, method, "_hist_run_",  run_id_i, ".png"), 
           plot     = out_histogram2, 
           width    = 9, 
           height   = 5)
    
    save(out_histogram2,
         file = paste0(out_plot,  "interactive_data/",  method, "_hist_run_",  run_id_i, ".Rdata"))
    
    
  } # Close inner for loop.
} # Close outer for loop.


