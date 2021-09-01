#====================================#
# ==== Quality control functions ====
#====================================#

teacher_xwalk_qc <- function(in_teacher_ability_xwalk  = NULL,
                               run_id                    = NULL,
                               out_path                  = NULL){
  
  in_teacher_ability_xwalk <- copy(in_teacher_ability_xwalk)
  
  # get counts for schools, teachers, students
  # mean studs per teacher and teachers per school in a cohort 
  count_dt <- in_teacher_ability_xwalk[, list(n_school = as.numeric(length(unique(school_id))),
                                              n_teacher = as.numeric(length(unique(teacher_id))),
                                              n_stud_p_cohort = as.numeric(sum(n_studs)),
                                              n_grades        = as.numeric(length(unique(grade))),
                                              mean_stud_p_teacher = mean(n_studs),
                                              sd_stud_p_teacher   = sd(n_studs)) ]
  # get mean teachers per school
  teach_per_school <- in_teacher_ability_xwalk[, .N, school_id]
  tps_dt <- teach_per_school[, list(mean_teacher_p_school = mean(N),
                                    sd_teacher_p_school = sd(N))]
  
  # merge counts 
  count_dt <- cbind( count_dt, tps_dt)
  
  # reshape it 
  count_dt <- melt.data.table(count_dt, measure.vars = colnames(count_dt))
  count_dt[, value := as.character(round(value, 2))]
  

  # mean and sd of teacher ability, center, and max 
  teacher_ability_dt <- in_teacher_ability_xwalk[, list(mean_ability = mean(teacher_ability),
                                                        sd_ability   = sd(teacher_ability),
                                                        mean_center  = mean(teacher_center),
                                                        sd_center    = sd(teacher_center),
                                                        mean_max     = mean(teacher_max),
                                                        sd_max       = sd(teacher_max),
                                                        max_ability_sd_ratio = sd(teacher_max)/ sd(teacher_ability)),]
  
  # measure of vertial to horizontal teacher ability 
  teacher_ability_dt <- melt.data.table(teacher_ability_dt, measure.vars = colnames(teacher_ability_dt))
  teacher_ability_dt[, value := as.character(round(value, 4))]
  
  # plot attributes 
  plot_attributes <- theme_classic() + 
    theme(text = element_text(size= 20),
          plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =25),
          plot.subtitle = element_text(color = "black",hjust = 0.5, size =25),
          legend.title = element_text(size=20),
          legend.position="right")
  
  # make teacher max bar point 
  in_teacher_ability_xwalk[, bar_lower := teacher_ability-(.5*teacher_max)]
  in_teacher_ability_xwalk[, bar_upper := teacher_ability+(.5*teacher_max)]
  
  # graph teacher ability on y, teacher center on x, teacher max as error bar 
  teacher_ability_plot <- ggplot(in_teacher_ability_xwalk, aes(x = teacher_center, y = teacher_ability)) + 
    geom_point(size = 1) + 
    geom_linerange(aes(ymin=bar_lower, ymax=bar_upper, color = "teacher_max"), alpha = 1, size = .5) +
    scale_color_manual(name = "Legend: ", values = c("#77AADD")) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid"),
                                                     shape = NA))) + 
    ggtitle("Teacher Ability Distribution") + 
    plot_attributes
  
  # save everything 
  full_out_path <- paste0(out_path, "run_", run_id, "/teacher_xwalk_qc/")
  
  dir.create( paste0(full_out_path, "/interactive_data/"),
              recursive = TRUE)
  
  write.csv(count_dt,
            paste0(full_out_path, "teacher_xwalk_counts_", run_id,".csv"),
                   row.names = FALSE)
  
  write.csv(teacher_ability_dt,
            paste0(full_out_path, "teacher_ability_tab_", run_id, ".csv"),
            row.names = FALSE)
  
  # Save the plots as png to view and ggplot for interactive tool 
  ggsave(filename =paste0(full_out_path, "teacher_ability_dist_", run_id, ".png"), 
         plot     = teacher_ability_plot, 
         width    = 9, 
         height   = 4)
  
  save(teacher_ability_plot,
       file = paste0(full_out_path, "/interactive_data/", "teacher_ability_dist_", run_id, ".Rdata"))
  
  
  return("results saved")
  
}
