#==============================#
# ==== residual experiment ====
#==============================#


# load packages 
library(data.table)
library(arm)

rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")
#========================#
# ==== generate data ====
#========================#


# so I need to generate teacher and student matches and data where a students ability is 
# completely dsecribed by their pretest. 


# start data set with teacher ID's and abilities 
my_dt <- data.table(teacher_id = rep(1:100, 100), t_ability = rep(runif(100), 100))


# make teacher ID a factotr for eventual regression 
my_dt[, teacher_id := as.factor(teacher_id)]

# now generate students that have ability correlated with the teachers 
my_dt[, student_id := 1:nrow(my_dt)]

my_dt[, s_ability1 := runif(nrow(my_dt)) + t_ability*.3]

# check correlation 
cor(my_dt$t_ability, my_dt$s_ability1)


# add student ability after teacher's effect 
my_dt[, s_ability2 := s_ability1 + t_ability]


# now add in some noise so the residuals are not all zero 
my_dt[, s_test2 := s_ability2 + runif(nrow(my_dt),0, .6)]


# Now run the value added regression so we can check the residuals 
reg_res <- lm(s_test2 ~  s_ability1 + teacher_id - 1, data = my_dt)


# now check the residuals 
my_dt[, resids := reg_res$residuals]


# now plot residuals over ability 
plot(my_dt$s_ability1, my_dt$resids)

binnedplot(my_dt$s_ability1 ,my_dt$resids)


###############################
# Now check measurement error #
###############################

# suppose the pretest is measured with error 
my_dt[, s_test1 := s_ability1 +  runif(nrow(my_dt),0, 1)]

# Now run the value added regression and see that it is perfectly identified 
reg_res2 <- lm(s_test2 ~  s_test1 + teacher_id - 1, data = my_dt)


# now check the residuals 
my_dt[, resids2 := reg_res2$residuals]


# now plot residuals over ability 
plot(my_dt$s_ability1, my_dt$resids2)

binnedplot(my_dt$s_ability1 ,my_dt$resids2)

# plot residuals over pretest 
binnedplot(my_dt$s_test1 ,my_dt$resids2)



############################
# without teacher dummies #
###########################

  
  # see if my intution is right about the residuals sans teachers 
  reg_res4 <- lm(s_test2 ~  s_ability1, data = my_dt)
  
  
  # now check the residuals 
  my_dt[, resids4 := reg_res4$residuals]
  
  
  # now plot residuals over ability 
  plot(my_dt$s_ability1, my_dt$resids4)
  
  binnedplot(my_dt$s_ability1 ,my_dt$resids4)

  
#=========================================#
# ==== nonlinear pretest relationship ====
#=========================================#

  # start data set with teacher ID's and abilities 
  my_dt <- data.table(teacher_id = rep(1:100, 100), 
                      t_ability = rep(rnorm(100), 100),
                      t_center = rep(rnorm(100), 100))
  
  
  # make teacher ID a factotr for eventual regression 
  my_dt[, teacher_id := as.factor(teacher_id)]
  
  # now generate students 
  my_dt[, student_id := 1:nrow(my_dt)]
  
  my_dt[, s_ability1 := rnorm(nrow(my_dt))]
  
  # map out what the funciton I am using will do for a given teacher ability 
  pretests <- rnorm(nrow(my_dt))
  t_ability <- 1
  postests <- pretests + (1/(1+exp(1)^(-1*pretests)))*2  + (t_ability/2)
  # postests <- pretests +  (1/(1+exp(1)^(-1*pretests)))*(t_ability/.5)
  # postests <- pretests + t_ability
 
  plot(pretests, postests)
  
  # add student ability after teacher's effect, But do it nonlinearly 
  # my_dt[, s_ability2 := s_ability1 + (1/(1+exp(1)^(-1*s_ability1)))*(t_ability/.5)]
  my_dt[, s_ability2 := s_ability1 + (1/(1+exp(1)^(-1*s_ability1)))*2  + t_ability/2]
  plot(my_dt$s_ability1, my_dt$s_ability2)
  
  # now add in some noise so the residuals are not all zero 
  my_dt[, s_test2 := s_ability2 + runif(nrow(my_dt),0, .6)]
  
  
  # Now run the value added regression so we can check the residuals 
  reg_res <- lm(s_test2 ~  s_ability1 + teacher_id - 1, data = my_dt)
  
  
  # now check the residuals 
  my_dt[, resids := reg_res$residuals]
  
  # now plot residuals over ability 
  plot(my_dt$s_ability1, my_dt$resids)
  
  binnedplot(my_dt$s_ability1 ,my_dt$resids)

  
  #====================================#
  # ==== add comparative advantage ====
  #====================================#
  
  # Now try it with teacher center   
  my_dt[, teacher_comp_adv := as.numeric(t_center >-.5)]

  k <- 3
  # increasign in pretest  
  my_dt[teacher_comp_adv == 1, s_ability2 := s_ability1 + (1/(1+exp(1)^(-k*s_ability1)))*2  + t_ability/2]
  
  # decreasing in pretest 
  my_dt[teacher_comp_adv == 0, s_ability2 := s_ability1 + (1/(1+exp(1)^(k*s_ability1)))*2  + t_ability/2]
  
  
  # sanity checks 
  # now add in some noise so the residuals are not all zero 
  my_dt[, s_test2 := s_ability2 + runif(nrow(my_dt),0, .6)]
  
  
  # Now run the value added regression so we can check the residuals 
  reg_res <- lm(s_test2 ~  s_ability1 + teacher_id - 1, data = my_dt)
  
  
  # now check the residuals 
  my_dt[, resids := reg_res$residuals]
  
  # now plot residuals over ability 
  plot(my_dt$s_ability1, my_dt$resids)
  
  binnedplot(my_dt$s_ability1 ,my_dt$resids)

  #==========================================#
  # ==== parm for degree of nonlinearity ====
  #==========================================#
  
  # no need to create a new variavle, just use teacher center 

  t_ability <- 1
  t_center <- 1
  postests <- pretests + (1/(1+exp(1)^(-5*pretests)))*t_center  - (1/(1+exp(1)^(-1*0)))*t_center+ t_ability/2
  # postests <- pretests +  (1/(1+exp(1)^(-1*pretests)))*(t_ability/.5)
  # postests <- pretests + t_ability
  
  plot(pretests, postests)
  
  
  # increasign in pretest  
  my_dt[, s_ability2 := s_ability1 + (1/(1+exp(1)^(-1*s_ability1)))*t_center - (1/(1+exp(1)^(-1*0)))*t_center+ t_ability/2]
  
  plot(my_dt$s_ability1, my_dt$s_ability2)
  
  
  # now add in some noise so the residuals are not all zero 
  my_dt[, s_test2 := s_ability2 + runif(nrow(my_dt),0, .6)]
  
  
  # Now run the value added regression so we can check the residuals 
  reg_res <- lm(s_test2 ~  s_ability1 + teacher_id - 1, data = my_dt)
  
  
  # now check the residuals 
  my_dt[, resids := reg_res$residuals]
  
  # now plot residuals over ability 
  plot(my_dt$s_ability1, my_dt$resids)
  
  binnedplot(my_dt$s_ability1 ,my_dt$resids)
  
  
  
  
  
  
  
  