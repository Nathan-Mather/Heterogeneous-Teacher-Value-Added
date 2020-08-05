#=================#
# ==== set up ====
#=================#
# tbh I hvae no idea what this is doing but I copied it from Nate's code

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")
#install.packages("np")

# check users (can do something here eventually to automatically pick a user)

# load packages and our functions 
library(data.table)
library(broom)
#source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/functions/simulate_test_data.R")  #set this path
# source("~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/functions/simulate_test_data.R")
library(Matrix)
library(ggplot2)
library(np) # non parametric library
library(quantreg)


#=============================
#    Semi/non parametric 
#============================



# === generate simulated data for one teacher over ~7 years (with high dropoff for visualization ) ===
r_dt <- simulate_test_data(n_teacher               = 1,
                           n_stud_per_teacher      = 200,
                           test_SEM                = .07,
                           teacher_va_epsilon      = .1,
                           teacher_ability_drop_off = .5)

# Call teacher center to put on plot
tc = unique(r_dt[,teacher_center])

# Estimate relationship of y and y(t-1) currently dropping 5% of students with extreme outcomes to make graphs clearer
m <- npreg(test_2~test_1, data = r_dt[abs(test_1)<2])

#m <- npindex(test_2~test_1+ ... , data = r_dt[abs(test_1)<2])


# Plot
plot(m, plot.errors.method="asymptotic",
     plot.errors.style="band", ylim=c(-2, 2), xlim=c(-2, 2) )

points(r_dt[ ,test_1], r_dt[ ,test_2], cex=.25)

# Draw a line of (roughly) what we expect
abline(-.25,1)
#abline(v = tc )


# This looks about right. Teachers have positive-ish VA at the center and it slopes away 
# The transformation is weird. Maybe we should run it on the residual of test_2.
# ... Once we add in covariates the semiparametirc index model will be great.






#=============================
#   quantile regression
#============================

n=20

# === generate simulated data over ~7 years ===
r_dt <- simulate_test_data(n_teacher               = 20,
                           n_stud_per_teacher      = 200,
                           test_SEM                = .07,
                           teacher_va_epsilon      = .1,
                           teacher_ability_drop_off = .5)




# convert teacher_id to a factor so we can treat it as a dummy 
r_dt[, teacher_id := as.factor(teacher_id)]

#Set desired percentiles
ptle = seq(.01,.99,by=.02)

# Run quantie regressions
rqfit <- rq(test_2 ~ test_1 + teacher_id -1, data = r_dt, tau = ptle)


# clean results for teachers
coefs <- rqfit[["coefficients"]]
coefs <- coefs[1:n+1,]

# Standardize coefs (should I do this? If I don't things look really wrong...)
std_coefs <- matrix(0,n,length(ptle))
for (x in seq(1,length(ptle),by=1))
{
std_coefs[,x] = (coefs[,x]-mean(coefs[,x]))/sd(coefs[,x])
}

# Graph it for teacher 1
out <- data.frame(std_coefs[1,],ptle)

diog_plot1 <- ggplot(data = out, aes(x= ptle, y = std_coefs.1...)) +
  geom_point()  +
  ylab("Standardized Effect") + xlab("Percentile")
print(diog_plot1)





