
# try a test run with three bins 
test_grid <- expand.grid(c(0:10), c(5), c(0:10))


test_grid <- data.table(test_grid)
colnames(test_grid) <- c("L", "M", "H")



# so now we have a grid of possible weights for the three bins 

# now suppose I observe the following changes (those changes must be anet loss b/c they didn't do them )
# L*10 +m*5 -h*7 < 0
# we just eliminate anything that violates this from the grid 

res_grid <- test_grid[10*L+5*M-7*H <0]


# okay this makes a lot of sense to me, lets try a real example 
# generate fake teachers 
n_teach <- 5
teachers <- data.table(va_l = runif(n_teach), va_m = runif(n_teach), va_h = runif(n_teach))



# generate fake students Fir eas just use uniform for not 
studs <- data.table(stud_pre = runif(100, 0, 3), classroom = 1:5)

# figure out welfare for each possible teacher classrom pair

# allocate to highest than

# allocate students according to the fake welfare welfre function 

# back out welfre function from grid 
