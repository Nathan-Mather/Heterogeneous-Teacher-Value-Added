#===============================#
# ==== concordant pair loss ====
#===============================#

# examples 
t_dt <- data.table(id = c(1:100), value = runif(100))
t_dt[, rank := frank(value)]

s_dt <- data.table(id_s = c(1:100), value_s = runif(100))
s_dt[, rank_s := frank(value_s)]


# merge these together 
conc_dt <- merge(t_dt, s_dt, by.x = "id", by.y = "id_s")

start_t <- Sys.time()
conc_dt[, merge := 1]

# make a data.table of all possible combinations 
conc_dt <- merge(conc_dt, conc_dt, by = "merge", allow.cartesian = TRUE)
conc_dt <- conc_dt[id.x != id.y]

# say if it is higher or lower
conc_dt[, concordant := as.numeric((rank.x > rank.y) == (rank_s.x > rank_s.y))*2-1 ]
conc_dt[, mean(concordant)]
end_t <- Sys.time()
end_t-start_t

# test 
start_t2 <- Sys.time()
cor(t_dt$value, s_dt$value_s, method="kendall", use="pairwise")
end_t2 <- Sys.time()
end_t2-start_t2

