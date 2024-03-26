
# Load Required Package

library("nloptr")

# Load Functions

source("functions.R")

# Load Example Data

load("data_ii.rda")
#load("data_viii.rda")

# The Number of Transactions

a0 <- sum(data$customer_loyalty)
a1 <- sum(!data$customer_loyalty)

# True (Observed) Number of Customers with the Loyalty Card

d0_true <- length(unique(data$customer_id[data$customer_loyalty]))
d0_true

# True (Unobserved) Number of Customers Without the Loyalty Card

d1_true <- length(unique(data$customer_id[!data$customer_loyalty]))
d1_true

# Keep Only Observable Values in the Dataset

data$customer_id[!data$customer_loyalty] <- NA
data$customer_segment[!data$customer_loyalty] <- NA

# The Numbers of Basket Types and Customer Segments

x0 <- table(data$basket_type[!is.na(data$customer_id)])
x0

y0 <- table(data$customer_segment[!is.na(data$customer_id)])
y0

n <- length(x0)
m <- length(y0)

z0 <- sapply(1:m, function(j) { table(data$basket_type[!is.na(data$customer_id) & data$customer_segment == paste0("c", j)]) })
colnames(z0) <- paste0("c", 1:m)
z0

x1 <- table(data$basket_type[is.na(data$customer_id)])
x1

# Probabilities

q0 <- y0 / sum(y0)
q0

r0 <- sapply(1:m, function(j) { z0[, j] / y0[j] })
colnames(r0) <- paste0("c", 1:m)
r0

f0 <- sapply(1:m, function(j) { mean(table(data$customer_id[data$customer_segment == paste0("c", j)])) })
names(f0) <- paste0("c", 1:m)
f0

# Proposed Estimate

opti = list(algorithm = 'NLOPT_GN_ISRES', maxeval = 1e5, xtol_rel = 0)
results <- customerDist(x = x1, r = r0, f = f0, init = y0, method = 'ML', opti = opti)

d1_proposed <- results$d
d1_proposed

d1_proposed - d1_true

# Naive Estimate

d1_naive <- d0_true / a0 * a1
d1_naive

d1_naive - d1_true
