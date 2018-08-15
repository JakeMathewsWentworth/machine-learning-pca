x1 <- c(1, 2, 1)
x2 <- c(4, 2, 13)
x3 <- c(7, 8, 1)
x4 <- c(8, 4, 5)

x <- rbind(x1, x2, x3, x4)
data1 <- data.frame(x)
colnames(data1) <- c("a1", "a2", "a3")
rownames(data1) <- c("1", "2", "3", "4")

#print(data1)

dA <- data.matrix(data1)
m <-ncol(dA)
me <- numeric()

for (i in 1:m) {
  me[i] <- mean(dA[,i])
}
A <- matrix(dA, nrow = 4, ncol = 3)
At <- t(A)

Id <- matrix(c(me[1], me[2], me[3]), nrow = 3, ncol = 4)

#print(Id)
mem <- At - Id
#print(mem)
N <- ncol(At)
covmatr <- (1 / (N - 1)) * (mem %*% t(mem))

#print(covmatr)

S = matrix(c(
  2382.78, 2611.84, 2136.20, 
  2611.84, 3106.47, 2553.90,
  2136.20, 2553.90, 2650.71),
  nrow = 3, ncol = 3)
#print(S)

pcarailroad <- eigen(S)
#print(pcarailroad)

lambda <- pcarailroad$values
#print(paste(c("eigenvalues", lambda)))
evec <- pcarailroad$vectors
#print(paste(c("eigenvectors", evec)))

perclambda <- ((pcarailroad$values)/sum(pcarailroad$values))*100
#print(perclambda)

data("heptathlon", package = "HSAUR")
#print(heptathlon)
# Lets make all events in the same direction so that
# large values are good
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

correl <- round(cor(heptathlon[, -8]), 2)
#print(correl)

plot(heptathlon[, -8])
heptathlon_pca <- prcomp(heptathlon[, -8], scale = T)
print(summary(heptathlon_pca))
pc1 <- heptathlon_pca$rotation[, 1]
print(pc1)

center <- heptathlon_pca$center
scale <- heptathlon_pca$scale
pred <- predict(heptathlon_pca)[, 1]
print(pred)
cors



