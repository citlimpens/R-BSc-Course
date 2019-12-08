v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,5,6)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,4,6)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
nom <- factor(c(rep("A", 10), rep("B", 8)))
nom
m1 <- cbind (v1,v2,v3,v4,v5,v6)
cor(m1)
mod1 <- fractanal(m1, factors = 3, scores="regression")
str(mod1)
mod1$loadings

S1 <- mod1$scores[,1]
S2 <- mod1$scores[,2]
plot(S1, S2, pch=20, col = c("S", "3")[nom])
abline(h=0)
abline(v=0)