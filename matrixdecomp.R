random.matrix <- matrix(rnorm(30), nrow=10, ncol=3)

qr.decomp <- qr(random.matrix)
qr.R(qr.decomp)
qr.Q(qr.decomp)

sv.decomp <- svd(random.matrix)
sv.decomp$u %*% diag(sv.decomp$d) %*% t(sv.decomp$v)

## What happens if the matrix isn't full rank?
rank2.matrix <- random.matrix
# (make the third column a combination of the first two)
rank2.matrix[,3] <- 0.7 * random.matrix[,1] + 0.3 * random.matrix[,2]

qr.decomp <- qr(rank2.matrix)
qr.R(qr.decomp)
qr.Q(qr.decomp)

## Multiply the matrix by itself
square.matrix <- tcrossprod(random.matrix)

eigen.decomp <- eigen(square.matrix, symmetric=T)
