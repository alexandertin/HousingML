library(tree)
houses <- read.csv("houses", row.names = "Id")
head(houses)
summary(houses)
house.tree <- tree(SalePrice ~ . - SalePrice, split = "deviance", data = houses)
summary(house.tree)
plot(house.tree)
text(house.tree, pretty=1)

library(randomForest)
set.seed(0)
train = sample(1:nrow(houses), 7*nrow(houses)/10)
house.forest <- randomForest(SalePrice ~ . - SalePrice, data = houses, subset = train, importance = TRUE)
house.forest
oob.err = numeric(23)
for (mtry in 1:23) {
  fit = randomForest(SalePrice ~ . - SalePrice, data = houses, subset = train, mtry = mtry)
  oob.err[mtry] = fit$rsq[500]
  cat("We're performing iteration", mtry, "\n")
}

oob.err
