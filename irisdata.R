library(rpart)

fit.rpart <- rpart(Species ~ ., data = iris)

library(caret)

fit.rpart <- train(Species ~ ., data = iris, method = "rpart")