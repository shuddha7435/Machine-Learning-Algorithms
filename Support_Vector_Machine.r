# Sample heart disease dataset classification problem implementation

install.packages('caret')
library('caret')

heart <- read.csv("Heart_Disease.csv", sep = ',', header = FALSE)

str(heart)

head(heart)

set.seed(3033)
intrain <- createDataPartition(y = heart$V9, p= 0.7, list = FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]


dim(training); 
dim(testing);


anyNA(heart)

summary(heart)


training[["V14"]] = factor(training[["V14"]])


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear


test_pred <- predict(svm_Linear, newdata = testing)
test_pred

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid


