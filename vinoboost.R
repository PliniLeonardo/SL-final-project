library(xgboost)
library(caret)


data = read.csv('C:/Users/CreCre/Downloads/sl_final/ahshitherewegoagain.csv')


data[,'local1'] = as.factor(data[,'local1'])


set.seed(420)
train_idx = sample(1:dim(data)[1], replace = FALSE, size = floor(0.8 * dim(data)[1]))
train = data[train_idx,]
val = data[-train_idx,]

#train[,c(5:33, 35:43)]

X_train = data.matrix(train[,c(5, 16:32, 42)])
label = data.matrix(train[,33])

X_test = data.matrix(val[,c(5, 16:32, 42)])
label_val = data.matrix(val[,33])

xgboost_train = xgb.DMatrix(data=X_train, label=label)
xgboost_val = xgb.DMatrix(data=X_test, label=label_val)


#?xgboost


model = xgboost(data = xgboost_train, eta = 0.07, max.depth = 9, nrounds = 10000,
                verbose = 0, early_stopping_rounds = 100, lambda = 0.7, min_child_weight = 1,
                objective="reg:squarederror") 



pred_val = predict(model, xgboost_val)

caret::RMSE(label_val, pred_test)


