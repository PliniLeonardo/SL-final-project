require(progress, quietly = T)
require(ggplot2)
require(data.table, quietly = T)
require(kernlab, quietly = T)
require(e1071, quietly = T)
require(caret, quietly = T)
require(randomForest, quietly = T)

dataset = fread(file = "ahshitherewegoagain.csv")
data = dataset[,-'nation']
data = data[,-'id']
data = data[,-'producer']
data = data[,-'name']
data = data[,-'ml']
data = data[,-'price']
data = data[,-'year']
data = data[,-'latitudine']
data = data[,-'longitudine']
data = data[,-'degree']
data = data[,-(24:31)]
data[,'local1'] = as.factor(data[['local1']])

sum(is.na(data))

dim(data)


colnames(data)

colu = 'local1'

table(data[[colu]])
hist(data[[colu]], breaks = 30)



X = data[,-c('score', 'sweet', 'acidity', 'body', 'tannin')]
chem = data[,c('sweet', 'acidity', 'body', 'tannin')]
y = data[,'score']


prova = kpca( ~ ., data = data.frame(scale(X[,-'local1'])), kernel = "rbfdot", kpar = list(sigma = 1), verbose = T)

cumsum(eig(prova)/sum(eig(prova)))


data_to_plot = rotated(prova)
ggplot(data = data.frame(data_to_plot, score = y), aes(X1, X2, label = score)) +
  geom_point(
    aes(color = score),
    alpha = .7,
    size = 2,
    show.legend = T
  )# + ylim(0, 80)



svm_model = svm(score ~ .,data = data.frame(rotated(prova)[,1:100], score = y), kernel = "linear")


train_pred = predict( svm_model, rotated(prova)[,1:100] )


caret::RMSE(train_pred, as.matrix(y))



set.seed(3)
train_idx = sample(1:149, size = 100, replace = F)
X_train = X[train_idx,]
y_train = y[train_idx,]
X_val = X[-train_idx,]
y_val = y[-train_idx,]


prova = kpca( ~ ., data = data.frame(scale(X_train)), kernel = "rbfdot", kpar = list(sigma = 10), verbose = T)

data_train = predict(prova, scale(X_train))

svm_model = svm(score ~ .,data = data.frame(data_train, score = y_train), kernel = "linear")
train_pred = predict( svm_model, data_train )
caret::RMSE(train_pred, as.matrix(y_train))

val_pred = predict(svm_model, predict(prova, scale(X_val)))
caret::RMSE(val_pred, as.matrix(y_val))



svm_model = svm(score ~ .,data = data.frame(X_train, score = y_train), kernel = "radial", gamma = 1)
train_pred = predict( svm_model, X_train )
caret::RMSE(train_pred, as.matrix(y_train))

val_pred = predict(svm_model, X_val)
caret::RMSE(val_pred, as.matrix(y_val))




set.seed(42)
library("iml")

predictor <- Predictor$new(svm_model, data = X_train, y = y_train)

imp <- FeatureImp$new(predictor, loss = "mae")

plot(imp)


ale <- FeatureEffect$new(predictor, feature = "local1")
ale$plot()
