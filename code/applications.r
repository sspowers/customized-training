# Needs: dataSet, x, y, test

x = scale(x)
xTrain = x[-test, ]
yTrain = y[-test]
xTest = x[test, ]
yTest = y[test]
n = nrow(xTrain)
p = ncol(xTrain)

Ks = c(1, 2, 3, 5, 10)
lambdas = glmnet(xTrain, yTrain, family = 'multi', nlam = 100)$lambda

if(is.null(numFolds)) numFolds = 5
if(is.null(permutation)) permutation = sample(1:n)
lasso = cv.ct(xTrain, yTrain, xTest, 1, lambdas,
			'joint', 'multi', numFolds, permutation)
CT1 = cv.ct(xTrain, yTrain, xTest, Ks, lambdas,
			'joint', 'multi', numFolds, permutation)
CT2 = cv.ct(xTrain, yTrain, xTest, Ks, lambdas,
			'test', 'multi', numFolds, permutation)
KNN = cv.knn(xTrain, yTrain, xTest, 'multi', numFolds, permutation)

print('lasso test error:')
print(mean(lasso$prediction != yTest))
print('CT1 test error:')
print(mean(CT1$prediction != yTest))
print('CT2 test error:')
print(mean(CT2$prediction != yTest))
print('KNN test error:')
print(mean(KNN$prediction != yTest))

save(lasso, CT1, CT2, KNN, file = paste('results/dataset-specific/', dataSet, 'CV.Rdata', sep = ''))