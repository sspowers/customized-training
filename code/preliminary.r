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
CT1 = CT2 = list()
CT1$error = CT2$error = matrix(NA, length(Ks), length(lambdas))
CT1$complexity = CT2$complexity = matrix(NA, length(Ks), length(lambdas))
CT1$abstentions = CT2$abstentions = matrix(NA, length(Ks), nrow(xTest))

for (j in 1:length(Ks)) {
	K = Ks[j]
	print(K)
	CT = ct1(xTrain, yTrain, xTest, K, lambdas)
	CT1$error[j, ] = colMeans(CT$prediction != yTest, na.rm = TRUE)
	CT1$complexity[j, ] = colMeans(CT$complexity, na.rm = TRUE)
	CT1$abstentions[j, ] = is.na(CT$prediction[, 1])
	CT = ct2(xTrain, yTrain, xTest, K, lambdas)
	CT2$error[j, ] = colMeans(CT$prediction != yTest, na.rm = TRUE)
	CT2$complexity[j, ] = colMeans(CT$complexity, na.rm = TRUE)
	CT2$abstentions[j, ] = is.na(CT$prediction[, 1])
}

KNN = list(error = array(NA, floor(n*max(lambdas))),
			complexity = 1:floor(n*max(lambdas))/n)
NN = get.knnx(xTrain, xTest, floor(n*max(lambdas)))
get.knnpred = function(nn.index) names(which.max(table(yTrain[nn.index])))[1]
for (k in 1:floor(n*max(lambdas))) {
	print(k)
	prediction = apply(as.matrix(NN$nn.index[, 1:k]), 1, get.knnpred)
	KNN$error[k] = mean(yTest != prediction)
}

save(CT1, CT2, KNN, lambdas, file = paste('results/dataset-specific/', dataSet, '.Rdata', sep = ''))

pdf(paste('results/dataset-specific/', dataSet, '.pdf', sep = ''))
yMax = max(CT1$error, CT2$error, KNN$error, na.rm = TRUE)
plot(lambdas, CT1$error[1, ], type = 'l', xlab = expression(lambda), ylab = 'error',
	ylim = range(CT1$error, CT2$error, KNN$error, na.rm = TRUE))
for (k in 2:5) lines(lambdas, CT1$error[k, ], type = 'l', col = k, lty = 1)
for (k in 1:5) lines(lambdas, CT2$error[k, ], type = 'l', col = k + 5, lty = 2)
lines(KNN$complexity, KNN$error, col = 11, lty = 2)
legend("topleft", c('CT1(1)', 'CT1(2)', 'CT1(3)', 'CT1(5)', 'CT1(10)',
					'CT2(1)', 'CT2(2)', 'CT2(3)', 'CT2(5)', 'CT2(10)',
					'KNN'), col = 1:11, lty = c(rep(1, 5), rep(2, 6)))
legend("topright", c(paste('n =', n), paste('p =', p)))
dev.off()
