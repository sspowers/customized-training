source('code/customizedTraining.r')

xTrain = matrix(scan('data/massspec/x.txt'), ncol = 2220, byrow = T)
yTrain = scan('data/massspec/y.txt')
patientTrain = scan('data/massspec/pattr.txt')
xTest = matrix(scan('data/massspec/xte.txt'), ncol = 2220, byrow = T)
yTest = scan('data/massspec/yte.txt')
patientTest = scan('data/massspec/patte.txt')
fold = scan('data/massspec/foldid.txt')

patient = unique(patientTest)
yTrain[yTrain == 3] = 1
yTest[yTest == 3] = 1
n = nrow(xTrain)
p = ncol(xTrain)
m = nrow(xTest)

# Standard training
classifierST = glmnet(xTrain, yTrain, 'binomial')
lambdas = classifierST$lambda
errorST = matrix(NA, length(unique(fold)), length(lambdas))
for (k in 1:length(unique(fold))) {
	print(paste('Making predictions for fold', k))
	validation = fold == unique(fold)[k]
	classifier = glmnet(xTrain[!validation, ], yTrain[!validation], 'binomial')
	prob = predict(classifier, xTrain[validation, ], lambdas, 'response')
	pred = 1 + (prob > 1/3)
	y = matrix(yTrain[validation], nrow = sum(validation), ncol = length(lambdas))
	errorST[k, ] = colSums(y == 1 & pred == 2) + 2*colSums(y == 2 & pred == 1)
}
lambdaST = lambdas[which.min(colSums(errorST))]
probST = predict(classifierST, xTest, lambdaST, 'response')
predST = 1 + (probST > 1/3)
lossST = (yTest == 1 & predST == 2) + 2*(yTest == 2 & predST == 1)
mean(lossST)
aggregate(lossST, by = list(patientTest), FUN = mean)

selectedST = predict(classifierST, xTest, lambdaST, 'nonzero')[[1]]
length(selectedST)

# Customized training
errorCT = matrix(NA, length(unique(fold)) - 1, length(lambdas))
for (k in 1:(length(unique(fold)) - 1)) {
	print(paste('Making predictions for fold', k))
	validation = fold == unique(fold)[k]
	NN = get.knnx(xTrain[!validation, ], xTrain[validation, ])$nn.index
	CTset = unique(c(NN))
	classifier = glmnet(xTrain[CTset, ], yTrain[CTset], 'binomial')
	prob = predict(classifier, xTrain[validation, ], lambdas, 'response')
	pred = 1 + (prob > 1/2)
	y = matrix(yTrain[validation], nrow = sum(validation), ncol = length(lambdas))
	errorCT[k, ] = colSums(y == 1 & pred == 2) + 2*colSums(y == 2 & pred == 1)
}
lambdaCT = lambdas[which.min(colSums(errorCT))]

predCT = rep(NA, m)
selectedCT = matrix(0, length(patient), p + 1)
for (i in 1:length(patient)) {
	print(paste('Making predictions for patient', i))
	test = patientTest == unique(patient)[i]
	NN = get.knnx(xTrain, xTest[test, ])$nn.index
	CTset = unique(c(NN))
	classifier = glmnet(xTrain[CTset, ], yTrain[CTset], 'binomial')
	prob = predict(classifier, xTest[test, ], lambdaCT, 'response')
	predCT[test] = 1 + (prob > 1/3)
	selectedCT[i, predict(classifier, xTest[test, ], lambdaCT, 'nonzero')[[1]]] = 1
}
lossCT = ((yTest == 1 & predCT == 2) + 2*(yTest == 2 & predCT == 1))
mean(lossCT)
aggregate(lossCT, by = list(patientTest), FUN = mean)

require(proxy)
dist(selectedCT, method = 'Jaccard')

save(predST, selectedST, predCT, selectedCT, file = 'results/massspec2.Rdata')


forLivia = matrix(NA, 14, 6)
colnames(forLivia) = patient
rownames(forLivia) = unique(patientTrain)
for (i in 1:length(patient)) {
	test = patientTest == unique(patient)[i]
	NN = get.knnx(xTrain, xTest[test, ])$nn.index
	CTset = unique(c(NN))
	for (id in unique(patientTrain)) {
		forLivia[as.character(id), i] = sum(patientTrain[CTset] == id)
	}
}
write.csv(forLivia, file = 'results/table.csv')
