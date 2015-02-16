require(FNN)
require(glmnet)
require(MCMCpack)

ct = function(xTrain, yTrain, xTest, K, lambda, clustering = c('joint', 'test'),
				family = c('multi', 'gaussian')) {

	clustering = clustering[1]
	family = family[1]
	if (family == 'multinomial') type = 'class'
	if (family == 'gaussian') type = 'response'
	prediction = matrix(NA, nrow(xTest), length(lambda))
#	complexity = matrix(NA, K, length(lambda))
	
	if (clustering == 'joint') dendrogram = hclust(dist(rbind(xTrain, xTest)))
	if (clustering == 'test') dendrogram = hclust(dist(xTest))
	height = dendrogram$height[nrow(xTest) - K]
	clusterTest = cutree(dendrogram, k = K)
	if (clustering == 'joint') {
		clusterTrain = clusterTest[1:nrow(xTrain)]
		clusterTest = clusterTest[nrow(xTrain) + 1:nrow(xTest)]}
	if (clustering == 'test')	neighbors = get.knnx(xTrain, xTest)$nn.index
	
	for (k in sort(unique(clusterTest))) {
		
		xTestk = matrix(xTest[clusterTest == k, ], ncol = ncol(xTrain))
		
		if (clustering == 'joint') {
			xTraink = matrix(xTrain[clusterTrain == k, ], ncol = ncol(xTrain))
			yTraink = yTrain[clusterTrain == k]
		}
		if (clustering == 'test') {
			customizedTrainingSet = unique(c(neighbors[clusterTest == k, ]))
			xTraink = xTrain[customizedTrainingSet, ]
			yTraink = yTrain[customizedTrainingSet]
		}
		
		if (length(unique(yTraink)) > 1 && nrow(xTestk) > 0) {
			if (family == 'multinomial') yTraink = as.factor(as.character(yTraink))
			classifier = glmnet(xTraink, yTraink,
								family = family)
			prediction[clusterTest == k,]=predict(classifier,xTestk,s=lambda,type,exact=FALSE)
			selected = predict(classifier, xTestk, s=lambda, 'nonzero',exact=FALSE)
			if (family == 'multinomial') {
				if (min(sapply(selected, length)) == 1) {
					for (i in which(sapply(selected, length) == 1)) {
						selected[[i]] = as.list(selected[[i]][[1]])
					}
				}
#				complexity[k, ] = rowSums(sapply(selected, sapply, length))
			}
#			if (family == 'gaussian') complexity[k, ] = sapply(selected, length)
		}
		else if (length(unique(yTraink)) == 1 && nrow(xTestk) > 0) {
			singleton = unique(yTraink)
			if (family == 'multinomial') singleton = as.character(singleton)
			prediction[clusterTest == k, ] = singleton
#			complexity[k, ] = 0
		}
	}
	
#	return(list(prediction = prediction, complexity = complexity))
	return(list(prediction = prediction))
}


cv.ct = function(xTrain, yTrain, xTest, Ks, lambda = NULL,
		clustering = c('joint', 'test'), family = c('multi', 'gaussian'),
		foldid = NULL, nfolds = 10) {

	n = nrow(xTrain)

	if (is.null(lambda)) {
		lambda = glmnet(xTrain, yTrain, family = family)$lambda
	}
	if (is.null(foldid)) {
		foldid = sample(rep(1:nfolds, length.out = nrow(xTrain)))
	}
	nfolds = max(foldid)

	errorTotal = array(dim = c(nfolds, length(Ks), length(lambda)))
	predictionCount = array(dim = c(nfolds, length(Ks), length(lambda)))
	for (k in 1:nfolds) {
		print(paste('making predictions for fold', k))
		test = foldid == k
		xTraink = xTrain[-test, ]
		yTraink = yTrain[-test]
		xTestk = xTrain[test, ]
		yTestk = yTrain[test]
		for (j in 1:length(Ks)) {
			prediction = ct(xTraink, yTraink, xTestk, Ks[j], lambda,
								clustering, family)$prediction
			if (family == 'gaussian') {
				errorTotal[k, j, ] = colSums((prediction - yTestk)^2, na.rm = TRUE)
			}
			if (family == 'multinomial') {
				errorTotal[k, j, ] = colSums(prediction != yTestk, na.rm = TRUE)
			}
			predictionCount[k, j, ] = colSums(!is.na(prediction))
		}
	}
	errorGrid = apply(errorTotal, c(2, 3), sum)/apply(predictionCount, c(2, 3), sum)
	j = which.max(rowSums(errorGrid == min(errorGrid, na.rm = TRUE)) > 0)
	i = which.min(errorGrid[j, ])
	K = Ks[j]
	lambda.min = lambda[i]
	best = ct(xTrain, yTrain, xTest, K, lambda.min, clustering, family)
	CV = list(K = K, lambda.min = lambda.min, errorGrid = errorGrid)
#	list(prediction = best$prediction[, i], complexity = best$complexity[, i], CV = CV)
	list(prediction = best$prediction, CV = CV)
}

cv.knn = function(xTrain, yTrain, xTest, family = c('multi', 'gaussian'),
			nfolds = 5, permutation = NULL) {
	family = family[1]
	n = nrow(xTrain)
	kMax = min(100, n/2)
	if (is.null(permutation)) permutation = sample(1:n)
	errorTotal = matrix(NA, nfolds, kMax)
	for (k in 1:nfolds) {
		print(paste('making predictions for fold', k))
		test = permutation[round(n*(k-1)/nfolds+1):round(n*k/nfolds)]
		xTraink = xTrain[-test, ]
		yTraink = yTrain[-test]
		xTestk = xTrain[test, ]
		yTestk = yTrain[test]
		NN = get.knnx(xTraink, xTestk, kMax)
		if (family == 'gaussian') get.knnpred = function(nn.index) {
			mean(yTrain[nn.index])
		}
		if (family == 'multinomial') get.knnpred = function(nn.index) {
			names(which.max(table(yTrain[nn.index])))[1]
		}
		for (j in 1:kMax) {
			prediction = apply(as.matrix(NN$nn.index[, 1:j]), 1, get.knnpred)
			if (family == 'gaussian') errorTotal[k, j] = sum((prediction-yTestk)^2)
			if (family == 'multinomial') errorTotal[k, j] = sum(prediction != yTestk)
		}
	}
	error = colSums(errorTotal)/n
	kOpt = which.min(error)
	if (family=='multi') prediction = as.character(knn(xTrain,xTest,yTrain,k=kOpt))
	if (family == 'gaussian') prediction = knn.reg(xTrain,xTest,yTrain,k=kOpt)$pred
	list(prediction = prediction, CV = list(k = kOpt, error = error))
}
