require(glmnet)
require(e1071)
require(randomForest)




svmMultCost = function(x, y, kernel = "linear", type = "C-classification",
			costs = 10^(-3:2)) {

	fit = list()
	for (cost in costs) {
		fit[[as.character(cost)]] = svm(x, y, cost = cost,
			kernel = kernel, type = type)
	}
	class(fit) = "svmMultCost"
	return(fit)
}




predict.svmMultCost = function(object, newx, costs, ...) {
	prediction = matrix(NA, nrow(newx), length(costs))
	colnames(prediction) = costs
	for (cost in costs) {
		index = as.character(cost)
		prediction[, index] = predict(object[[index]], newx)
	}
	return(prediction)
}




customizeGlmnet = function(xTrain, yTrain, xTest, K, dendrogram = NULL,
	testInd = NULL, method = c("glmnet", "svm"),
	family = c("gaussian", "binomial", "multinomial"), costs = NULL) {

	method = method[1]
	family = family[1]

	if (is.null(dendrogram)) dendrogram = hclust(dist(rbind(xTrain, xTest)))
	if (is.null(testInd)) {
		testInd = rep(c(FALSE, TRUE),
			times = c(nrow(xTrain), nrow(xTest)))
	}

	cluster = cutree(dendrogram, k = K)
	clusterTrain = cluster[!testInd]
	clusterTest = cluster[testInd]

	fit = list()
	for (k in 1:K) {
		x = xTrain[clusterTrain == k, ]
		y = yTrain[clusterTrain == k]
		if (family == "multinomial") y = as.factor(as.character(y))
		if (length(y) == 0) {
			fit[[k]] = NA
			class(fit[[k]]) = "singleton"
		} else if (length(unique(y)) == 1) {
			fit[[k]] = unique(y)
			class(fit[[k]]) = "singleton"
		} else if (method == "glmnet") {
			fit[[k]] = glmnet(x, y, family = family)
		} else if (method == "svm") {
			fit[[k]] = svmMultCost(x, y, costs = costs)
		} else if (method == "randomForest") {
			fit[[k]] = randomForest(x, y)
		}
	}

	output = list(cluster = list(train = clusterTrain, test = clusterTest),
		fit = fit, x = list(train = xTrain, test = xTest), y = yTrain,
		method = method, family = family)
	class(output) = "customizeGlmnet"
	return(output)
}




predict.singleton = function(y, ...) {
	if (!is.null(levels(y))) {
		return(levels(y))
	} else return(y)
}




num2label = function(y, numbers) {
	if (length(numbers) == 1 & is.na(numbers[1])) {
		return(NA)
	} else {
		return(levels(y)[numbers])
	}
}




predict.customizeGlmnet = function(object, newx = NULL, regParam = NULL,
	type = c("link", "response", "coefficients", "nonzero", "class")) {

	if (object$family == "gaussian") type = "response" else type = "class"

	prediction = matrix(NA, nrow(object$x$test), length(regParam))
	for (k in sort(unique(object$cluster$test))) {
                newx = object$x$test[object$cluster$test == k, ]
                if (sum(object$cluster$test == k) == 1) newx = t(newx)
		if ( object$method == "glmnet" ||
			is.element('singleton', class(object$fit[[k]]))) {
			prediction[object$cluster$test == k, ] =
				predict(object$fit[[k]], newx, s = regParam,
					costs = regParam, type = "class")
		} else {
			prediction[object$cluster$test == k, ] =
				num2label(object$y[object$cluster$train == k],
					predict(object$fit[[k]], newx,
					s = regParam, costs = regParam,
					type = "class"))
		}
	}
	prediction
}




cv.customizeGlmnet = function(xTrain, yTrain, xTest, Ks,
			dendrogram = NULL, dendrogramCV = NULL,
			foldid = foldid, nfolds = 10,
			method = c("glmnet", "svm", "randomForest"),
			family = c("gaussian", "binomial", "multinomial")) {

	method = method[1]
	if (method == "glmnet") {
		regParam = glmnet(xTrain, yTrain, family = family)$lambda
	} else if (method == "svm") {
		regParam = 10^(-3:2)
	} else if (method == "randomForest") {
		regParam = 1
	}

	family = family[1]
	if (family == "multinomial" | family == "binomial") {
		yTrain = as.factor(yTrain)
	}

	if (is.null(dendrogram)) {
		dendrogram = hclust(dist(rbind(xTrain, xTest)))
	}
	if (is.null(dendrogramCV)) {
		dendrogramCV = hclust(dist(xTrain))
	}

	if (is.null(foldid)) {
		foldid = sample(rep(1:nfolds, length.out = nrow(xTrain))) 
	}
	nfolds = max(foldid)

	error = matrix(NA, length(Ks), length(regParam))
	rownames(error) = Ks
	for (K in Ks) {
		prediction = matrix(NA, length(yTrain), length(regParam))
		for (k in 1:nfolds) {
			xTrain_k = xTrain[foldid != k, ]
			yTrain_k = yTrain[foldid != k]
			xTest_k = xTrain[foldid == k, ]
			yTest_k = yTrain[foldid == k]
			testInd = array(FALSE, nrow(xTrain))
			testInd[foldid == k] = TRUE
			fit = customizeGlmnet(xTrain_k, yTrain_k, xTest_k, K,
				dendrogram = dendrogramCV, testInd = testInd,
				method = method, family = family,
				costs = regParam)
			prediction[foldid == k, ] = predict(fit, newx = xTest_k,
				regParam = regParam)
		}
		error[as.character(K), ] = colSums(prediction != yTrain,
			na.rm = TRUE)# + colSums(is.na(prediction))
	}

	K.min = Ks[which.min(apply(error, 1, min))]
	regParam.min = regParam[which.min(error[as.character(K.min), ])]
	fit = customizeGlmnet(xTrain, yTrain, xTest, K.min, costs = regParam,
		dendrogram = dendrogram, method = method, family = family)
	prediction = predict(fit, regParam = regParam.min)
	return(list(prediction = prediction, fit = fit, K.min = K.min,
		regParam.min = regParam.min, error = error))
}
