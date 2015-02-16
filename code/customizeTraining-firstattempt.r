design.matrix = function(xTrain, xTest, group) {
	x = rbind(xTrain, xTest)
	groupDesigns = lapply(1:length(unique(group)), function(g) {
		design = cbind(1000, scale(x)); design[group != g, ] = 0; design})
	design = do.call(cbind, groupDesigns)
	list(train = design[1:nrow(xTrain), ],
		test = design[nrow(xTrain) + 1:nrow(xTest), ])
}




customizeGlmnet = function(xTrain, yTrain, xTest, Gs,
				nfolds = 10, foldid, dendrogram, ...) {

	if (missing(foldid)) {
		foldid = sample(rep(1:nfolds, length.out = nrow(xTrain)))}
	nfolds = length(unique(foldid))

	if (missing(dendrogram)) dendrogram = hclust(dist(rbind(xTrain, xTest)))

	fit = lapply(1:length(Gs), function(x) NULL)

	for (index in 1:length(Gs)) {
		cluster = cutree(dendrogram, k = Gs[index])
		design = design.matrix(xTrain, xTest, cluster)
		fit[[index]] = cv.glmnet(design$train, yTrain, foldid = foldid,
			standardize = FALSE, ...)
		fit[[index]]$prediction = predict(fit[[index]], design$test,
			type = 'response')
	}

	cvm = lapply(fit, function(glmnet.fit) glmnet.fit$cvm) 
	G.min.index = which.min(sapply(cvm, min))

	list(prediction = fit[[G.min.index]]$prediction,
		G.min = Gs[G.min.index],
		lambda.min = fit[[G.min.index]]$lambda.min)
}




customizeSVM = function(xTrain, yTrain, xTest, Gs, dendrogram) {

	if (missing(dendrogram)) dendrogram = hclust(dist(rbind(xTrain, xTest)))

	fit = lapply(1:length(Gs), function(x) NULL)

	for (index in 1:length(Gs)) {
		cluster = cutree(dendrogram, k = Gs[index])
		design = design.matrix(xTrain, xTest, cluster)
		fit[[index]] = tune(svm, design$train, yTrain, scale = FALSE,
			kernel = 'linear', ranges = list(cost = 10^(-3:2)))
		fit[[index]]$prediction =
			predict(fit[[index]]$best.model, design$test)
	}

	cvm = lapply(fit, function(svm.fit) svm.fit$best.performance)
	G.min.index = which.min(sapply(cvm, min))
	
	list(prediction = fit[[G.min.index]]$prediction,
		G.min = Gs[G.min.index],
		cost.min = fit[[index]]$best.parameters$cost)
}




customizeRF = function(xTrain, yTrain, xTest, G, dendrogram) {

	if (missing(dendrogram)) dendrogram = hclust(dist(rbind(xTrain, xTest)))

	cluster = cutree(dendrogram, k = G)
	clusterTrain = cluster[1:nrow(xTrain)]
	clusterTest = cluster[nrow(xTrain) + 1:nrow(xTest)]

	prediction = array(NA, nrow(xTest))
	for (g in 1:G) {
		xTraing = xTrain[clusterTrain == g, ]
		yTraing = yTrain[clusterTrain == g]
		xTestg = xTest[clusterTest == g, ]
		if (length(unique(yTraing)) > 1 && sum(clusterTest == g) > 0) {
			forest = randomForest(xTraing, yTraing)
			prediction[index, clusterTest == g] =
				predict(forest, xTestg)
		} else if (length(unique(yTraing))==1&&sum(clusterTest==g)>0) {
			prediction[index, clusterTest == g] =
				unique(yTraing)
		}
	}

	prediction
}




cv.customizeRF = function(xTrain, yTrain, xTest, Gs,
				nfolds = 10, foldid, dendrogram) {

        if (missing(foldid)) {
                foldid = sample(rep(1:nfolds, length.out = nrow(xTrain)))}
        nfolds = length(unique(foldid))

        if (missing(dendrogram)) dendrogram = hclust(dist(rbind(xTrain, xTest)))

	prediction = matrix(NA, nrow(xTrain), length(Gs))

	for (index in 1:length(Gs)) {
		G = Gs[index]
		for (k in 1:nfolds) {
			xTraink = xTrain[foldid != k, ]
			yTraink = yTrain[foldid != k, ]
			xTestk = xTrain[foldid == k, ]
			forest = customizeRF(xTraink, yTraink, xTestk,
				G, dendrogram)
			prediction[foldid == k, index] = predict(forest, xTestk)
		}
	}

	
}





