require(FNN)
require(glmnet)




customizeGlmnet = function(xTrain, yTrain, xTest, groupid = NULL, G = NULL,
    dendrogram = NULL, dendrogramTestIndices = NULL, 
	family = c("gaussian", "binomial", "multinomial")) {

	family = family[1]
    CTset = list()

    if (!is.null(groupid)) {

        groups = as.character(sort(unique(groupid)))
        G = length(groups)
        
        for (group in groups) {
            NN = get.knnx(xTrain, xTest[groupid == group, ])$nn.index
            CTset[[group]] = unique(c(NN))
        }

    } else {

        if (is.null(G)) {
        stop('Either G or group must be specified')
        }

        if (is.null(dendrogram)) {
            dendrogram = hclust(dist(rbind(xTrain, xTest)))
        }

        if (is.null(dendrogramTestIndices)) {
            dendrogramTestIndices =
                rep(c(FALSE, TRUE), times = c(nrow(xTrain), nrow(xTest)))
        }

        cluster = cutree(dendrogram, k = G)
        groupid = cluster[dendrogramTestIndices]
        groups = 1:G

        for (group in groups) {
            CTset[[group]] = which(cluster[!dendrogramTestIndices] == group)
        }
    }

	fit = list()
	for (group in groups) {
		x = xTrain[CTset[[group]], ]
		y = yTrain[CTset[[group]]]
		if (family == "multinomial") y = as.factor(as.character(y))
		if (length(y) == 0) {
			fit[[group]] = NA
			class(fit[[group]]) = "singleton"
		} else if (length(unique(y)) == 1) {
            if (family == "gaussian") {
    			fit[[group]] = unique(y)
            } else if (family == "binomial") {
                fit[[group]] = 1*(unique(y) == sort(unique(yTrain))[2])
            }
			class(fit[[group]]) = "singleton"
		} else {
			fit[[group]] = glmnet(x, y, family = family)
		}
    }

	output = list(CTset = CTset, fit = fit, groupid = groupid,
		x = list(train = xTrain, test = xTest), y = yTrain, family = family)
	class(output) = "customizeGlmnet"
	return(output)
}




predict.singleton = function(y, ...) {
	if (!is.null(levels(y))) {
		return(levels(y))
	} else return(y)
}




predict.customizeGlmnet = function(object, newx = NULL, lambda = NULL) {

    groups = as.character(sort(unique(object$groupid)))
	prediction = matrix(NA, nrow(object$x$test), length(lambda))

	for (group in groups) {

        newx = object$x$test[object$groupid == group, ]

        if (sum(object$groupid == group) == 1) {
            newx = t(newx)
        }

        prediction[object$groupid == group, ] =
            predict(object$fit[[group]], newx,
            s = lambda/object$fit[[group]]$nobs, type = "response")
    }

	prediction
}




cv.customizeGlmnet = function(xTrain, yTrain, xTest, groupid = NULL, Gs = NULL,
			dendrogram = NULL, dendrogramCV = NULL,
			nfolds = 10, foldid = NULL,
			family = c("gaussian", "binomial", "multinomial")) {

	family = family[1]
    lambda = glmnet(xTrain, yTrain, family = family)$lambda*nrow(xTrain)

	if (family == "multinomial" | family == "binomial") {
		yTrain = as.factor(yTrain)
	}

    if (!is.null(groupid)) {
        Gs = length(unique(groupid))
    } else {

        if (is.null(Gs)) {
            stop('Either groupid or Gs must be specified')}

	    if (is.null(dendrogram)) {
		    dendrogram = hclust(dist(rbind(xTrain, xTest)))}

	    if (is.null(dendrogramCV)) {
		    dendrogramCV = hclust(dist(xTrain))}
    }

	if (is.null(foldid)) {
		foldid = sample(rep(1:nfolds, length.out = nrow(xTrain))) 
	}
	folds = sort(unique(foldid))

	error = matrix(NA, length(Gs), length(lambda))
	rownames(error) = Gs
	for (G in Gs) {
		prediction = matrix(NA, length(yTrain), length(lambda))
		for (fold in folds) {
			xTrain_k = xTrain[foldid != fold, ]
			yTrain_k = yTrain[foldid != fold]
			xTest_k = xTrain[foldid == fold, ]
			yTest_k = yTrain[foldid == fold]
            groupid_k = foldid[foldid == fold]
			dendrogramTestIndices = array(FALSE, nrow(xTrain))
			dendrogramTestIndices[foldid == fold] = TRUE
			fit = customizeGlmnet(xTrain_k, yTrain_k, xTest_k, groupid_k, G,
				dendrogramCV, dendrogramTestIndices, family)
 			prediction[foldid == fold, ] = predict(fit, xTest_k, lambda)
		}
        if (family == "gaussian") {
            error[as.character(G), ] =
                colMeans((prediction - yTrain)^2, na.rm = TRUE)
        } else if (family == "binomial") {
 		    error[as.character(G), ] =
                colMeans(1 + (prediction > 1/2) != yTrain, na.rm = TRUE)
        }
	}

	G.min = Gs[which.min(apply(error, 1, min))]
	lambda.min = lambda[which.min(error[as.character(G.min), ])]
	fit = customizeGlmnet(xTrain, yTrain, xTest, groupid, G.min,
		dendrogram, family = family)
	prediction = predict(fit, lambda = lambda.min)
	return(list(prediction = prediction, fit = fit, G.min = G.min,
		lambda.min = lambda.min, error = error))
}
