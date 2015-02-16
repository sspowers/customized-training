require(FNN)
require(glmnet)
require(MCMCpack)
require(modeest)


# Function: builddataset
# Usage: builddataset(x, g)
# -------------------------
# Takes in x a numeric data matrix, and g an integer vector that indicates
# the 1-indexed group membership of each row of x.  Returns the synthetic
# dataset for data shared analysis (observations retain their ordering)
#
builddataset <- function(x, g, intercept=TRUE){
    extracols <- lapply(1:length(unique(g)), function(kk){
        tmp <- x
        if(!is.null(colnames(x))){
            colnames(tmp) <- paste(colnames(x), "_G", kk, sep = "")}
        tmp[g != kk,] <- 0
        tmp
    })
    if(intercept){
        ints <- lapply(1:length(unique(g)), function(kk){
            ifelse(g == kk, 1, 0)
    })
        names(ints) <- paste("Int_G", 1:length(unique(g)), sep = "")
        do.call(cbind, args = c(ints, list(x), extracols))
    }
    else{
        do.call(cbind, args = c(list(x), extracols))
    }
}


# Function: builddatasetmatg
# Usage: builddatasetmatg(x, G)
# -----------------------------
# Same thing as builddataset but for indicator matrix G where G[i,j]
# indicates that obs i belongs to group j.
#
builddatasetmatg <-  function(x, G){
    extracols <- lapply(1:ncol(G), function(jj){
        tmp <- x
        if(!is.null(colnames(x))){
            colnames(tmp) <- paste(colnames(x), "_G", jj, sep = "")}
        tmp[!G[,jj],] <- 0
        tmp
    })
    do.call(cbind, args = c(list(x), extracols))
}


# Function: dsl1.glmnet
# Usage: dsl1.glmnet(z, y, r, p)
# ------------------------------
# Runs data-shared lasso for synthetic dataset z. First p coefficients penalized with
# relative rate 1 and the rest are penalized with relative rate r. Typically p is ncol(x).
# Number of groups (gmax) must be specified to run with separate intercepts (intercept = FALSE).
# To run with NO intercept, use intercept = FALSE and gmax = 0.
#
dsl1.glmnet <-  function(z, y, r, p, gmax = NULL, thresh = 1e-8, standardize = FALSE,
                         intercept = FALSE, exclude = integer(0), ...){
  if(intercept){
      pf = rep(c(1, r), times = c(p, ncol(z) - p))
  }
  else{
      pf = rep(c(0, 1, r), times = c(gmax, p, ncol(z) - p - gmax))
  }
  glmnet(z, y, thresh = thresh, penalty.factor=pf, intercept =
         intercept, exclude = exclude, standardize = standardize, ...)
}




# Function: dsl1.cv.glmnet
# Last updated: Aug 5, 2014
# ---------------------------------
# Similar to dsl1.glmnet, but with CV over lambda
# ---------------------------------
# Arguments:
# z	If 
#
dsl1.cv.glmnet = function(z, y, r, p, gmax = NULL, standardize = FALSE, intercept = FALSE, thresh = 1e-8, ...) {
  if(intercept){
      pf = rep(c(1, r), times = c(p, ncol(z) - p))
  }
  else{
      pf = rep(c(0, 1, r), times = c(gmax, p, ncol(z) - p - gmax))
  }
  cv.glmnet(z, y, penalty.factor = pf, standardize = standardize, thresh = thresh,
            intercept = intercept, ...)
}



# Function: augmentedCols
# Last updated: Aug 12, 2014
# ------------
# Expand the n-by-p covariate matrix into an n-by-(G*p), where G is the
# number of groups. Each of the G sets of p columns corresponds to the
# coefficients for one group.
# ------------
# Arguments:
# x	n-by-p matrix of n observations in p variables
# g	n-by-G matrix of group incidence
augmentedCols = function(x, g) {
    extracols <- lapply(1:ncol(g), function(jj){
        tmp <- x
        if(!is.null(colnames(x))){
            colnames(tmp) <- paste(colnames(x), "_g", jj, sep = "")}
        tmp[!g[,jj],] <- 0
        tmp
    })
    do.call(cbind, args = extracols)
}




# Function: augmentedRows
# Last updated: Aug 12, 2014
# ------------
# Given p and G, return the Cholesky decomposition of the penalty matrix
# corresponding to the differences between pairs of betas. Augmenting
# the result to the (expanded) design matrix allows an L2 penalty on these
# differences in glmnet.
# ------------
# Arguments:
# p	number of predictors
# G	number of groups
#
augmentedRows = function(p, G) {
	matrix = diag(sqrt(G - (1:G - 1)/(G + 2 - 1:G)))
	for (row in 1:(G-1)) {
		matrix[row, (row+1):G] = -matrix[row, row]/(G + 1 - row)
	}
	kronecker(matrix, diag(rep(1, p)))
}




# Function: dsl2.glmnet (not currently up and running)
# Last updated: Aug 7, 2014
# -------------------------
# Scott's idea for DSL: no common beta, but penalize (L2) the differences
# between betas for each pair of groups
# -------------------------
# Arguments:
# x		n-by-p matrix of n observations in p variables
# y		length-n vector of responses
# g		n-by-G matrix of group memberships, for G groups
# gamma	a regularization parameter on the difference between
#		the regression coefficients in each group.
#		the greater gamma is the more similar the group fits are
# ...	additional arguments to be passed to glmnet
#
dsl2.glmnet = function(x, y, g, gamma = 1,
		standardize = TRUE, ...) {
	if (standardize) x = scale(x)
	n = nrow(x)
	x = cbind(rep(1, n), x)
	p = ncol(x)
	G = ncol(g)
	xAugCol = augmentedCols(x, g)
	xAugRow = augmentedRows(p, G)
	xDesign = rbind(xAugCol, sqrt(gamma)*xAugRow)
	yDesign = c(y, rep(0, G*p))
	glmnet(xDesign, yDesign, standardize = FALSE, ...)
}

#Sys.time = Sys.time()
#	glmnet(xDesign, yDesign, standardize = FALSE, family = 'binomial')
#Sys.time() - Sys.time


# Function: dsl.ctj
# Usage: dsl.ctj(xTrain, yTrain, xTest, K, lambdas, family)
# -------------------------------------------------------
# Given a number K of clusters, jointly clusters the training and test data
# and fits a data-shared lasso on the clusters. Returns only the predictions.
#
dsl.ctj = function(xTrain, yTrain, xTest, K, lambda = NULL, family = c('multinomial', 'gaussian'),  ...) {

	family = family[1]
	if(family == 'multi') {
		type = 'class'
		yTrain = as.character(yTrain)}
	if(family == 'gaussian') type = 'response'
	p = ncol(xTrain)
	r = sqrt(1/K)
        gmax = K
	
	dendrogram = hclust(dist(rbind(xTrain, xTest)))
	cluster = cutree(dendrogram, k = K)
	clusterTrain = cluster[1:nrow(xTrain)]
	clusterTest = cluster[nrow(xTrain) + 1:nrow(xTest)]

	designTrain = builddataset(xTrain, clusterTrain)
	designTest = builddataset(xTest, clusterTest)
			
	model = dsl1.glmnet(designTrain, yTrain, r, p, gmax, lambda = lambda, family = family, ...)
	prediction = predict(model, designTest, lambda, type)
	return(prediction)
}




# Function: cv.dsl.ctj(xTrain, yTrain, xTest, Ks, foldid, family, ...)
# -------------------------------------------------------
# Given an array of possible choices of the number of clusters K and an array of
# possible values of the regularization parameter lambda, evaluates CV error for each
# combination of the parameters and returns predictions for xTest corresponding to
# the best CV error. Also returns a grid of errors for the parameters. 
#
cv.dsl.ctj = function(xTrain, yTrain, xTest, Ks, foldid,
		family = c('gaussian', 'binomial', 'multinomial'), ...) {
	
	family = family[1] 
	n = nrow(xTrain)
	p = ncol(xTrain)
	dendrogram = hclust(dist(rbind(xTrain, xTest)))

	models = lapply(Ks, function(K, ...) {

		print(K)
		r = sqrt(1/K)
                gmax = K
		
		cluster = cutree(dendrogram, k = K)
		if (min(table(cluster)) == 1) print('Warning: singleton cluster')
		clusterTrain = cluster[1:nrow(xTrain)]
		clusterTest = cluster[nrow(xTrain) + 1:nrow(xTest)]
                dslint = FALSE
                
		if (K == 1) {
			designTrain = xTrain
			designTest = xTest
                        dslint = TRUE
		} else {
			designMatrix = builddataset(rbind(xTrain, xTest), cluster)
			designTrain = designMatrix[1:n, ]
			designTest = designMatrix[-(1:n), ]
		}

		model = dsl1.cv.glmnet(designTrain, yTrain, r, p, gmax, family = family, foldid = foldid, intercept = dslint, ...)
		model$prediction = predict(model, designTest, s = 'lambda.min', type = 'response')
		return(model)
        })

        errors = lapply(models, function(model){model$cvm})
        K.min.index = which.min(sapply(errors, min))
        prediction = models[[K.min.index]]$prediction
        K.min = Ks[K.min.index]
        lambda.min = models[[K.min.index]]$lambda.min

	return(list(prediction = prediction, K.min = K.min, lambda.min = lambda.min))
}




# Function: cv.dsl.ctt
# Last updated: Aug 5, 2014
# -------------------------------------------------------
# Implements CT+DSL with pre-specified test clusters, allowing for
# overlapping groups in the training sets.
# -------------------------------------------------------
# Arguments:
# xTrain	an n-by-p matrix of covariates
# yTrain	a length-n vector of responses
# xTest		an m-by-p matrix of covariates
# Ks		a vector of values of K over which to cross-validate
#		K denotes the number of clusters to find
# foldid	a length-n vector denoting fold membership of the training
#		data for cross-validation. Must be specified.
# family	response type
# -------------------------------------------------------
# Value:
# An object of class 'cv.glmnet' is returned, with the additional value
# prediction, which gives the predictions for the supplied test data.
#
cv.dsl.ctt = function(xTrain, yTrain, xTest, groupid, foldid,
		family = c('gaussian', 'binomial', 'multinomial'), ...) {
	
	family = family[1]
	n = nrow(xTrain)
	p = ncol(xTrain)
	groups = unique(as.factor(groupid))[order(unique(as.factor(groupid)))]
	r = 1/sqrt(length(groups))
        gmax = length(groups)

	NN = get.knnx(xTrain, xTest)$nn.index
	CTset = lapply(groups, function(k) unique(c(NN[groupid == k, ])))
	groupTrain = sapply(CTset, function(neighbors) {
		column = rep(0, n)
		column[neighbors] = 1
		return(column)
	})
	designTrain = builddatasetmatg(xTrain, groupTrain)

	groupTest = model.matrix( ~ as.factor(groupid) - 1)
	designTest = builddatasetmatg(xTest, groupTest)

	Sys.time = Sys.time()
	model = dsl1.cv.glmnet(designTrain, yTrain, r, p, gmax, family = family, foldid = foldid)
	Sys.time() - Sys.time

	model$prediction = predict(model, designTest, s = 'lambda.1se', type = 'response')
	return(model)
}









