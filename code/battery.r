require(e1071)
require(glmnet)
require(randomForest)
source('code/customizeTraining.r')
source('code/customizedTraining.r')
directory = 'http://statweb.stanford.edu/~sspowers/battery/'

for (dataset in c(#'BalanceScale', 'BreastCancerWisconsin(Diagnostic)',
#		'Chess(King-RookvsKing-Pawn)', 'ContraceptiveMethodChoice',
#		'Fertility',
#		'First-ordertheoremproving',
#		'LSVTVoiceRehabilitation', 'Mushroom',
#		'Musk(Version1)',
		'OpticalRecognitionofHandwrittenDigits', 'Parkinsons',
#		'QSARbiodegradation', 'seeds', 'SteelPlatesFaults',
#		'TeachingAssistantEvaluation', 'UserKnowledgeModeling',
		'vowel')) {
print(paste('Working on dataset', dataset))
source(paste(directory, 'code/', dataset, '.r', sep = ''))
source(paste(directory, 'code/formatting.r', sep = ''))

#if(!exists('foldid')) foldid = sample(rep(1:5, length.out = nrow(xTrain)))
if (!exists('foldid')) {
	permutation = sample(1:nrow(xTrain))
	foldid = rep(NA, nrow(xTrain))
	for (k in 1:5) {
		foldid[permutation[round(n*(k-1)/5+1):round(n*k/5)]] = k
	}
}

Ks = c(1, 2, 3, 5, 10)
dendrogram = hclust(dist(rbind(xTrain, xTest)))
dendrogramCV = hclust(dist(xTrain))

Sys.time = Sys.time()
glmnet = cv.customizeGlmnet(xTrain, yTrain, xTest, 1,
        dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
        method = "glmnet", family = "multinomial")
print(mean(glmnet$prediction != yTest))
print(Sys.time() - Sys.time)

Sys.time = Sys.time()
glmnetCT = cv.customizeGlmnet(xTrain, yTrain, xTest, Ks,
	dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
	method = "glmnet", family = "multinomial")
print(mean(glmnetCT$prediction != yTest))
print(Sys.time() - Sys.time)

#Sys.time = Sys.time()
#old = cv.ct(xTrain, yTrain, xTest, Ks, clustering = 'joint', family = 'multinomial',
#	foldid = foldid)
#print(mean(old$prediction != yTest))
#print(Sys.time() - Sys.time)


Sys.time = Sys.time()
svm = cv.customizeGlmnet(xTrain, yTrain, xTest, 1,
        dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
        method = "svm", family = "multinomial")
print(mean(svm$prediction != yTest))
print(Sys.time() - Sys.time)

Sys.time = Sys.time()
svmCT = cv.customizeGlmnet(xTrain, yTrain, xTest, Ks,
	dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
        method = "svm", family = "multinomial")
print(mean(svmCT$prediction != yTest))
print(Sys.time() - Sys.time)

Sys.time = Sys.time()
rf = cv.customizeGlmnet(xTrain, yTrain, xTest, 1,
        dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
        method = "randomForest", family = "multinomial")
print(mean(rf$prediction != yTest))
print(Sys.time() - Sys.time)

Sys.time = Sys.time()
rfCT = cv.customizeGlmnet(xTrain, yTrain, xTest, Ks,
        dendrogram = dendrogram, dendrogramCV = dendrogramCV, foldid = foldid,
        method = "randomForest", family = "multinomial")
print(mean(rfCT$prediction != yTest))
print(Sys.time() - Sys.time)

rm(foldid)

save(glmnet, glmnetCT, svm, svmCT, rf, rfCT,
	file = paste('results/dataset-specific/', dataset, '1.rda', sep = ''))
}
