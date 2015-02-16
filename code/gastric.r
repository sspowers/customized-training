require(xtable)
source('code/customizeTraining.r')

xTrain = matrix(scan('data/massspec/x.txt'), ncol = 2220, byrow = T)
yTrain = scan('data/massspec/y.txt')
patientTrain = scan('data/massspec/pattr.txt')
xTest = matrix(scan('data/massspec/xte.txt'), ncol = 2220, byrow = T)
yTest = scan('data/massspec/yte.txt')
patientTest = scan('data/massspec/patte.txt')
foldid = scan('data/massspec/foldid.txt')

patient = unique(patientTest)
yTrain[yTrain == 3] = 1
yTest[yTest == 3] = 1
n = nrow(xTrain)
p = ncol(xTrain)
m = nrow(xTest)

fitLasso = cv.glmnet(xTrain, yTrain, foldid = foldid, family = 'binomial', type.measure = 'class')
predLasso = predict(fitLasso, xTest, type = 'response', s = 'lambda.min')
table(yTest, 1 + (predLasso > 1/3))

fitCT = cv.customizeGlmnet(xTrain, yTrain, xTest, patientTest,
    foldid = foldid, family = 'binomial')
predCT = fitCT$prediction
table(yTest, 1 + (predCT > 1/3))


CTsetTable = matrix(NA, length(unique(patientTrain)),
    length(unique(patientTest)))
colnames(CTsetTable) = sort(unique(patientTest))
rownames(CTsetTable) = sort(unique(patientTrain))
for (group in as.character(sort(unique(patientTest)))) {
    for (patient in as.character(sort(unique(patientTrain)))) {
        CTsetTable[patient, group] =
            sum(patientTrain[fitCT$fit$CTset[[group]]] == patient)
    }
}
CTsetTable = t(t(CTsetTable)/colSums(CTsetTable))
CTsetTable = CTsetTable[, c(5, 6, 1, 4, 3, 2)]
CTsetTable = CTsetTable[c(8, 2, 1, 9, 13, 6, 5, 12, 14, 11, 7, 3, 4, 10), ]
rownames(CTsetTable) = 1:14
colnames(CTsetTable) = 1:6

table = xtable(CTsetTable, digits = 3)
file = file('tabs/gastric.tex')
writeLines(print(table, only.contents = TRUE,
    sanitize.text.function = identity, print.results = FALSE), file)
close(file)

