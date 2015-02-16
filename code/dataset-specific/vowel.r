train = read.csv('data/vowel/vowel.train')
test = read.csv('data/vowel/vowel.test')
data = rbind(train, test)
x = model.matrix(y ~ 0 + . - row.names
	, data = data)
y = data$y
set.seed(5616)
test = 529:990
numFolds = 8
permutation = 1:528