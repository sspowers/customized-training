train = read.table('data/User Knowledge Modeling/ukm.tra', header = TRUE, sep = '\t')
test = read.table('data/User Knowledge Modeling/ukm.tes', header = TRUE, sep = '\t')
data = rbind(train, test)
data$UNS[data$UNS == 'very_low'] = 'Very Low'
data$UNS = as.factor(as.character(data$UNS))
x = model.matrix(UNS ~ 0 + .
	, data = as.data.frame(data))
y = data$UNS
set.seed(1601)
test = 259:403
