data = read.table('data/Balance Scale/balance-scale.data', sep = ',')
x = model.matrix(V1 ~ 0 + .
	, data = as.data.frame(data))
y = data$V1
set.seed(5953)
test = sample(1:625, 312)
