data = read.table('data/Contraceptive Method Choice/cmc.data', sep = ',')
x = model.matrix(~ 0 + . - V10
	- V2 + as.factor(V2)
	- V3 + as.factor(V3)
	- V7 + as.factor(V7)
	- V8 + as.factor(V8)
	, data = as.data.frame(data))
y = data[, 10]
set.seed(2946)
test = sample(1:1473, 736)
