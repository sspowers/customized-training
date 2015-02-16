data = read.table('data/Mushroom/agaricus-lepiota.data', sep = ',')
x = model.matrix(V1 ~ 0 + .
	, data = as.data.frame(data[, -17]))
y = data[, 1]
set.seed(3910)
test = sample(1:8124, 4062)
