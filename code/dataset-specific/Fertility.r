data = read.table('data/Fertility/fertility_Diagnosis.txt', sep = ',')
x = model.matrix(V10 ~ 0 + .
	, data = as.data.frame(data))
y = data[, 10]
set.seed(7057)
test = sample(1:100, 50)
