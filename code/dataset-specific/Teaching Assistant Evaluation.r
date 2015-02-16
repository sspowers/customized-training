data = read.table('data/Teaching Assistant Evaluation/tae.data', sep = ',')
x = model.matrix(V6 ~ 0 + .
	- V2 + as.factor(V2)
	- V3 + as.factor(V3)
	, data = as.data.frame(data))
y = data$V6
set.seed(2977)
test = sample(1:151, 75)
