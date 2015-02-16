data = read.table('data/Musk (Version 1)/clean2.data', sep = ',')
x = model.matrix(V169 ~ 0 + . - V1 - V2
	, data = as.data.frame(data))
y = data[, 169]
set.seed(7764)
test = sample(1:6598, 3299)
