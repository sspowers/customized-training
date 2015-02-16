data = read.table('data/Breast Cancer Wisconsin (Diagnostic)/wdbc.data', sep = ',')
x = model.matrix(~ 0 + . - V1 - V2
	, data = as.data.frame(data))
y = data[, 2]
set.seed(5523)
test = sample(1:569, 284)
