data = read.csv('data/QSAR biodegradation/biodeg.csv', header = FALSE, sep = ';')
x = model.matrix(V42 ~ 0 + .
	, data = as.data.frame(data))
y = data$V42
set.seed(7507)
test = sample(1:1055, 527)
