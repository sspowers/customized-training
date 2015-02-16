data = read.table('data/Parkinsons/parkinsons.data', header = TRUE, sep = ',')
x = model.matrix(status ~ 0 + . - name
	, data = as.data.frame(data))
y = data$status
set.seed(7305)
test = sample(1:195, 97)
