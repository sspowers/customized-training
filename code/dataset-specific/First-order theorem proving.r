train = read.csv('data/First-order theorem proving/train.csv', header = F)
validation = read.csv('data/First-order theorem proving/validation.csv', header = F)
test = read.csv('data/First-order theorem proving/test.csv', header = F)
data = rbind(train, validation, test)
x = model.matrix(~ 0 + . - V52 - V53 - V54 - V55 - V56 - V57
	, data = as.data.frame(data))
y = rep(NA, nrow(x))
for (c in 1:6) y[data[, 51 + c] == 1] = c
set.seed(9814)
test = 3060:6118
