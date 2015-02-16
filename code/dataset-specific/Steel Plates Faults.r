data = read.table('data/Steel Plates Faults/Faults.NNA')
x = model.matrix(~ 0 + . - V28 - V29 - V30 - V31 - V32 - V33 - V34
	, data = as.data.frame(data))
y = rep(NA, nrow(x))
for (c in 1:7) y[data[, 27 + c] == 1] = c
set.seed(6563)
test = sample(1:1941, 970)
