data = read.table('data/Chess (King-Rook vs King-Pawn)/kr-vs-kp.data', sep = ',')
x = model.matrix(V37 ~ 0 + .
	, data = as.data.frame(data))
y = data$V37
set.seed(6354)
test = sample(1:3196, 1598)
