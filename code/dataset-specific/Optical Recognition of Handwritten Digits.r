train = read.table('data/Optical Recognition of Handwritten Digits/optdigits.tra', sep = ',')
test = read.table('data/Optical Recognition of Handwritten Digits/optdigits.tes', sep = ',')
data = rbind(train, test)
x = model.matrix(V65 ~ 0 + . - V1 - V40
	, data = as.data.frame(data))
y = data$V65
set.seed(7131)
test = 3824:5620
