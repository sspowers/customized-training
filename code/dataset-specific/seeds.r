data = read.table('data/seeds/seeds_dataset.txt')
x = as.matrix(data[, 1:7])
y = data[, 8]
set.seed(8462)
test = sample(1:210, 105)
