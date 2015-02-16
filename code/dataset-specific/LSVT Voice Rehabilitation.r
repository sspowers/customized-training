data = read.csv('data/LSVT Voice Rehabilitation/LSVT_voice_rehabilitation.csv')
x = model.matrix(~ 0 + .
	, data = as.data.frame(data))
y = read.csv('data/LSVT Voice Rehabilitation/LSVT_voice_rehabilitation_class.csv')[, 1]
set.seed(7620)
test = sample(1:126, 63)
