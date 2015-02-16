require(xtable)

datasets = c('BalanceScale', 'BreastCancerWisconsin(Diagnostic)',
		'Chess(King-RookvsKing-Pawn)', 'ContraceptiveMethodChoice',
		'Fertility', 'LSVTVoiceRehabilitation', 'Mushroom', 
		'OpticalRecognitionofHandwrittenDigits', 'Parkinsons',
		'QSARbiodegradation', 'seeds', 'SteelPlatesFaults',
		'TeachingAssistantEvaluation', 'UserKnowledgeModeling',
		'vowel')
directory = 'http://statweb.stanford.edu/~sspowers/battery/'

table = matrix(NA, length(datasets), 16)
rownames(table) = c('BS', 'BCW', 'C', 'CMC', 'F', 'LSVT', 'M', 'ORHD', 'P',
	'Q', 'S', 'SPF', 'TAE', 'UKM', 'V') 

for (i in 1:length(datasets)) {

	dataset = datasets[i]
	source(paste(directory, 'code/', dataset, '.r', sep = ''))
	source(paste(directory, 'code/formatting.r', sep = ''))
	load(paste('results/dataset-specific/', dataset, '.rda', sep = ''))
	load(paste('results/dataset-specific/', dataset, '.Rdata', sep = ''))

	# n
	table[i, 1] = nrow(x) - length(test)

	# p
	table[i, 2] = ncol(x)

	# glmnet
	glmnetError = mean(glmnet$prediction != yTest, na.rm = TRUE)
	table[i, 3] = substr(glmnetError, 2, 5)
	if (glmnetError == 0)	table[i, 3] = '.000'

	# glmnetCT
	glmnetCTerror = mean(glmnetCT$prediction != yTest, na.rm = TRUE)
	table[i, 4] = substr(glmnetCTerror, 2, 5)
	if (glmnetCTerror == 0)	table[i, 4] = '.000'
	table[i, 5] = glmnetCT$K.min

	# CTt
	CT2error = mean(CT2$prediction != yTest, na.rm = TRUE)
	table[i, 6] = substr(CT2error, 2, 5)
	if (CT2error == 0) table[i, 6] = '.000'
	table[i, 7] = CT2$CV$K

	# svm
	svmError = mean(svm$prediction != yTest, na.rm = TRUE)
	table[i, 8] = substr(svmError, 2, 5)
	if (svmError == 0)	table[i, 8] = '.000'

	# svmCT
	svmCTerror = mean(svmCT$prediction != yTest, na.rm = TRUE)
	table[i, 9] = substr(svmCTerror, 2, 5)
	if (svmCTerror == 0)	table[i, 9] = '.000'
	table[i, 10] = svmCT$K.min

	# rf
	rfError = mean(rf$prediction != yTest, na.rm = TRUE)
	table[i, 11] = substr(rfError, 2, 5)
	if (rfError == 0)	table[i, 11] = '.000'

	# rfCT
	rfCTerror = mean(rfCT$prediction != yTest, na.rm = TRUE)
	table[i, 12] = substr(rfCTerror, 2, 5)
	if (rfCTerror == 0)	table[i, 12] = '.000'
	table[i, 13] = rfCT$K.min

	# kNN
	kNNerror = mean(KNN$prediction != yTest, na.rm = TRUE)
	table[i, 14] = substr(kNNerror, 2, 5)
	if (kNNerror == 0)	table[i, 14] = '.000'
	table[i, 15] = KNN$CV$k

	# Percent improvement
	PI = (glmnetError - glmnetCTerror)/max(glmnetError, 1e-9)
	if (PI > 0) {
		table[i, 16] = paste(substr(100*PI, 1, 3), '\\%', sep = '')
	} else if (PI >= 0.1) {
		table[i, 16] = paste(substr(100*PI, 1, 4), '\\%', sep = '')
	} else if (PI == 0) {
		table[i, 16] = '---'
	} else if (PI < 0) {
		table[i, 16] = paste(substr(100*PI, 1, 4), '\\%', sep = '')
	} else if (PI < -0.1) {
		table[i, 16] = paste(substr(100*PI, 1, 5), '\\%', sep = '')
	}

	for (j in c(3, 4, 6, 8, 9, 11, 12, 14)) {
		if (nchar(table[i, j]) == 3) {
			table[i, j] = paste(table[i, j], '0', sep = '')
		}
	}

	best = min(glmnetError, glmnetCTerror, CT2error, svmError, svmCTerror, rfError,
		rfCTerror, kNNerror)
	if (glmnetError == best) {
		table[i, 3] = paste('{\\bf ', table[i, 3], '}', sep='')
	}
	if (glmnetCTerror == best) {
		table[i, 4] = paste('{\\bf ', table[i, 4], '}', sep='')
	}
	if (CT2error == best) {
		table[i, 6] = paste('{\\bf ', table[i, 6], '}', sep='')
	}
	if (svmError == best) {
		table[i, 8] = paste('{\\bf ', table[i, 8], '}', sep='')
	}
	if (svmCTerror == best) {
		table[i, 9] = paste('{\\bf ', table[i, 9], '}', sep='')
	}
	if (rfError == best) {
		table[i, 11] = paste('{\\bf ', table[i, 11], '}', sep='')
	}
	if (rfCTerror == best) {
		table[i, 12] = paste('{\\bf ', table[i, 12], '}', sep='')
	}
	if (kNNerror == best) {
		table[i, 14] = paste('{\\bf ', table[i, 14], '}', sep='')
	}
}

xtable = xtable(table, label = "tab-battery",
	align = 'l|rr|r|rr|rr|r|rr|r|rr|rr|r',
	caption = 'Test error of customized training on 14 benchmark data sets')
file = file("tables/battery.tex")
writeLines(print(xtable, sanitize.text.function = identity), file)
close(file)
