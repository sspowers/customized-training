dataSets = c('Balance Scale', 'Breast Cancer Wisconsin (Diagnostic)',
		'Chess (King-Rook vs King-Pawn)', 'Contraceptive Method Choice',
		'Fertility', 'First-order theorem proving', 'LSVT Voice Rehabilitation',
		'Mushroom', 'Musk (Version 1)', 'Optical Recognition of Handwritten Digits',
		'Parkinsons', 'QSAR Biodegradation', 'seeds', 'Steel Plates Faults',
		'Teaching Assistant Evaluation', 'User Knowledge Modeling', 'vowel')

errorTable = matrix(NA, length(dataSets), 11)

for (i in 1:length(dataSets)) {
	load(paste('results/', dataSets[i], '.Rdata', sep = ''))
	for (k in 1:5) {
		index = which.min(CT1$error[k, ])
		if (length(index) == 1) errorTable[i, k] = CT1$error[k, index]
	}
	for (k in 1:5) {
		index = which.min(CT2$error[k, ])
		if (length(index) == 1) errorTable[i, k + 5] = CT2$error[k, index]
	}
	index = which.min(KNN$error)
	errorTable[i, 11] = KNN$error[index]
}