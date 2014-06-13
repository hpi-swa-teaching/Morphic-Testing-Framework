search tests
testWrapperFindByLabel

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self subject}.
	self assert: ((wrapper findByLabel: 'Flash') contains: self subject button).