test cases
testWrapperFindByName

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: ((wrapper findByName: 'button') contains: self testForm button).