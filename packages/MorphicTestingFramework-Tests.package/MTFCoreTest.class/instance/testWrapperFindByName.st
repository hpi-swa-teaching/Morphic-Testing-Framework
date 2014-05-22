test cases
testWrapperFindByName

	| wrapper |
	wrapper := MTFMorphWrapper newWith: self testForm.
	self assert: ((wrapper findByName: 'button_change_color') contains: self testForm button).