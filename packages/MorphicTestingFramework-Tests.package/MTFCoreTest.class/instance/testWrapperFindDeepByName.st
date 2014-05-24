test cases
testWrapperFindDeepByName

	| wrapper |
	wrapper := MTFMorphWrapper newWith: {self testForm}.
	self assert: ((wrapper findByName: 'button_in_panel') contains: self testForm panelButton).