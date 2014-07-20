test cases
testCalculatorTextField

	| display |
	display := self subject findByClass: TextFieldMorph.
	display sendKeys: '-12'.
	self assert: display contents = '-12'.