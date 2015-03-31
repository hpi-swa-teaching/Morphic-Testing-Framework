test cases
testCalculatorTextField

	| display |
	display := self subject findByClass: TextMorph.
	display sendKey: Character delete asInteger.
	display sendKeys: '-12'.
	self assert: display contents = '-12'.