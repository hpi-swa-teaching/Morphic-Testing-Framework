test cases
testCalculatorNumberInput

	| display expectedText |
	expectedText := '0'.
	display := self subject findByClass: TextFieldMorph.
	self assert: display contents = expectedText.
	9 to: 0 do: [:number |
		expectedText := expectedText, number asString.
		self subject findByLabel: (number asString) click.
		self assert: display contents = expectedText.
	].