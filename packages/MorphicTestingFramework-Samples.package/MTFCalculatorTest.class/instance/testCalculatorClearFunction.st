test cases
testCalculatorClearFunction

	| display |
	display := self subject findByClass: TextFieldMorph.
	(self subject findByLabel: '5') click.
	(self subject findByLabel: 'C') click.
	self assert: display contents = '0'.
	(self subject findByLabel: '=') click.
	self assert: display contents = '0'.