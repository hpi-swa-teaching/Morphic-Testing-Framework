test cases
testCalculatorSubtraction

	| display |
	display := self subject findByClass: TextFieldMorph.
	(self subject findByLabel: '5') click.
	(self subject findByLabel: '-') click.
	(self subject findByLabel: '7') click.
	(self subject findByLabel: '=') click.
	self assert: display contents = '-2'.