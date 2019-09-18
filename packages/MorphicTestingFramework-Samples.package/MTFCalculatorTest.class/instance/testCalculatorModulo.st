test cases
testCalculatorModulo

	| display |
	display := self subject findByClass: TextFieldMorph.
	(self subject findByLabel: '2') click.
	(self subject findByLabel: '2') click.
	(self subject findByLabel: 'mod') click.
	(self subject findByLabel: '1') click.
	(self subject findByLabel: '5') click.
	(self subject findByLabel: '=') click.
	self assert: display contents = '7'.