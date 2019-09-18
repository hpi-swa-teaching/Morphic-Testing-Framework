test cases
testCalculatorAddition

	| display |
	display := self subject findByClass: TextFieldMorph.
	(self subject findByLabel: '3') click.
	(self subject findByLabel: '+') click.
	(self subject findByLabel: '4') click.
	(self subject findByLabel: '=') click.
	self assert: display contents = '7'.