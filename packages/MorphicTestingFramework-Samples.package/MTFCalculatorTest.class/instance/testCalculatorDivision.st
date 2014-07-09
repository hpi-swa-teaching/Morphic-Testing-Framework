test cases
testCalculatorDivision

	| display |
	display := self subject findByClass: TextFieldMorph.
	(self subject findByLabel: '4') click.
	(self subject findByLabel: '2') click.
	(self subject findByLabel: '/') click.
	(self subject findByLabel: '1') click.
	(self subject findByLabel: '3') click.
	(self subject findByLabel: '3') click.
	(self subject findByLabel: '7') click.
	(self subject findByLabel: '=') click.
	self assert: display contents = '0.031413612565445025'.