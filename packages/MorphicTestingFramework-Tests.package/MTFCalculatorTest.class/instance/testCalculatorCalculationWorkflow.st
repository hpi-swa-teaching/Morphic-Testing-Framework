test cases
testCalculatorCalculationWorkflow

	| display |
	display := self subject findByClass: TextFieldMorph.
	self assert: display contents = '0'.
	(self subject findByLabel: '5') click.
	self assert: display contents = '5'.
	(self subject findByLabel: '+') click.
	(self subject findByLabel: '7') click.
	self assert: display contents = '7'.
	(self subject findByLabel: '=') click.
	self assert: display contents = '12'.