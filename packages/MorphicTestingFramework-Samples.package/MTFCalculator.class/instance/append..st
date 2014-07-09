private-calculation
append: aString

	(self currentInputFinal or: [self currentInput = '0']) ifTrue: [
		self currentInput: aString.
	] ifFalse: [
		self currentInput: (self currentInput , aString).
	].
	self currentInputFinal: false.