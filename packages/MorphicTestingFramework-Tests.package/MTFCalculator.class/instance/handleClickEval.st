event handling
handleClickEval

	self operation ifNotNil: [
		self currentInputFromNumber: (self operation value: self lastFloatInput value: self currentFloatInput).
		self currentInputFinal: true.
		self operation: nil.
	].