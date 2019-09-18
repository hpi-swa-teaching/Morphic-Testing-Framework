event handling
handleClickEval

	self operation ifNotNil: [
		self currentInputFromNumber: 
			(self operation value: self lastFloatInput value: self currentFloatInput);
			currentInputFinal: true;
			operation: nil.
	].