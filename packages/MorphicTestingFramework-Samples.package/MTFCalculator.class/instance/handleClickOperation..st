event handling
handleClickOperation: aBlock

	self operation ifNil: [self swapInput.].
	self operation: aBlock.