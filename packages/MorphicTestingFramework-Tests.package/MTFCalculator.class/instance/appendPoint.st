private-calculation
appendPoint

	self currentInputBecomesFraction ifFalse: [
		self append: '.'.
	]..