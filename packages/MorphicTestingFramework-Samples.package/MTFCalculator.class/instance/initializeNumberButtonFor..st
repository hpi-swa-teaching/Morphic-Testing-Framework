private-initialization
initializeNumberButtonFor: aNumber

	self numberArea addMorphBack: (
		self numberButtons at: aNumber put: (self generateNumberButtonMorphFor: aNumber)
	).