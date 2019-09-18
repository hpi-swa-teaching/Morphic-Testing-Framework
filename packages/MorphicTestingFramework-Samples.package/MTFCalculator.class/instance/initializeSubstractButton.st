private-initialization-operations
initializeSubstractButton

	| subtractButtonMorph |
	subtractButtonMorph := self generateButtonMorphLabeled: '-'.
	subtractButtonMorph target: self;
		actionSelector: #handleClickSubtract.
	self operationArea addMorphBack: subtractButtonMorph.