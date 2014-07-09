private-initialization-operations
initializeMultiplyButton

	| multiplyButtonMorph |
	multiplyButtonMorph := self generateButtonMorphLabeled: '*'.
	multiplyButtonMorph target: self;
		actionSelector: #handleClickMultiply.
	self operationArea addMorphBack: multiplyButtonMorph.