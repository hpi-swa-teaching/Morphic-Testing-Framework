private-initialization-operations
initializeClearButton

	| clearButtonMorph |
	clearButtonMorph := self generateButtonMorphLabeled: 'C'.
	clearButtonMorph target: self;
		actionSelector: #handleClickClear.
	self operationArea addMorphBack: clearButtonMorph.