private-initialization-operations
initializeDivideButton

	| divideButtonMorph |
	divideButtonMorph := self generateButtonMorphLabeled: '/'.
	divideButtonMorph target: self;
		actionSelector: #handleClickDivide.
	self operationArea addMorphBack: divideButtonMorph.