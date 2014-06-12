private-initialization-operations
initializeEvalButton

	| evalButtonMorph |
	evalButtonMorph := self generateButtonMorphLabeled: '='.
	evalButtonMorph target: self;
		actionSelector: #handleClickEval.
	self operationArea addMorphBack: evalButtonMorph.