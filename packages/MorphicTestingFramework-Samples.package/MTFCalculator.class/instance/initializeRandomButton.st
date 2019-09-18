private-initialization-operations
initializeRandomButton

	| randomButtonMorph |
	randomButtonMorph := self generateButtonMorphLabeled: 'rnd'.
	randomButtonMorph target: self;
		actionSelector: #handleClickRandom.
	self operationArea addMorphBack: randomButtonMorph.