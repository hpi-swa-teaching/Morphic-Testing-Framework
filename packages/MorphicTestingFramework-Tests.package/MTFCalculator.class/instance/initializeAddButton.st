private-initialization-operations
initializeAddButton

	| addButtonMorph |
	addButtonMorph := self generateButtonMorphLabeled: '+'.
	addButtonMorph target: self;
		actionSelector: #handleClickAdd.
	self operationArea addMorphBack: addButtonMorph.