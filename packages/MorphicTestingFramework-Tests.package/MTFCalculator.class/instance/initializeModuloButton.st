private-initialization-operations
initializeModuloButton

	| moduloButtonMorph |
	moduloButtonMorph := self generateButtonMorphLabeled: 'mod'.
	moduloButtonMorph target: self;
		actionSelector: #handleClickModulo.
	self operationArea addMorphBack: moduloButtonMorph.