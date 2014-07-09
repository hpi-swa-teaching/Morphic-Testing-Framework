private-initialization
initializeOperationArea

	self operationArea: RectangleMorph new.
	self setupTableLayoutFor: self operationArea.
	self operationArea extent: 100@120;
		color: Color transparent;
		borderWidth: 0.
	self addMorphBack: self operationArea.