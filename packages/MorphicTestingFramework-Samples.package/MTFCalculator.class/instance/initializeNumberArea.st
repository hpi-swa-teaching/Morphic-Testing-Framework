private-initialization
initializeNumberArea

	self numberArea: RectangleMorph new.
	self numberArea
		extent: 154@90;
		color: Color transparent;
		borderWidth: 0.
	self setupTableLayoutFor: self numberArea.
	self addMorphBack: self numberArea.