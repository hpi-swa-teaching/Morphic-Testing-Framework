private-initialization
initializeInput
	
	self inputMorph:
		(TextFieldMorph new
			contents: '0';
			extent: 254@50;
			color: Color white
		).
	self addMorph: self inputMorph.