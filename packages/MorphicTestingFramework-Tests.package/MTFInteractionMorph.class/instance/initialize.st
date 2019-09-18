initialization
initialize

	super initialize.
	self 
		extent: 150@25;
		label: StringMorph new.
	self label 
		contents: 'not pressed';
		position: self position.
	self addMorph: label.