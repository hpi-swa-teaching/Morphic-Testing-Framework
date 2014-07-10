initialization
initialize

	super initialize.
	self 
		extent: 150@25;
		"clickAction: [Transcript show: 'click!'. Transcript cr]."
		label: StringMorph new.
	self label contents: 'not pressed'.
	self label position: self position.
	self  addMorph: label.
	