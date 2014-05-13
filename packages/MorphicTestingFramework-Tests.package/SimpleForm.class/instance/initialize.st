initialization
initialize

	super initialize.
	self
		button: SimpleButtonMorph new;
		textfield: TextMorph new;
		applyStyle;
		initializeInteractions