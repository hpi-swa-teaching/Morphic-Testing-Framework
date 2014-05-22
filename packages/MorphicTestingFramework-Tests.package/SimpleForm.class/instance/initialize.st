initialization
initialize

	super initialize.
	self
		button: SimpleButtonMorph new;
		textfield: TextMorph new;
		applyStyle;
		initializeInteractions.
	
	self button name: 'button_change_color'.