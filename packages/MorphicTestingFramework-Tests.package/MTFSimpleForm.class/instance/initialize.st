initialization
initialize

	super initialize.
	self
		button: SimpleButtonMorph new;
		textfield: TextMorph new;
		panel: Morph new;
		panelButton: SimpleButtonMorph new;
		applyStyle;
		initializeInteractions.
	
	self button name: 'button_change_color'.
	self panelButton name: 'button_in_panel'.