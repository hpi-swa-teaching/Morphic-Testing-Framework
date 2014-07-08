initialization
initialize

	super initialize.
	self
		button: SimpleButtonMorph new;
		textfield1: TextMorph new;
		textfield2: TextMorph new;
		textfield3: TextMorph new;
		panel: Morph new;
		panelButton: SimpleButtonMorph new;
		clickmorph: MTFClickMorph new;
		applyStyle;
		initializeInteractions.
	
	self button name: 'button_change_color'.
	self panelButton name: 'button_in_panel'.