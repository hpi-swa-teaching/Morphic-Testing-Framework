interacting
mouseDownForButton: aButtonIdentifier with: modifiers
	
	self sendMouseEvent:  #mouseDown button:  aButtonIdentifier modifiers: modifiers.