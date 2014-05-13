interacting
click
	
	self morph 
		mouseDown: (self generateMouseEvent: #mouseDown);
		mouseUp: (self generateMouseEvent: #mouseUp)