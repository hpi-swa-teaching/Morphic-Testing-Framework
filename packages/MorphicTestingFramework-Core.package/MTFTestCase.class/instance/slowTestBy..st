running
slowTestBy: aNumber

	self delay: aNumber.
	self timeout: 60.
	self subject 
		openInWorld;
		redraw;
		delay: aNumber.