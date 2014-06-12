running
slowTestBy: aNumber

	self subject openInWorld.
	self subject redraw.
	self delay: aNumber.
	self subject delay: aNumber.