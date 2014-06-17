waiting
wait: aFactor
	
	self redraw.
	delay > 0 ifTrue:
		[(Delay forMilliseconds: delay * aFactor) wait.]