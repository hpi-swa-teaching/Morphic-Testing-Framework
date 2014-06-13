waiting
wait
	
	self redraw.
	delay > 0 ifTrue:
		[(Delay forMilliseconds: delay) wait.]