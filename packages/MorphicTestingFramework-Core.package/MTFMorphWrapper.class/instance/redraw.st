interacting
redraw
	
	self morphs do: [:each| each invalidRect: self fullBounds	].

	
