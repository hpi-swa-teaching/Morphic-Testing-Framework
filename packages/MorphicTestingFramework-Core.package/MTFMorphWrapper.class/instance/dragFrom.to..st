interacting
dragFrom: aStartPosition to: anEndPosition
	
	self morphs do: [ :morph |
		morph
			mouseMove: 
				(self generateMouseEvent: #mouseMove on: morph at: aStartPosition);
			mouseDown: 
				(self generateMouseEvent: #mouseDown on: morph at: aStartPosition);
			mouseMove: 
				(self generateMouseEvent: #mouseMove on: morph at: anEndPosition);
			mouseUp: 
				(self generateMouseEvent: #mouseUp on: morph at: anEndPosition).
	].