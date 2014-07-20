interacting
clickAtRelative: aPosition

	self morphs do: [ :morph| 
		self clickAtAbsolute: morph position + aPosition].