interacting
sendKeys: aString

	self morphs do: [ :morph |
		aString do: [ :aCharacter | self sendKey: aCharacter]].