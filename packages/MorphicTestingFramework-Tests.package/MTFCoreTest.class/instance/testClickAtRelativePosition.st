interaction tests
testClickAtRelativePosition

	| buttonPosition |
	buttonPosition := 170@15.
	self assert: self subject color = Color white.
	self subject clickAtRelative: buttonPosition.
	self assert: subject color = Color blue.