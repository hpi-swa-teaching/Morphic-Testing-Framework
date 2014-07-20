interaction tests
testClickAtAbsolutePosition

	| buttonPosition |
	buttonPosition := (self subject topLeft) + (170@15).
	self assert: (self subject color = Color white).
	self subject clickAtAbsolute: buttonPosition.
	(Delay forMilliseconds: 1500) wait. 
	self assert: (self subject color = Color blue).