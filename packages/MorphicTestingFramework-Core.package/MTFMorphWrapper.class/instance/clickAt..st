Event creation
clickAt: aPosition

	| eventBuffer |
	eventBuffer := Sensor createMouseEvent.
	eventBuffer
		at: 3 put: aPosition x;
		at: 4 put: aPosition y.
	Sensor queueEvent: eventBuffer.