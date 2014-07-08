event handling
mouseDown: evt

	evt anyModifierKeyPressed
		ifFalse: [self label contents: 'no modifiers'].
	evt shiftPressed 
		ifTrue: [self label contents: 'shift pressed'].
	evt controlKeyPressed
		ifTrue: [self label contents: 'control pressed'].
	evt commandKeyPressed
		ifTrue: [self label contents: 'command pressed'].