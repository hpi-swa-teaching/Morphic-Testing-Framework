private-event-handling
sendFocusEvent: anEvent to: focusHolder clear: aBlock
	"Aend the event to the morph currently holding the focus"

	| result w |	
	w := ActiveWorld.
	w becomeActiveDuring:[
		ActiveHand := self.
		ActiveEvent := anEvent.
		result := focusHolder handleFocusEvent: 
			(anEvent transformedBy: (focusHolder transformedFrom: self))].
		
	^ result