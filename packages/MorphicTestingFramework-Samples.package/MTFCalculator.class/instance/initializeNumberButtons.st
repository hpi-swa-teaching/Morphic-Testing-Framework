private-initialization
initializeNumberButtons

	self numberButtons: Dictionary new.
	self initializeNumberArea.
	0 to: 9 do: [ :number |
		self initializeNumberButtonFor: 9 - number.
	].
	(self numberArea submorphNamed: '0') width: 100.
	self numberArea addMorphBack: (
		(self generateButtonMorphLabeled: '.')
			target: self;
			actionSelector: 'handleClickPoint'
	).