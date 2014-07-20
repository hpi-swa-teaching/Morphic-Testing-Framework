interacting
clickAtAbsolute: aPosition

	|posWrapper|
	posWrapper := self findByPosition: aPosition.
	posWrapper click.