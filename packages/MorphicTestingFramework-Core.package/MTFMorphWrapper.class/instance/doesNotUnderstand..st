delegation
doesNotUnderstand: aMessage
	"Delegates not implemented message invocations to the first wrapped morph to simplify property assertions"

	^morphs asArray first perform: aMessage selector withArguments: aMessage arguments
