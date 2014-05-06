baseline
baseline: spec
	<baseline>
	spec
		for: #'common'
		do: [
			spec
				package: 'MorphicTestingFramework-Core';
				package: 'MorphicTestingFramework-Tests' with: 
					[ spec requires: #('default') ].
			spec
				group: 'default' with: #('MorphicTestingFramework-Core');
				group: 'Tests' with: #('MorphicTestingFramework-Tests');
				group: 'TravisCI' with: #('Tests') ]