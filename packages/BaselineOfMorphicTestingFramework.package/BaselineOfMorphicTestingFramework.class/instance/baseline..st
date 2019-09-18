baseline
baseline: spec
	<baseline>
	spec
		for: #'common'
		do: [
			spec
				package: 'MorphicTestingFramework-Core';
				package: 'MorphicTestingFramework-Samples' with:
					[ spec requires: #('Core') ];
				package: 'MorphicTestingFramework-Tests' with: 
					[ spec requires: #('Core') ].
			spec
				group: 'Core' with: #('MorphicTestingFramework-Core');
				group: 'Samples' with: #('MorphicTestingFramework-Samples');
				group: 'Tests' with: #('MorphicTestingFramework-Tests');
				group: 'default' with: #('Core' 'Samples');
				group: 'TravisCI' with: #('Tests') ]