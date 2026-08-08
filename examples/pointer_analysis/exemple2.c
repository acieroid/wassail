int foo(int part1Value, int part2Value, int n) {
	int *part1, *part2;
	int a[10],*p_array0;
	int i;
	part1=&a[0];
	p_array0=part1;
	part2=&a[5];
	for(i=0;i<n;++i) {
		*part1=part1Value;
		*part2=part2Value;
		part1++;
		part2++;
	}
	return *p_array0;
}
