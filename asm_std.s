	ldr	x7, [x7, _camlDune__exe__Indexed_arrays__Pccall_68@GOTPAGEOFF]
	ldr	x8, [x7, #0]
	orr	x9, xzr, #3
	str	x9, [x8, #0]
	adrp	x10, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGE
	ldr	x10, [x10, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGEOFF]
	adrp	x11, _camlDune__exe__Indexed_arrays__Pccall_68@GOTPAGE
	ldr	x11, [x11, _camlDune__exe__Indexed_arrays__Pccall_68@GOTPAGEOFF]
	ldr	x12, [x11, #0]
	ldr	x13, [x12, #-8]
	cmp	x13, #1023
	b.ls	L103
	ldr	x14, [x12, #0]
	str	x14, [x10, #0]
	adrp	x15, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGE
	ldr	x15, [x15, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGEOFF]
	ldr	x1, [x15, #0]
