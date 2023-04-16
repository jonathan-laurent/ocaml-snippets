	ldr	x0, [x7, #0]
	ldrb	w9, [x0, #-8]
	cmp	x9, #254
	b.eq	L101
	orr	x1, xzr, #3
	bl	_caml_modify
	b	L100
L101:
	orr	x11, xzr, #3
	ldr	d0, [x11, #0]
	str	d0, [x0, #0]
L100:
	adrp	x12, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGE
	ldr	x12, [x12, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGEOFF]
	adrp	x13, _camlDune__exe__Indexed_arrays__Pccall_68@GOTPAGE
	ldr	x13, [x13, _camlDune__exe__Indexed_arrays__Pccall_68@GOTPAGEOFF]
	ldr	x14, [x13, #0]
	ldr	x15, [x14, #-8]
	cmp	x15, #1023
	b.ls	L105
	ldr	x19, [x14, #0]
	str	x19, [x12, #0]
	adrp	x20, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGE
	ldr	x20, [x20, _camlDune__exe__Indexed_arrays__Parrayrefs_66@GOTPAGEOFF]
	ldr	x1, [x20, #0]