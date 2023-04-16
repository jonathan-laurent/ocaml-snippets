	.file	""
	.data
	.globl	_camlDune__exe__Asm_alias__data_begin
_camlDune__exe__Asm_alias__data_begin:
	.text
	.globl	_camlDune__exe__Asm_alias__code_begin
_camlDune__exe__Asm_alias__code_begin:
	.data
	.align 3
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_alias__Pccall_91
	.globl	_camlDune__exe__Asm_alias__Pccall_91
_camlDune__exe__Asm_alias__Pccall_91:
	.quad	1
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_alias__Parrayrefs_44
	.globl	_camlDune__exe__Asm_alias__Parrayrefs_44
_camlDune__exe__Asm_alias__Parrayrefs_44:
	.quad	1
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_alias__Pccall_46
	.globl	_camlDune__exe__Asm_alias__Pccall_46
_camlDune__exe__Asm_alias__Pccall_46:
	.quad	1
	.data
	.align 3
	.globl	_camlDune__exe__Asm_alias__gc_roots
	.globl	_camlDune__exe__Asm_alias__gc_roots
_camlDune__exe__Asm_alias__gc_roots:
	.quad	_camlDune__exe__Asm_alias__Pccall_46
	.quad	_camlDune__exe__Asm_alias__Parrayrefs_44
	.quad	_camlDune__exe__Asm_alias__Pccall_91
	.quad	0
	.data
	.align 3
	.quad	768
	.globl	_camlDune__exe__Asm_alias
	.globl	_camlDune__exe__Asm_alias
_camlDune__exe__Asm_alias:
	.text
	.align	3
	.globl	_camlDune__exe__Asm_alias__entry
_camlDune__exe__Asm_alias__entry:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
L102:
	orr	x1, xzr, #1
	orr	x0, xzr, #3
	adrp	x8, _caml_make_vect@GOTPAGE
	ldr	x8, [x8, _caml_make_vect@GOTPAGEOFF]
	bl	_caml_c_call
L103:
	adrp	x3, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGE
	ldr	x3, [x3, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGEOFF]
	str	x0, [x3, #0]
	adrp	x4, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGE
	ldr	x4, [x4, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGEOFF]
	ldr	x0, [x4, #0]
	ldrb	w6, [x0, #-8]
	cmp	x6, #254
	b.eq	L101
	orr	x1, xzr, #3
	bl	_caml_modify
	b	L100
L101:
	orr	x8, xzr, #3
	ldr	d0, [x8, #0]
	str	d0, [x0, #0]
L100:
	adrp	x9, _camlDune__exe__Asm_alias__Parrayrefs_44@GOTPAGE
	ldr	x9, [x9, _camlDune__exe__Asm_alias__Parrayrefs_44@GOTPAGEOFF]
	adrp	x10, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGE
	ldr	x10, [x10, _camlDune__exe__Asm_alias__Pccall_46@GOTPAGEOFF]
	ldr	x11, [x10, #0]
	ldr	x12, [x11, #-8]
	cmp	x12, #1023
	b.ls	L104
	ldr	x13, [x11, #0]
	str	x13, [x9, #0]
	adrp	x14, _camlDune__exe__Asm_alias__Parrayrefs_44@GOTPAGE
	ldr	x14, [x14, _camlDune__exe__Asm_alias__Parrayrefs_44@GOTPAGEOFF]
	ldr	x1, [x14, #0]
	adrp	x0, _camlStdlib__const_immstring_542@GOTPAGE
	ldr	x0, [x0, _camlStdlib__const_immstring_542@GOTPAGEOFF]
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L106:
	adrp	x21, _camlDune__exe__Asm_alias__Pccall_91@GOTPAGE
	ldr	x21, [x21, _camlDune__exe__Asm_alias__Pccall_91@GOTPAGEOFF]
	str	x0, [x21, #0]
	adrp	x22, _camlDune__exe__Asm_alias__Pccall_91@GOTPAGE
	ldr	x22, [x22, _camlDune__exe__Asm_alias__Pccall_91@GOTPAGEOFF]
	ldr	x1, [x22, #0]
	adrp	x24, _camlStdlib__Pccall_1851@GOTPAGE
	ldr	x24, [x24, _camlStdlib__Pccall_1851@GOTPAGEOFF]
	ldr	x0, [x24, #0]
	bl	_camlStdlib__output_string_763
L107:
	orr	x0, xzr, #1
	ldr	x30, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset	-16
	ret
	.cfi_adjust_cfa_offset	16
L104:	bl	_caml_ml_array_bound_error
L105:
	.cfi_endproc
	.data
	.align 3
	.text
	.globl	_camlDune__exe__Asm_alias__code_end
_camlDune__exe__Asm_alias__code_end:
	.data
	.quad	0
	.globl	_camlDune__exe__Asm_alias__data_end
_camlDune__exe__Asm_alias__data_end:
	.quad	0
	.align	3
	.globl	_camlDune__exe__Asm_alias__frametable
_camlDune__exe__Asm_alias__frametable:
	.quad	4
	.quad	L107
	.short	17
	.short	0
	.align	2
	.long	L108 - . + 0x0
	.align	3
	.quad	L106
	.short	17
	.short	0
	.align	2
	.long	L109 - . + 0x0
	.align	3
	.quad	L105
	.short	17
	.short	0
	.align	2
	.long	L110 - . + 0x0
	.align	3
	.quad	L103
	.short	17
	.short	0
	.align	2
	.long	L111 - . + 0x0
	.align	3
	.align	2
L108:
	.long	L113 - . + 0xe0000001
	.long	0x1e9120
	.long	L115 - . + 0x4c000000
	.long	0x6020
	.align	2
L109:
	.long	L116 - . + 0x4c000001
	.long	0x10c020
	.long	L113 - . + 0xe0000001
	.long	0x1e9270
	.long	L115 - . + 0x4c000000
	.long	0x6020
	.align	2
L111:
	.long	L115 - . + 0x68000000
	.long	0x40c0
	.align	2
L110:
	.long	L115 - . + 0x4c000000
	.long	0x60c0
L112:
	.asciz	"stdlib.ml"
L114:
	.asciz	"asm_alias.ml"
	.align	2
L116:
	.long	L112 - . + 0x0
	.asciz	"Stdlib.string_of_int"
	.align	2
L115:
	.long	L114 - . + 0x0
	.asciz	"Dune__exe__Asm_alias.main"
	.align	2
L113:
	.long	L112 - . + 0x0
	.asciz	"Stdlib.print_int"
	.align	3
