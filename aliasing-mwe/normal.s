	.file	""
	.data
	.globl	_camlDune__exe__Asm_normal__data_begin
_camlDune__exe__Asm_normal__data_begin:
	.text
	.globl	_camlDune__exe__Asm_normal__code_begin
_camlDune__exe__Asm_normal__code_begin:
	.data
	.align 3
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_normal__Pccall_69
	.globl	_camlDune__exe__Asm_normal__Pccall_69
_camlDune__exe__Asm_normal__Pccall_69:
	.quad	1
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_normal__Parrayrefs_34
	.globl	_camlDune__exe__Asm_normal__Parrayrefs_34
_camlDune__exe__Asm_normal__Parrayrefs_34:
	.quad	1
	.data
	.align 3
	.quad	1792
	.globl	_camlDune__exe__Asm_normal__Pccall_36
	.globl	_camlDune__exe__Asm_normal__Pccall_36
_camlDune__exe__Asm_normal__Pccall_36:
	.quad	1
	.data
	.align 3
	.globl	_camlDune__exe__Asm_normal__gc_roots
	.globl	_camlDune__exe__Asm_normal__gc_roots
_camlDune__exe__Asm_normal__gc_roots:
	.quad	_camlDune__exe__Asm_normal__Pccall_36
	.quad	_camlDune__exe__Asm_normal__Parrayrefs_34
	.quad	_camlDune__exe__Asm_normal__Pccall_69
	.quad	0
	.data
	.align 3
	.quad	768
	.globl	_camlDune__exe__Asm_normal
	.globl	_camlDune__exe__Asm_normal
_camlDune__exe__Asm_normal:
	.text
	.align	3
	.globl	_camlDune__exe__Asm_normal__entry
_camlDune__exe__Asm_normal__entry:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.cfi_offset 30, -8
	str	x30, [sp, #8]
L100:
	orr	x1, xzr, #1
	orr	x0, xzr, #3
	adrp	x8, _caml_make_vect@GOTPAGE
	ldr	x8, [x8, _caml_make_vect@GOTPAGEOFF]
	bl	_caml_c_call
L101:
	adrp	x3, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGE
	ldr	x3, [x3, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGEOFF]
	str	x0, [x3, #0]
	adrp	x4, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGE
	ldr	x4, [x4, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGEOFF]
	ldr	x5, [x4, #0]
	orr	x6, xzr, #3
	str	x6, [x5, #0]
	adrp	x7, _camlDune__exe__Asm_normal__Parrayrefs_34@GOTPAGE
	ldr	x7, [x7, _camlDune__exe__Asm_normal__Parrayrefs_34@GOTPAGEOFF]
	adrp	x8, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGE
	ldr	x8, [x8, _camlDune__exe__Asm_normal__Pccall_36@GOTPAGEOFF]
	ldr	x9, [x8, #0]
	ldr	x10, [x9, #-8]
	cmp	x10, #1023
	b.ls	L102
	ldr	x11, [x9, #0]
	str	x11, [x7, #0]
	adrp	x12, _camlDune__exe__Asm_normal__Parrayrefs_34@GOTPAGE
	ldr	x12, [x12, _camlDune__exe__Asm_normal__Parrayrefs_34@GOTPAGEOFF]
	ldr	x1, [x12, #0]
	adrp	x0, _camlStdlib__const_immstring_542@GOTPAGE
	ldr	x0, [x0, _camlStdlib__const_immstring_542@GOTPAGEOFF]
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L104:
	adrp	x19, _camlDune__exe__Asm_normal__Pccall_69@GOTPAGE
	ldr	x19, [x19, _camlDune__exe__Asm_normal__Pccall_69@GOTPAGEOFF]
	str	x0, [x19, #0]
	adrp	x20, _camlDune__exe__Asm_normal__Pccall_69@GOTPAGE
	ldr	x20, [x20, _camlDune__exe__Asm_normal__Pccall_69@GOTPAGEOFF]
	ldr	x1, [x20, #0]
	adrp	x22, _camlStdlib__Pccall_1851@GOTPAGE
	ldr	x22, [x22, _camlStdlib__Pccall_1851@GOTPAGEOFF]
	ldr	x0, [x22, #0]
	bl	_camlStdlib__output_string_763
L105:
	orr	x0, xzr, #1
	ldr	x30, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset	-16
	ret
	.cfi_adjust_cfa_offset	16
L102:	bl	_caml_ml_array_bound_error
L103:
	.cfi_endproc
	.data
	.align 3
	.text
	.globl	_camlDune__exe__Asm_normal__code_end
_camlDune__exe__Asm_normal__code_end:
	.data
	.quad	0
	.globl	_camlDune__exe__Asm_normal__data_end
_camlDune__exe__Asm_normal__data_end:
	.quad	0
	.align	3
	.globl	_camlDune__exe__Asm_normal__frametable
_camlDune__exe__Asm_normal__frametable:
	.quad	4
	.quad	L105
	.short	17
	.short	0
	.align	2
	.long	L106 - . + 0x0
	.align	3
	.quad	L104
	.short	17
	.short	0
	.align	2
	.long	L107 - . + 0x0
	.align	3
	.quad	L103
	.short	17
	.short	0
	.align	2
	.long	L108 - . + 0x0
	.align	3
	.quad	L101
	.short	17
	.short	0
	.align	2
	.long	L109 - . + 0x0
	.align	3
	.align	2
L108:
	.long	L111 - . + 0x4c000000
	.long	0x40c0
	.align	2
L107:
	.long	L113 - . + 0x4c000001
	.long	0x10c020
	.long	L114 - . + 0xe0000001
	.long	0x1e9270
	.long	L111 - . + 0x4c000000
	.long	0x4020
	.align	2
L106:
	.long	L114 - . + 0xe0000001
	.long	0x1e9120
	.long	L111 - . + 0x4c000000
	.long	0x4020
	.align	2
L109:
	.long	L111 - . + 0x68000000
	.long	0x20c0
L112:
	.asciz	"stdlib.ml"
L110:
	.asciz	"asm_normal.ml"
	.align	2
L113:
	.long	L112 - . + 0x0
	.asciz	"Stdlib.string_of_int"
	.align	2
L111:
	.long	L110 - . + 0x0
	.asciz	"Dune__exe__Asm_normal.main"
	.align	2
L114:
	.long	L112 - . + 0x0
	.asciz	"Stdlib.print_int"
	.align	3
