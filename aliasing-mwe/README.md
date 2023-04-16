# How to replicate

To replicate, just run:

```
opam install core_bench core_unix
bash replicate.sh
```

# Results

The aliased version is about 3x slower.
According to the Flambda inlining report, `unsafe_set` was inlined:

```txt
* Application of unsafe_set/2{asm_alias.ml:5,2-20}

  This function was not specialised because it is not recursive.

  This function was inlined because of an annotation.
```

Looking at the assembly, it seems like the aliased version has an additional bound-check (which is weird given that `Array.unsafe_set`) is not supposed to make such a check.

**Normal version:**

```
.L101:
	movq	8(%r14), %r15
	movq	camlDune__exe__Asm_normal__Pccall_36@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	camlDune__exe__Asm_normal__Pccall_36@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	$3, (%rax)
	movq	camlDune__exe__Asm_normal__Parrayrefs_34@GOTPCREL(%rip), %rax
	movq	camlDune__exe__Asm_normal__Pccall_36@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rbx
	movq	-8(%rbx), %rdi
	cmpq	$1023, %rdi
```

**Aliased version:**

```
.L103:
	movq	8(%r14), %r15
	movq	camlDune__exe__Asm_alias__Pccall_46@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	camlDune__exe__Asm_alias__Pccall_46@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	movzbq	-8(%rdi), %rax
	cmpq	$254, %rax
	je	.L101
	movl	$3, %esi
	call	caml_modify@PLT
	jmp	.L100
.L101:
	movl	$3, %eax
	movsd	(%rax), %xmm0
	movsd	%xmm0, (%rdi)
.L100:
	movq	camlDune__exe__Asm_alias__Parrayrefs_44@GOTPCREL(%rip), %rax
	movq	camlDune__exe__Asm_alias__Pccall_46@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rbx
	movq	-8(%rbx), %rdi
	cmpq	$1023, %rdi
...
.L104:
	call	caml_ml_array_bound_error@PLT
```