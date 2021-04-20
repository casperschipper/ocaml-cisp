	.file ""
	.section __TEXT,__literal16,16byte_literals
	.align	4
_caml_negf_mask:
	.quad	0x8000000000000000
	.quad	0
	.align	4
_caml_absf_mask:
	.quad	0x7fffffffffffffff
	.quad	-1
	.data
	.globl	_camlTest_mod__data_begin
_camlTest_mod__data_begin:
	.text
	.globl	_camlTest_mod__code_begin
_camlTest_mod__code_begin:
	nop
	.data
	.align	3
	.data
	.align	3
	.quad	3063
	.globl	_camlTest_mod__set_of_closures_38
_camlTest_mod__set_of_closures_38:
	.globl	_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22_closure
_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22_closure:
	.quad	_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22
	.quad	3
	.data
	.align	3
	.quad	3063
	.globl	_camlTest_mod__set_of_closures_37
_camlTest_mod__set_of_closures_37:
	.globl	_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9_closure
_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9_closure:
	.quad	_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9
	.quad	3
	.data
	.align	3
	.quad	2816
	.globl	_camlTest_mod
_camlTest_mod:
	.quad	1
	.quad	1
	.data
	.align	3
	.quad	1792
	.globl	_camlTest_mod__ys_43
_camlTest_mod__ys_43:
	.quad	1
	.data
	.align	3
	.quad	1792
	.globl	_camlTest_mod__xs_45
_camlTest_mod__xs_45:
	.quad	1
	.data
	.align	3
	.globl	_camlTest_mod__gc_roots
_camlTest_mod__gc_roots:
	.quad	_camlTest_mod__xs_45
	.quad	_camlTest_mod__ys_43
	.quad	_camlTest_mod
	.quad	0
	.text
	.align	4
	.globl	_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9
_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9:
	.cfi_startproc
L100:
	movl	$3, %eax
	ret
	.cfi_endproc
	.text
	.align	4
	.globl	_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22
_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22:
	.cfi_startproc
L101:
	addq	$2, %rax
	ret
	.cfi_endproc
	.data
	.align	3
	.quad	2816
	.globl	_camlTest_mod__Pmakeblock_139
_camlTest_mod__Pmakeblock_139:
	.quad	3
	.quad	1
	.data
	.align	3
	.data
	.align	3
	.text
	.align	4
	.globl	_camlTest_mod__entry
_camlTest_mod__entry:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
L107:
	movq	_camlTest_mod__anon_fn$5btest_mod$2eml$3a1$2c31$2d$2d43$5d_9_closure@GOTPCREL(%rip), %rsi
	movabsq	$20000000001, %rdi
	movl	$3, %ebx
	movq	_camlTest_mod__Pmakeblock_139@GOTPCREL(%rip), %rax
	call	_camlStdlib__list__init_tailrec_aux_355
L102:
	cmpq	$1, %rax
	je	L106
	movq	(%rax), %rdi
	call	_caml_alloc2
L108:
	leaq	8(%r15), %rbx
	movq	$2048, -8(%rbx)
	movq	%rdi, (%rbx)
	movq	$1, 8(%rbx)
	movq	8(%rax), %rax
	call	_camlStdlib__list__rev_append_331
L103:
	jmp	L105
L106:
	movl	$1, %eax
L105:
	movq	_camlTest_mod__xs_45@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	_camlTest_mod__xs_45@GOTPCREL(%rip), %rax
	movq	(%rax), %rbx
	movq	_camlTest_mod__anon_fn$5btest_mod$2eml$3a3$2c18$2d$2d34$5d_22_closure@GOTPCREL(%rip), %rax
	call	_camlStdlib__list__map_453
L104:
	movq	_camlTest_mod__ys_43@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	_camlTest_mod@GOTPCREL(%rip), %rax
	movq	_camlTest_mod__ys_43@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, 8(%rax)
	movq	_camlTest_mod@GOTPCREL(%rip), %rax
	movq	_camlTest_mod__xs_45@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%rax)
	movl	$1, %eax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.data
	.align	3
	.text
	nop
	.globl	_camlTest_mod__code_end
_camlTest_mod__code_end:
	.data
				/* relocation table start */
	.align	3
				/* relocation table end */
	.data
	.quad	0
	.globl	_camlTest_mod__data_end
_camlTest_mod__data_end:
	.quad	0
	.align	3
	.globl	_camlTest_mod__frametable
_camlTest_mod__frametable:
	.quad	4
	.quad	L104
	.word	17
	.word	0
	.align	2
	.set L$set$1, (L109 - .) + 0
	.long	L$set$1
	.align	3
	.quad	L103
	.word	17
	.word	0
	.align	2
	.set L$set$2, (L110 - .) + 0
	.long	L$set$2
	.align	3
	.quad	L108
	.word	18
	.word	2
	.word	1
	.word	5
	.byte	1
	.byte	1
	.align	3
	.quad	L102
	.word	17
	.word	0
	.align	2
	.set L$set$3, (L111 - .) + 0
	.long	L$set$3
	.align	3
	.align	2
L111:
	.set L$set$4, (L113 - .) + -1207959551
	.long	L$set$4
	.long	262256
	.set L$set$5, (L114 - .) + 268435457
	.long	L$set$5
	.long	332401
	.set L$set$6, (L116 - .) + -1409286144
	.long	L$set$6
	.long	4240
	.align	2
L109:
	.set L$set$7, (L117 - .) + -1811939328
	.long	L$set$7
	.long	12432
	.align	2
L110:
	.set L$set$8, (L118 - .) + -1879048191
	.long	L$set$8
	.long	237792
	.set L$set$9, (L119 - .) + 1811939329
	.long	L$set$9
	.long	245952
	.set L$set$10, (L114 - .) + 268435457
	.long	L$set$10
	.long	332337
	.set L$set$11, (L116 - .) + -1409286144
	.long	L$set$11
	.long	4240
L112:
	.ascii	"list.ml\0"
L115:
	.ascii	"test_mod.ml\0"
	.align	2
L117:
	.set L$set$12, (L115 - .) + 0
	.long	L$set$12
	.ascii	"Test_mod.ys\0"
	.align	2
L113:
	.set L$set$13, (L112 - .) + 0
	.long	L$set$13
	.ascii	"Stdlib__list.init_tailrec_aux\0"
	.align	2
L116:
	.set L$set$14, (L115 - .) + 0
	.long	L$set$14
	.ascii	"Test_mod.xs\0"
	.align	2
L119:
	.set L$set$15, (L112 - .) + 0
	.long	L$set$15
	.ascii	"Stdlib__list.rev\0"
	.align	2
L118:
	.set L$set$16, (L112 - .) + 0
	.long	L$set$16
	.ascii	"Stdlib__list.rev_append\0"
	.align	2
L114:
	.set L$set$17, (L112 - .) + 0
	.long	L$set$17
	.ascii	"Stdlib__list.init\0"
	.align	3
