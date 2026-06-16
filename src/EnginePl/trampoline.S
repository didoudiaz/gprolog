/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : tampoline.S                                                     *
 * Descr.: trampoline (native C->native Polog code) and other asm routines *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/

/*
 * Trampoline: enter native-compiled Prolog code from C.
 *
 * Pl_Call_Compiled(codep) mainly:
 *   - reserves enough space on the C stack (for calling any C runtime function)
 *   - initializes machine register(s) used for the WAM register bank
 *     (depending on NO_USE_REGS)
 *   - applies final stack alignment fixups if required
 *   - transfers control to codep
 *
 * Before 1.6.0, this logic was implemented in C. It is now written in
 * assembler to avoid relying on compiler-specific tricks to inhibit
 * optimizations. One implementation per CPU family.
 *
 * The function is effectively noreturn: control does not return here.
 * It exits via Call_Prolog_Success/Failure or Pl_Exit_With_Exception
 * (setjmp/longjmp path).
 *
 * With NO_USE_REGS (see also machine_regs.h) no Global Register Variable (GRV)
 * are used for WAM registers (either activated with ./configure --disable-regs
 * or forced since the C compiler does not support GRV). It is however possible
 * to initialize a register in this trampoline. Mainly useful for constant
 * values. A common case is to set a register with the address of pl_reg_bank
 * (useful to both simplifies and speedups ma2asm mappers). For this:
 *
 *   - define M_TRAMPOLINE_REG_BANK as a callee-saved register r
 *     (in machine_regs.h)
 *
 *   - if NEEDS_REG_BANK_INIT is defined, initialize r with the VALUE of
 *     pl_reg_bank: r = pl_reg_bank;
 *     Recall:      WamWord *pl_reg_bank;   (global variable)
 *
 *     NB: load the contents of pl_reg_bank (ptr to the register bank array),
 *          NOT the address of the variable itself (i.e. not r = &pl_reg_bank).
 */


#define GP_CONFIG_H_MINIMAL
#include "gp_config.h"
#include "machine_regs.h"
#include "wam_regs.h"


/* PIC: 0 = no PIC, 1 = PCI, 2 = PIE */
#ifndef __PIC__
#define __PIC__ 0
#endif

#if __PIC__
/*#warning "PIC MODE"*/
#endif	

#if (defined(_WIN32) && !defined(_WIN64)) || \
     defined(M_darwin) || defined(M_sunos) 
#define SYM(x) _##x
#else
#define SYM(x) x
#endif


#if defined(NO_USE_REGS) && defined(M_TRAMPOLINE_REG_BANK)
# define NEEDS_REG_BANK_INIT
#endif

/* Do not use #define for constant expressions (not evaluated by cpp) */
.equ RSZ_BYTES, 1024 * SIZEOF_VOIDP



/*
 * ix86 - Intel 32-bit
 */
#if defined(M_ix86)

	.extern SYM(pl_reg_bank)

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	movl	4(%esp), %eax			/* load codep */
	subl	$RSZ_BYTES, %esp

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
	movl	SYM(pl_reg_bank), M_TRAMPOLINE_REG_BANK
# else /* PIC */
#  if defined(M_darwin)
#   error "i386 Darwin PIC for Pl_Call_Compiled not supported"
#  endif
	call	1f
1:	popl	%ecx
	movl	SYM(pl_reg_bank)@GOT(%ecx), M_TRAMPOLINE_REG_BANK
	movl	(M_TRAMPOLINE_REG_BANK), M_TRAMPOLINE_REG_BANK
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

#if defined(M_ix86_darwin)
	andl	$0xfffffff0, %esp
	addl	$4, %esp
#endif

	call	*%eax
	int3


/*
 * x86_64 - Intel 64 / AMD64
 */
#elif defined(M_x86_64)

	.extern SYM(pl_reg_bank)

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
#if defined(_WIN64) || defined(__CYGWIN__)
	movq	%rcx, %rax			/* load codep */
#else
	movq	%rdi, %rax			/* load codep */
#endif
	subq	$RSZ_BYTES, %rsp

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
	movq	SYM(pl_reg_bank)(%rip), M_TRAMPOLINE_REG_BANK
# else /* PIC */
#  if defined(_WIN64)
	movq	SYM(pl_reg_bank)(%rip), M_TRAMPOLINE_REG_BANK
#  else
	movq	SYM(pl_reg_bank)@GOTPCREL(%rip), M_TRAMPOLINE_REG_BANK
	movq	(M_TRAMPOLINE_REG_BANK), M_TRAMPOLINE_REG_BANK
#  endif
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

	andq	$0xfffffffffffffff0, %rsp	# ensure 4 LSB are set to 0
	addq	$8, %rsp			# align stack

	call	*%rax
	int3


/*
 * arm32 (aarch32) - ARM 32 bit (mainly armv6, armv7)
 */
#elif defined(M_arm32)

	.extern pl_reg_bank

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mov	r12, r0				/* load codep */

#if !__PIC__
	sub	sp, sp, #RSZ_BYTES
#ifdef NEEDS_REG_BANK_INIT
	ldr	M_TRAMPOLINE_REG_BANK, =pl_reg_bank
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#endif /* NEEDS_REG_BANK_INIT */
#else /* PIC */
# error "ARM32 PIC for Pl_Call_Compiled not supported"
#endif /* __PIC__ */

	blx	r12
	udf	#0


/*
 * arm64 (aarch64) - ARM 64-bit (armv8, armv9)
 */
#elif defined(M_arm64)

	.extern SYM(pl_reg_bank)

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mov	x16, x0				/* load codep */
	sub	sp, sp, #RSZ_BYTES

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
#  if defined(M_darwin)
	adrp	M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@GOTPAGE
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@GOTPAGEOFF]
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#  else
	adrp	M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)
	add	M_TRAMPOLINE_REG_BANK, M_TRAMPOLINE_REG_BANK, :lo12:SYM(pl_reg_bank)
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#  endif
# else /* PIC */
#  if defined(M_darwin)
	adrp	M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@GOTPAGE
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@GOTPAGEOFF]
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#  elif defined(M_win64) || defined(_WIN64)
	adrp	M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)
	add	M_TRAMPOLINE_REG_BANK, M_TRAMPOLINE_REG_BANK, :lo12:SYM(pl_reg_bank)
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#  else
	adrp	M_TRAMPOLINE_REG_BANK, :got:SYM(pl_reg_bank)
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK, :got_lo12:SYM(pl_reg_bank)]
	ldr	M_TRAMPOLINE_REG_BANK, [M_TRAMPOLINE_REG_BANK]
#  endif
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

	br	x16
	brk	#0


/*
 * riscv64 - RISC-V 64-bit
 */
#elif defined(M_riscv64)

	.extern pl_reg_bank

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mv	t0, a0				/* load codep */
	li	t1, RSZ_BYTES
	sub	sp, sp, t1

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
	lla	M_TRAMPOLINE_REG_BANK, pl_reg_bank
	ld	M_TRAMPOLINE_REG_BANK, 0(M_TRAMPOLINE_REG_BANK)
# else /* PIC */
	la	M_TRAMPOLINE_REG_BANK, pl_reg_bank
	ld	M_TRAMPOLINE_REG_BANK, 0(M_TRAMPOLINE_REG_BANK)
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

	jr	t0
	unimp


/*
 * mip32 - MIPS 32-bit (IRIX, o32)
 */
#elif defined(M_mips32)

	.extern pl_reg_bank

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	move	$t9, $a0			/* load codep */

#if !__PIC__
	addiu	$sp, $sp, -RSZ_BYTES
#ifdef NEEDS_REG_BANK_INIT
	la	M_TRAMPOLINE_REG_BANK, pl_reg_bank
	lw	M_TRAMPOLINE_REG_BANK, 0(M_TRAMPOLINE_REG_BANK)
#endif /* NEEDS_REG_BANK_INIT */
#else /* PIC */
# error "MIPS32 PIC for Pl_Call_Compiled not supported"
#endif /* __PIC__ */

	jalr	$t9
	nop
	break	0


/*
 * Alpha
 */
#elif defined(M_alpha)

	.extern pl_reg_bank

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mov	$16, $0				/* load codep */

#if !__PIC__
	lda	$30, -RSZ_BYTES($30)
#ifdef NEEDS_REG_BANK_INIT
	lda	M_TRAMPOLINE_REG_BANK, pl_reg_bank
	ldq	M_TRAMPOLINE_REG_BANK, 0(M_TRAMPOLINE_REG_BANK)
#endif /* NEEDS_REG_BANK_INIT */
#else /* PIC */
# error "Alpha PIC for Pl_Call_Compiled not supported"
#endif /* __PIC__ */

	jsr	$26, ($0), 0
	call_pal 0x80


/*
 * SPARC 32-bit
 */
#elif defined(M_sparc32)

	.extern pl_reg_bank

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mov	%o0, %g1			/* load codep in g1 */
	set	RSZ_BYTES, %g2
	sub	%sp, %g2, %sp

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
	sethi	%hi(pl_reg_bank), M_TRAMPOLINE_REG_BANK
	or	%l0, %lo(pl_reg_bank), M_TRAMPOLINE_REG_BANK
	ld	[%l0], %l0
# else /* PIC */
# error "SPARC32 PIC for Pl_Call_Compiled not supported"
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

	jmpl	%g1, %o7
	nop
	illtrap


/*
 * SPARC 64-bit
 */
#elif defined(M_sparc64)

	.extern pl_reg_bank
/*        .register       %g2, pl_reg_bank */
        .register       %g2, #scratch

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):	/* %o0 = codep */
        mov     %o0, %g1                        /* codep */
	set	RSZ_BYTES, %g2
	sub	%sp, %g2, %sp

#ifdef NEEDS_REG_BANK_INIT
# if !__PIC__
	sethi	%hi(pl_reg_bank), M_TRAMPOLINE_REG_BANK
	or	M_TRAMPOLINE_REG_BANK, %lo(pl_reg_bank), M_TRAMPOLINE_REG_BANK
	ldx	[M_TRAMPOLINE_REG_BANK], M_TRAMPOLINE_REG_BANK
# else /* PIC */
        sethi   %hi(_GLOBAL_OFFSET_TABLE_-4), %l7
	call    __sparc_get_pc_thunk.l7
	add    %l7, %lo(_GLOBAL_OFFSET_TABLE_+4), %l7

	sethi   %hi(pl_reg_bank), %o2
	or      %o2, %lo(pl_reg_bank), %o2
	ldx     [%l7 + %o2], %o2               /* load pl_reg_bank addr */
	ldx     [%o2], M_TRAMPOLINE_REG_BANK   /* load its content */
	
# endif /* __PIC__ */
#endif /* NEEDS_REG_BANK_INIT */

	jmpl	%g1, %o7
	nop
	illtrap


/*
 * PowerPC 32-bit
 */
#elif defined(M_ppc32)

	.extern SYM(pl_reg_bank)

	.text
	.globl SYM(Pl_Call_Compiled)
SYM(Pl_Call_Compiled):
	mr	12, 3				/* load codep */

#if !__PIC__
	addi	1, 1, -RSZ_BYTES
#ifdef NEEDS_REG_BANK_INIT
	lis	M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@ha
	addi	M_TRAMPOLINE_REG_BANK, M_TRAMPOLINE_REG_BANK, SYM(pl_reg_bank)@l
	lwz	M_TRAMPOLINE_REG_BANK, 0(M_TRAMPOLINE_REG_BANK)
#endif /* NEEDS_REG_BANK_INIT */
#else /* PIC */
# error "PPC32 PIC for Pl_Call_Compiled not supported"
#endif /* __PIC__ */

	mtctr	12
	bctrl
	trap


#else
# error "unsupported CPU for Pl_Call_Compiled"
#endif

/*
 * GNU ld on ELF requieres this .note.GNU-stack section
 * #if defined(__ELF__) should be enough (but is it reliable on old systems ?)
 */
#if !defined(_WIN32) && !defined(__CYGWIN__) && !defined(M_darwin) && !defined(_WIN32)
	.section .note.GNU-stack,""
	/* or 	.section .note.GNU-stack,"",@progbits on recent 'as' */
#endif


/*
 * --------------------------
 * setjmp/longjmp replacement
 * --------------------------
 */

#if defined(_WIN64)

#if defined(M_x86_64)

/*
 * Code based on the disassembly under Microsoft Visual Studio.
 * Follows the definition of jmp_buf in setjmp.h:
 *	
 *  typedef struct _JUMP_BUFFER
 *   {
 *	unsigned __int64 Frame; // 00 (offset)
 *	unsigned __int64 Rbx;   // 08
 *	unsigned __int64 Rsp;   // 10
 *	unsigned __int64 Rbp;   // 18
 *	unsigned __int64 Rsi;   // 20
 *	unsigned __int64 Rdi;   // 28
 *	unsigned __int64 R12;   // 30
 *	unsigned __int64 R13;   // 38
 *	unsigned __int64 R14;   // 40
 *	unsigned __int64 R15;   // 48
 *	unsigned __int64 Rip;   // 50
 *	unsigned long MxCsr;    // 58
 *	unsigned short FpCsr;   // 5C
 *	unsigned short Spare;   // 5E
 *
 *	SETJMP_FLOAT128 Xmm6;   // 60
 *	SETJMP_FLOAT128 Xmm7;   // 70
 *	SETJMP_FLOAT128 Xmm8;   // 80
 *	SETJMP_FLOAT128 Xmm9;   // 90
 *	SETJMP_FLOAT128 Xmm10;  // A0
 *	SETJMP_FLOAT128 Xmm11;  // B0
 *	SETJMP_FLOAT128 Xmm12;  // C0
 *	SETJMP_FLOAT128 Xmm13;  // D0
 *	SETJMP_FLOAT128 Xmm14;  // E0
 *	SETJMP_FLOAT128 Xmm15;  // F0
 *   } _JUMP_BUFFER;
 */

	.p2align	4,,15
	.globl	SYM(pl_setjmp)
	.def	SYM(pl_setjmp); .scl 2; .type 32; .endef
SYM(pl_setjmp):
	xorq	%rdx,%rdx	   /* this was added to the original MSVC version (ensured original longjmp works) */
	movq	%rdx,(%rcx)        /* jmp_buf->Frame */
	movq	%rbx,0x8(%rcx)	   /* jmp_buf->Rbx */	 
	leaq	0x8(%rsp),%rax	                       
	movq	%rax,0x10(%rcx)	   /* jmp_buf->Rsp */	 
	movq	%rbp,0x18(%rcx)	   /* jmp_buf->Rbp */	 
	movq	%rsi,0x20(%rcx)	   /* jmp_buf->Rsi */	 
	movq	%rdi,0x28(%rcx)	   /* jmp_buf->Rdi */	 
	movq	%r12,0x30(%rcx)	   /* jmp_buf->R12 */	 
	movq	%r13,0x38(%rcx)	   /* jmp_buf->R13 */	 
	movq	%r14,0x40(%rcx)	   /* jmp_buf->R14 */	 
	movq	%r15,0x48(%rcx)	   /* jmp_buf->R15 */	 
	movq	(%rsp),%rax	                       
	movq	%rax,0x50(%rcx)	   /* jmp_buf->Rip */	 
	stmxcsr 0x58(%rcx)	   /* jmp_buf->MxCsr */
	fnstcw  0x5c(%rcx)	   /* jmp_buf->FpCsr and Spare */
	movdqa  %xmm6,0x60(%rcx)   /* jmp_buf->Xmm6 */ 
	movdqa  %xmm7,0x70(%rcx)   /* jmp_buf->Xmm7 */ 
	movdqa  %xmm8,0x80(%rcx)   /* jmp_buf->Xmm8 */ 
	movdqa  %xmm9,0x90(%rcx)   /* jmp_buf->Xmm9 */ 
	movdqa  %xmm10,0xa0(%rcx)  /* jmp_buf->Xmm10 */
	movdqa  %xmm11,0xb0(%rcx)  /* jmp_buf->Xmm11 */
	movdqa  %xmm12,0xc0(%rcx)  /* jmp_buf->Xmm12 */
	movdqa	%xmm13,0xd0(%rcx)  /* jmp_buf->Xmm13 */
	movdqa	%xmm14,0xe0(%rcx)  /* jmp_buf->Xmm14 */
	movdqa	%xmm15,0xf0(%rcx)  /* jmp_buf->Xmm15 */
	xorq	%rax,%rax
	ret


	.p2align	4,,15
	.globl	SYM(pl_longjmp)
	.def	SYM(pl_longjmp); .scl 2; .type 32; .endef

SYM(pl_longjmp):
	test	%rdx,%rdx
	jne	LJ10
	incq	%rdx
LJ10:
	movq	%rdx,%rax          /* retval */	
	movq	0x8(%rcx),%rbx     /* jmp_buf->Rbx */
	movq	0x18(%rcx),%rbp    /* jmp_buf->Rbp */
	movq	0x20(%rcx),%rsi    /* jmp_buf->Rsi */
	movq	0x28(%rcx),%rdi    /* jmp_buf->Rdi */
	movq	0x30(%rcx),%r12    /* jmp_buf->R12 */
	movq	0x38(%rcx),%r13    /* jmp_buf->R13 */
	movq	0x40(%rcx),%r14    /* jmp_buf->R14 */
	movq	0x48(%rcx),%r15    /* jmp_buf->R15 */
	ldmxcsr	0x58(%rcx)         /* jmp_buf->MxCsr */
	fnclex
	fldcw	0x5C(%rcx)         /* jmp_buf->FpCsr and Spare */
	movdqa	0x60(%rcx),%xmm6   /* jmp_buf->Xmm6 */ 
	movdqa	0x70(%rcx),%xmm7   /* jmp_buf->Xmm7 */ 
	movdqa	0x80(%rcx),%xmm8   /* jmp_buf->Xmm8 */ 
	movdqa	0x90(%rcx),%xmm9   /* jmp_buf->Xmm9 */ 
	movdqa	0xA0(%rcx),%xmm10  /* jmp_buf->Xmm10 */
	movdqa	0xB0(%rcx),%xmm11  /* jmp_buf->Xmm11 */
	movdqa	0xC0(%rcx),%xmm12  /* jmp_buf->Xmm12 */
	movdqa	0xD0(%rcx),%xmm13  /* jmp_buf->Xmm13 */
	movdqa	0xE0(%rcx),%xmm14  /* jmp_buf->Xmm14 */
	movdqa	0xF0(%rcx),%xmm15  /* jmp_buf->Xmm15 */
	movq	0x50(%rcx),%rdx    /* jmp_buf->Rip */
	movq	0x10(%rcx),%rsp    /* jmp_buf->Rsp */
	jmp	*%rdx

#elif defined(M_arm64)

/*
 * See for instance:
 * https://github.com/ziglang/zig/blob/master/lib/libc/mingw/misc/mingw_longjmp.S
 * https://github.com/msys2/msys2-runtime/blob/msys2-3.6.5/newlib/libc/machine/aarch64/setjmp.S
 */
	.globl	SYM(pl_setjmp)
	.def	SYM(pl_setjmp);	.scl	2;	.type	32;	.endef
SYM(pl_setjmp):
	str     xzr, [x0]             /* jmp_buf->Frame */
	stp     x19, x20, [x0, #0x10] /* jmp_buf->X19, X20 */
	stp     x21, x22, [x0, #0x20] /* jmp_buf->X21, X22 */
	stp     x23, x24, [x0, #0x30] /* jmp_buf->X23, X24 */
	stp     x25, x26, [x0, #0x40] /* jmp_buf->X25, X26 */
	stp     x27, x28, [x0, #0x50] /* jmp_buf->X27, X28 */
	stp     x29, x30, [x0, #0x60] /* jmp_buf->Fp,  Lr  */
	mov     x2,  sp
	str     x2,       [x0, #0x70] /* jmp_buf->Sp */
	mrs     x2,  fpcr
	str     w2,       [x0, #0x78] /* jmp_buf->Fpcr */
	mrs     x2,  fpsr
	str     w2,       [x0, #0x7c] /* jmp_buf->Fpsr */
	stp     d8,  d9,  [x0, #0x80] /* jmp_buf->D[0-1] */
	stp     d10, d11, [x0, #0x90] /* jmp_buf->D[2-3] */
	stp     d12, d13, [x0, #0xa0] /* jmp_buf->D[4-5] */
	stp     d14, d15, [x0, #0xb0] /* jmp_buf->D[6-7] */
	mov     x0,  #0
	ret

	

	.globl SYM(pl_longjmp)
	.def	SYM(pl_longjmp);	.scl	2;	.type	32;	.endef
SYM(pl_longjmp):
	ldp     x19, x20, [x0, #0x10] /* jmp_buf->X19, X20 */
	ldp     x21, x22, [x0, #0x20] /* jmp_buf->X21, X22 */
	ldp     x23, x24, [x0, #0x30] /* jmp_buf->X23, X24 */
	ldp     x25, x26, [x0, #0x40] /* jmp_buf->X25, X26 */
	ldp     x27, x28, [x0, #0x50] /* jmp_buf->X27, X28 */
	ldp     x29, x30, [x0, #0x60] /* jmp_buf->Fp,  Lr  */
	ldr     x2,       [x0, #0x70] /* jmp_buf->Sp */
	mov     sp,  x2
	ldr     w2,       [x0, #0x78] /* jmp_buf->Fpcr */
	msr     fpcr, x2
	ldr     w2,       [x0, #0x7c] /* jmp_buf->Fpsr */
	msr     fpsr, x2
	ldp     d8,  d9,  [x0, #0x80] /* jmp_buf->D[0-1] */
	ldp     d10, d11, [x0, #0x90] /* jmp_buf->D[2-3] */
	ldp     d12, d13, [x0, #0xa0] /* jmp_buf->D[4-5] */
	ldp     d14, d15, [x0, #0xb0] /* jmp_buf->D[6-7] */
	mov     x0,  x1               /* retval */
	ret

#else

#error WIN64 pl_setjmp/pl_longjmp replacement unavailable for this CPU

#endif

#endif 			/* _WiN64 */
