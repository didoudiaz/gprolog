/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : eng1-x86_64_win.c                                               *
 * Descr.: general engine (assembly part)                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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

/* Replacement file for engine1.c for x86_64-win64 with MSVC
 * (MSVC 64 bits does not accept inline assembly)
 */

        .text

        .p2align 4,,15
.globl Pl_Call_Compiled
        .def    Pl_Call_Compiled;       .scl    2;      .type   32;     .endef
Pl_Call_Compiled:
# reserve space (same as WamWord reserved_stack_space[1024])
        subq    $8200, %rsp
        
# See comment in Ma2Asm/x86_64_any.c for stack alignment

# this is better:
#        andq    $0xfffffffffffffff0,%rsp
# but yasm emits a buggy warning 'value does not fit in 32 bit field'
# so we replace the 'and' by 2 'shifts' >> 4 << 4

        sarq    $4,%rsp         # ensure 4 LSB are set to 0
        salq    $4,%rsp
        
        addq    $8,%rsp         # align stack

        movq    pl_reg_bank(%rip), %r12 # set r12 to pl_reg_bank
        
# branch to the Prolog code (codep in engine1.c)
        call    *%rcx           # normally will never return (longjmp instead)
        addq    $8200, %rsp     # thus this is not important
        ret

/*
       .p2align 4,,15
.globl Pl_Set_Reg_Bank_Register
        .def    Pl_Set_Reg_Bank_Register;       .scl    2;      .type   32;     .endef
Pl_Set_Reg_Bank_Register:
        movq    pl_reg_bank(%rip), %r12
        ret
*/
        
/*
  The following is commented because for the moment it works with
  Call_Prolog_Success/Call_Prolog_Fail in engine.c.
  Else, call these versions instead
  (and remove static from p_jumper decl in engine.c)
        

        .p2align 4,,15
.globl Pl_Call_Prolog_Fail
        .def    Pl_Call_Prolog_Fail;       .scl    2;      .type   32;     .endef
Pl_Call_Prolog_Fail:
        subq    $40, %rsp
        subq    $8,%rsp
        movq    p_jumper(%rip), %rcx
        movl    $-1, %edx
        call    longjmp


        .p2align 4,,15
.globl Pl_Call_Prolog_Success
        .def    Pl_Call_Prolog_Success;       .scl    2;      .type   32;     .endef
Pl_Call_Prolog_Success:
        subq    $40, %rsp
        subq    $8,%rsp
        movq    p_jumper(%rip), %rcx
        movl    $1, %edx
        call    longjmp
 
        .data
        .align 32
        .comm   p_jumper, 8, 4
*/

/*
   MSVC Community 2019 x64 uses stack unwinding and this crashes gprolog.
   (see also comment in arch_dep.h for Mingw64-gcc)

   Under Microsoft Visual Studio I debugged chkma.exe (in Ma2Asm) and discovered
   the error is due to setjmp/longjmp. Indeed, now longjmp unwinds the stack and this
   causes a segmentation fault (the problem appears in ntdll!RtlUnwindEx).
   This change seems to be related to SEH (for a compatibility between SEH and setjmp ?).

   A similar problem is reported here:
   https://blog.lazym.io/2020/09/21/Unicorn-Devblog-setjmp-longjmp-on-Windows/

   There is a portion of code in longjmp (in __longjmp_internal) which first does
   a test on shallow stack and then checks the first field of jmp_buf
   (which contains the address of the frame jmp_buf or 0). If it is != 0
   the unwind is done (else this part is ignored). The unwinds crashes gprolog.

   Several solutions:
   1) provide a setjmp which put 0 in the first field (and keep original longjmp)
   2) provide a longjmp which ignores the tests (and keep original setjmp)
   3) provide both (in case something else change in jmp_buf structure

   I provide 3), i.e. a replacement for both, based on the disassembly under 
   Microsoft Visual Studio (rewritten in gas AT&T syntax). It thus respects the
   definition of jmp_buf in setjmp.h which can be found in:
   C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.28.29333\include/setjmp.h

    I report the relevant part (I added the offset in the buffer as comments)

    typedef struct _JUMP_BUFFER
    {
	unsigned __int64 Frame; // 00 (offset)
	unsigned __int64 Rbx;   // 08
	unsigned __int64 Rsp;   // 10
	unsigned __int64 Rbp;   // 18
	unsigned __int64 Rsi;   // 20
	unsigned __int64 Rdi;   // 28
	unsigned __int64 R12;   // 30
	unsigned __int64 R13;   // 38
	unsigned __int64 R14;   // 40
	unsigned __int64 R15;   // 48
	unsigned __int64 Rip;   // 50
	unsigned long MxCsr;    // 58
	unsigned short FpCsr;   // 5C
	unsigned short Spare;   // 5E

	SETJMP_FLOAT128 Xmm6;   // 60
	SETJMP_FLOAT128 Xmm7;   // 70
	SETJMP_FLOAT128 Xmm8;   // 80
	SETJMP_FLOAT128 Xmm9;   // 90
	SETJMP_FLOAT128 Xmm10;  // A0
	SETJMP_FLOAT128 Xmm11;  // B0
	SETJMP_FLOAT128 Xmm12;  // C0
	SETJMP_FLOAT128 Xmm13;  // D0
	SETJMP_FLOAT128 Xmm14;  // E0
	SETJMP_FLOAT128 Xmm15;  // F0
    } _JUMP_BUFFER;

   I declare several entries (without and with leading underscores) in case of...
*/

	.p2align	4,,15
	.globl	setjmp
	.def	setjmp; .scl 2; .type 32; .endef
	.globl	_setjmp
	.def	_setjmp; .scl 2; .type 32; .endef
	.globl	__setjmp
	.def	__setjmp; .scl 2; .type 32; .endef
setjmp:
_setjmp:
__setjmp:
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
	.globl	longjmp
	.def	longjmp; .scl 2; .type 32; .endef
	.globl	_longjmp
	.def	_longjmp; .scl 2; .type 32; .endef
	.globl	__longjmp
	.def	__longjmp; .scl 2; .type 32; .endef
longjmp:
_longjmp:
__longjmp:
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
