/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine1.c                                                       *
 * Descr.: general engine                                                  *
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


#include "engine_pl.h"

#ifdef M_darwin
#define UN "_"
#else
#define UN
#endif

WamWord *pl_ensure_reserved;

#if !(defined(M_x86_64) && defined(_MSC_VER))/* see file eng1-x86_64_win.s */


/*-------------------------------------------------------------------------*
 * Call_Compiled invokes a Prolog code.                                    *
 * reserved_stack_space is only used to reserve some space in the stack and*
 * should not be removed by the compiler. This allows prolog compiled code *
 * to use this space to pass the arguments to C run-time functions, without*
 * having to decrement $sp (ie. push/pop) (e.g. on ix86).                  *
 * Note finally that since this function is called inside Call_Next, we are*
 * sure that $sp at longjmp will be more recent (ie. <) than $sp at setjmp.*
 * Certain implementations of setjmp/longjmp requires this.                *
 *                                                                         *
 * Warning: do not use nested { ... } blocs to declare register variables  *
 * The C compiler "forgets" them after the bloc (e.g. solaris 2.6).        *
 *                                                                         *
 * This functions is in a separate file for historical reasons. In versions*
 * <= 1.2.4, this file was compiled without any C compiler optimization to *
 * ensure reserved_stack_space was not removed by the C compiler. In order *
 * to use ebp under ix86 it must be compiled with -fomit-frame-pointer. The*
 * simpliest way was to use the same C compiler invocation but adding a    *
 * global variable to ensure the stack is not removed.                     *
 *-------------------------------------------------------------------------*/

void
Pl_Call_Compiled(CodePtr codep)
{
  WamWord reserved_stack_space[1024];

  /* check why not sparc32_bsd ? */
#if (defined(M_sparc32) && !defined(M_sparc32_bsd)) || defined(M_sparc64)
  register PlLong * __attribute__ ((unused)) rfl asm("%l2") = pl_base_fl;
  register double * __attribute__ ((unused)) rfd asm("%l3") = pl_base_fd;
  pl_ensure_reserved = (WamWord *) rfl + (PlLong) rfd; /* to avoid gcc remove 2 previous inits ! */
#endif

#if !defined(NO_MACHINE_REG_FOR_REG_BANK) && !defined(MAP_REG_BANK)

#if defined(M_ix86_linux) || \
    defined(M_ix86_cygwin) || defined(M_ix86_mingw) || \
    defined(M_ix86_sco) || defined(M_ix86_bsd)

  register WamWord *rb asm("%ebx") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(_MSC_VER) && defined(M_ix86)

  _asm mov ebx, pl_reg_bank

#elif defined(M_mips32)

  register WamWord *rb asm("$16") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_alpha)

  register WamWord *rb asm("$9") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_ppc32_linux)

  register WamWord *rb asm("15") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_ppc32_darwin)

  register WamWord *rb asm("r15") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_sparc32) || defined(M_sparc64)

  register WamWord *rb asm("%l0") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_arm32)

  register WamWord *rb asm("r10") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_arm64)

  register WamWord *rb asm("x20") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */
#ifdef __clang__		       /* the above does not assign x20 with clang */
#ifdef M_darwin
  asm("adrp x20, " UN "pl_reg_bank@GOTPAGE");
  asm("ldr  x20, [x20, " UN "pl_reg_bank@GOTPAGEOFF]");
  asm("ldr  x20, [x20]");
#else
  /* this PIC code is mandatory on darwin ; could be simplified on linux (but ready for PIC) */
  asm("adrp x20, :got:" UN "pl_reg_bank");
  asm("ldr  x20, [x20, :got_lo12:" UN "pl_reg_bank]");
  asm("ldr  x20, [x20]");
#endif
#endif /* clang */

#elif defined(M_x86_64)
  register WamWord *rb asm("%r12") = pl_reg_bank;
  pl_ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */
#ifdef __clang__		       /* the above does not assign r12 with clang */
  /* this PIC code is mandatory on darwin ; could be simplified on linux (but ready for PIC) */
  /* on linux this is enough: asm("movq pl_reg_bank, %r12"); */
  asm("movq " UN "pl_reg_bank@GOTPCREL(%rip), %r12");
  asm("movq (%r12), %r12");
#endif /* clang */

#endif /* end of architectures switch */

#endif /* !defined(NO_MACHINE_REG_FOR_REG_BANK) && !defined(MAP_REG_BANK) */

  pl_ensure_reserved = reserved_stack_space;
#if defined(M_ix86_darwin)	/* see comment in Ma2Asm/ix86_any.c */
  asm("andl $0xfffffff0,%esp");
  asm("addl $4,%esp");
#elif defined(M_x86_64) && !defined(_MSC_VER)		/* see comment in Ma2Asm/x86_64_any.c */
  asm("andq $0xfffffffffffffff0,%rsp");
  asm("addq $8,%rsp");
#endif

  (*codep) ();
}
#endif

