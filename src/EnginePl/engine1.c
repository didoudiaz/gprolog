/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine1.c                                                       *
 * Descr.: general engine                                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include "engine_pl.h"


WamWord *ensure_reserved;


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
 * ensure reserved_stack_space was not removed but the C compiler. In order*
 * to use ebp under ix86 it must be compiled with -fomit-frame-pointer. The*
 * simpliest way was to use the same C compiler invocation but adding a    *
 * global variable to ensure the stack is not removed.                     *
 *-------------------------------------------------------------------------*/

void
Call_Compiled(CodePtr codep)
{
  WamWord reserved_stack_space[1024];

#if !defined(NO_MACHINE_REG_FOR_REG_BANK) && !defined(MAP_REG_BANK)

#if defined(M_ix86_linux) || defined(M_ix86_cygwin) || \
    defined(M_ix86_sco) || defined(M_ix86_bsd)

  register WamWord *rb asm("%ebx") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_ix86_win32)

  _asm mov ebx, reg_bank

#elif defined(M_mips_irix)

  register WamWord *rb asm("$16") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_alpha_linux) || defined(M_alpha_osf)

  register WamWord *rb asm("$9") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_powerpc_linux)

  register WamWord *rb asm("15") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_powerpc_darwin)

  register WamWord *rb asm("r15") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_sparc)

  register WamWord *rb asm("%l0") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#elif defined(M_x86_64_linux)

  register WamWord *rb asm("%r12") = reg_bank;
  ensure_reserved = (WamWord *) rb; /* to avoid gcc warning */

#endif

#endif

#ifdef M_sparc
  register long *rfl asm("%l2") = base_fl;
  register double *rfd asm("%l3") = base_fd;
#endif

  ensure_reserved = reserved_stack_space;
  (*codep) ();
}
