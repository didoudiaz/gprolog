/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog engine                                                   */
/* File  : engine1.c                                                       */
/* Descr.: general engine                                                  */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/
#include "engine_pl.h"




/*-------------------------------------------------------------------------*/
/* Call_Compiled is defined here to be sure that no C compiler optimization*/
/* will be applied (see Makefile). Normally it is not a problem, but for   */
/* future ports the -fomit-frame-pointer option could be a problem.        */
/* reserved_stack_space is only used to reserve some space in the stack and*/
/* should not be removed by the compiler. This allows prolog compiled code */
/* to use this space to pass the arguments to C run-time functions, without*/
/* having to decrement $sp (ie. push/pop) (e.g. on ix86).                  */
/* Note finally that since this function is called inside Call_Next, we are*/
/* sure that $sp at longjmp will be more recent (ie. <) than $sp at setjmp.*/
/* Certain implementations of setjmp/longjmp requires this.                */
/*                                                                         */
/* Warning: do not use nested { ... } blocs to declare register variables  */
/* The C compiler "forgets" them after the bloc (e.g. solaris 2.6).        */
/*-------------------------------------------------------------------------*/

void Call_Compiled(CodePtr codep)

{
 WamWord reserved_stack_space[1024];

#ifndef MAP_REG_BANK

#if defined(M_ix86_linux) || defined(M_ix86_cygwin) || \
    defined(M_ix86_sco) || defined(M_ix86_bsd)

 register WamWord *rb asm("%ebx")=reg_bank;

#elif defined(M_ix86_win32)

 _asm mov ebx,reg_bank

#elif defined(M_powerpc_linux)

 register WamWord *rb asm("15")=reg_bank;

#elif defined(M_sparc)

 register WamWord *rb asm("%l0")=reg_bank;

#endif

#endif

#ifdef M_sparc
 register long   *rfl asm("%l2")=base_fl;
 register double *rfd asm("%l3")=base_fd;
#endif

 (*codep)();
}



