/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_any.c                                                      *
 * Descr.: translation file for Linux/Cygwin/... on intel x86              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

#include <stdio.h>
#include <string.h>




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"

#define MAX_C_ARGS_IN_C_CODE       32

#if (defined(M_ix86_cygwin) || defined(M_ix86_bsd)) && !defined(__FreeBSD__)

#define UN                         "_"

#else

#define UN

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;

char *fc_arg_regs[] = FC_SET_OF_REGISTERS;
int inside_fc;
int fc_off_in_stack;




	  /* variables for ma_parser.c / ma2asm.c */

char *comment_prefix = "#";
char *local_symb_prefix = ".L";
int strings_need_null = 0;
int call_c_reverse_args = 0;

/* The code below has been generated using extract_asm from wam_inst.c with
 * fast call (FC) turned on by configure (--enable-fast-call by default).
 *
 * The extract_asm command line used is shown below. We ask extract_asm to :
 *
 *   - ignore pushl/popl %ebp/%edi/%esi (Prolog code does not need them to be
 *     saved, and we will finally return C by a longjmp restoring all regs).
 *     NB: the original code should be checked before since some pushl are
 *     used to pass arguments to functions not compiled with FC (see below).
 *     only leading pushl and traling popl should be removed.
 *
 *   - ignore subl/addl size,%esp we have reserved enough space in the stack
 *     that can be used safely (and we do not want to adjust %esp). NB: check
 *     then all references to off(%esp).
 *
 *   - stop a function when a ret is encountered (this caused a problem for
 *     Unify_Void since there was an intermediate ret, see comment at 
 *     Unify_Void entry).
 * 
 * The inlined code contains some calls to functions (e.g. Unify). Since 
 * this code can also be used if the core WAM functions are not compiled for
 * FC (--disable-fast-call) we have to check this mode and add a prelude to
 * each "call", "fct" where "fct" is supposed to be FC to pass arguments in
 * the stack (1st argument in 0(%esp), 2nd in 4(%esp), ...). On the contrary,
 * if "fct" is not a FC (e.g. indirect call to fd_unify_integer) or if some
 * arguments are not passed in registers (e.g double in Global_Push_Float),
 * we have to check if gcc pushes arguments. In that case we have to take
 * care to the stack (to avoid stack leaks). It is preferable to avoid push
 * and to pass arguments relatively to %esp (we have enough free space) else
 * we have to recover the stack after the call.
 * Conclusion: search and check each occurrence of "call".
 *
 * Another customization concerns functions that received some arguments in
 * the stack. Since the call is not done, the return address has not been
 * pushed onto the stack and thus all offsets wrt to %esp are wrong. This
 * is the case for Switch_On_Term whose 4th and 5th arguments are in the
 * stack. NB: to view how args are passed here, compile a Prolog program 
 * generating a switch_on_term and use:
 *    gplc -S --comment --full-inline-asm prog.pl
 * the file t.s shows which arguments are passed in registers and which 
 * are in the stack.
 *
 * Here are some parts of the original assembly code of this function
 * produced by gcc (use: extract_asm -a file.s -c ../EnginePl/wam_inst.c).
 *
 * Switch_On_Term:             # ret adr is at 0(%esp) and args from 4(%esp)
 *         subl $16,%esp       # reserve space - args are from 20(%esp)
 *         pushl %ebp          # save 3 registers 
 *         pushl %edi          # 12 more bytes in the stack
 *         pushl %esi          # args 4 is at 32(%esp) and 5 at 36(%esp)
 *         movl %edx,%edi
 *         movl %eax,12(%esp)  # a local variable (offset < 28)
 *         ...
 * .L386:
 *         movl 32(%esp),%eax  # 4th argument 
 *         jmp .L383
 *         .p2align 2
 * .L387:
 *         movl 36(%esp),%eax  # 5th argument 
 *         jmp .L383
 *         .p2align 2
 *         ...
 *         popl %esi           # restore registers
 *         popl %edi
 *         popl %ebp
 *         addl $16,%esp       # recover the stack
 *         ret
 *
 * To use this code as is for inlining it suffices to remove the 'ret' and
 * to simulate the push of the return address replacing $16,%esp by
 * $20,%esp. If we want to remove pushl/popl registers we can then replace
 * $20,%esp by $32,%esp. If we want to avoid stack adjustements we have to
 * remove subl/addl xxx,%esp (done by extract_asm) and adjust all off(%esp).
 * We subtract 32 for all offets for arguments (offsets are then >= 32) and
 * for local vars we remap them in the stack after arguments (in our example
 * from 8(%esp) is correct).
 * Conclusion: search and check each occurrence of %esp.
 */

 /* command: extract_asm -a x.s -c ../EnginePl/wam_inst.c -i 'pushl.*esi' -i 'popl.*esi' -i 'pushl.*edi' -i 'popl.*edi' -i 'pushl.*ebp' -i 'popl.*ebp' -i 'addl.*,%esp' -i 'subl.*,%esp' -e ret -f INLINED -o tmp.c */

char *inline_asm_data[] = {
  "Unify_Local_Value", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "movl", "1048(%ebx),%edx",
      "testl", "%edx,%edx",
      "je", INL_LABEL(1),
      "leal", "4(%edx),%ecx",
      "movl", "%ecx,1048(%ebx)",
      "movl", "(%edx),%edx",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
      "movl", "%edx,4(%esp)",
#endif
      "call", "Unify",
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%ecx",
      "xorl", "%esi,%esi",
      "andl", "$7,%ecx",
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(6),
      "movl", "%eax,%edx",
      "shrl", "$3,%edx",
      "je", INL_LABEL(3),
      ".p2align", "2",
  INL_LABEL(2),
      "movl", "%edx,%esi",
      "movl", "(%esi),%eax",
      "movl", "%eax,%ecx",
      "andl", "$7,%ecx",
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(6),
      "movl", "%eax,%edx",
      "shrl", "$3,%edx",
      "cmpl", "%esi,%edx",
      "jne", INL_LABEL(2),
  INL_LABEL(3),
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(6),
      "movl", "1060(%ebx),%edi",
      "cmpl", "%edi,%esi",
      "jb", INL_LABEL(6),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(4),
      "cmpl", "%edi,%esi",
      "jb", INL_LABEL(5),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(5),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(5),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$1,%al",
      "movl", "%eax,(%esi)",
      "movl", "1024(%ebx),%edx",
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%edx",
      "leal", "4(%eax),%ecx",
      "orl", "$1,%edx",
      "jmp", INL_LABEL(7),
      ".p2align", "2",
  INL_LABEL(6),
      "movl", "%eax,%edx",
      "movl", "%ecx,%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
      "movl", "%edx,4(%esp)",
#endif
      "call", "Make_Copy_Of_Word",
      "movl", "%eax,%edx",
      "movl", "1024(%ebx),%eax",
      "leal", "4(%eax),%ecx",
  INL_LABEL(7),
      "movl", "%ecx,1024(%ebx)",
      "movl", "%edx,(%eax)",
      "movl", "$1,%eax",
  INL_LABEL(8),
  INL_END_FUNC,

/* This function should be generated without -i pushl... since some pushl
 * are used to pass arguments to fd_unify_with_integer */

  "Unify_Integer", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "movl", "%eax,%ebp",
      "movl", "1048(%ebx),%edi",
      "testl", "%edi,%edi",
      "je", INL_LABEL(10),
      "movl", "(%edi),%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "addl", "$4,%edi",
      "movl", "%edi,1048(%ebx)",
      "cmpl", "$1,%edx",
      "je", INL_LABEL(5),
      "jg", INL_LABEL(3),
      "testl", "%edx,%edx",
      "je", INL_LABEL(4),
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(3),
      "cmpl", "$2,%edx",
      "je", INL_LABEL(8),
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(4),
      "sarl", "$3,%ecx",
      "cmpl", "%ebp,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(7),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(7),
      ".p2align", "2",
  INL_LABEL(6),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(7),
      "sall", "$3,%ebp",
      "movl", "%ebp,(%esi)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(8),
      "movl", "fd_unify_with_integer,%eax",
      "movl", "%ebp,4(%esp)",	/* original was pushl %ebp */
      "shrl", "$3,%ecx",
      "movl", "%ecx,0(%esp)",	/* original was pushl %ecx */
      "call", "*%eax",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(9),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(10),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%ebp",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1024(%ebx)",
      "movl", "%ebp,(%eax)",
      "movl", "$1,%eax",
  INL_LABEL(11),
  INL_LABEL(12),
  INL_END_FUNC,

  "Unify_List", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1048(%ebx),%eax",
      "testl", "%eax,%eax",
      "je", INL_LABEL(9),
      "movl", "(%eax),%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%edx",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$5,%edx",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$5,%al",
      "movl", "%eax,(%esi)",
      "movl", "$0,1048(%ebx)",
      "jmp", INL_LABEL(10),
      ".p2align", "2",
  INL_LABEL(7),
      "shrl", "$3,%ecx",
      "movl", "$1,%eax",
      "movl", "%ecx,1048(%ebx)",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(8),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(9),
      "movl", "1024(%ebx),%edx",
      "movl", "$0,1048(%ebx)",
      "leal", "4(%edx),%eax",
      "movl", "%eax,1024(%ebx)",
      "sall", "$3,%eax",
      "orb", "$5,%al",
      "movl", "%eax,(%edx)",
  INL_LABEL(10),
      "movl", "$1,%eax",
  INL_LABEL(11),
  INL_END_FUNC,

  "Unify_Structure", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "movl", "%eax,%ebp",
      "movl", "%edx,%edi",
      "movl", "1048(%ebx),%eax",
      "testl", "%eax,%eax",
      "je", INL_LABEL(10),
      "movl", "(%eax),%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%edx",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$6,%edx",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$6,%al",
      "movl", "%eax,(%esi)",
      "movl", "1024(%ebx),%eax",
      "leal", "4(%eax),%esi",
      "sall", "$16,%edi",
      "movl", "%esi,1024(%ebx)",
      "addl", "%ebp,%edi",
      "movl", "%edi,(%eax)",
      "movl", "$0,1048(%ebx)",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(7),
      "movl", "%ecx,%esi",
      "sall", "$16,%edi",
      "shrl", "$3,%esi",
      "leal", "(%ebp,%edi),%eax",
      "cmpl", "%eax,(%esi)",
      "je", INL_LABEL(8),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(8),
      "addl", "$4,%esi",
      "movl", "$1,%eax",
      "movl", "%esi,1048(%ebx)",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(9),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(10),
      "movl", "1024(%ebx),%eax",
      "sall", "$16,%edi",
      "leal", "4(%eax),%edx",
      "addl", "%ebp,%edi",
      "leal", "4(%edx),%esi",
      "movl", "%edx,%ecx",
      "movl", "%esi,1024(%ebx)",
      "sall", "$3,%ecx",
      "movl", "%edi,(%edx)",
      "orl", "$6,%ecx",
      "movl", "$0,1048(%ebx)",
      "movl", "%ecx,(%eax)",
  INL_LABEL(11),
      "movl", "$1,%eax",
  INL_LABEL(12),
  INL_END_FUNC,

  "Get_Atom", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%edx,%ecx",
      "movl", "%eax,%edi",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "shrl", "$3,%edx",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%edx,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%edx",
      "shrl", "$3,%edx",
      "cmpl", "%esi,%edx",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%eax",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$3,%eax",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
#if 0
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
#else
      "addl", "$4,1032(%ebx)",
#endif
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "leal", "0(,%edi,8),%eax",
      "orb", "$3,%al",
      "movl", "%eax,(%esi)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(7),
      "sarl", "$3,%ecx",
      "cmpl", "%edi,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(8),
      "xorl", "%eax,%eax",
  INL_LABEL(9),
  INL_END_FUNC,


/* This function should be generated without -i pushl... since some pushl
 * are used to pass arguments to fd_unify_with_integer */

  "Get_Integer", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%edx,%ecx",
      "movl", "%eax,%edi",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "shrl", "$3,%edx",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%edx,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%edx",
      "shrl", "$3,%edx",
      "cmpl", "%esi,%edx",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%eax",
      "je", INL_LABEL(6),
  INL_LABEL(3),
      "cmpl", "$1,%eax",
      "jg", INL_LABEL(4),
      "testl", "%eax,%eax",
      "je", INL_LABEL(5),
      "jmp", INL_LABEL(10),
      ".p2align", "2",
  INL_LABEL(4),
      "cmpl", "$2,%eax",
      "je", INL_LABEL(9),
      "jmp", INL_LABEL(10),
      ".p2align", "2",
  INL_LABEL(5),
      "sarl", "$3,%ecx",
      "cmpl", "%edi,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(6),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(7),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(8),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(7),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(8),
      "sall", "$3,%edi",
      "movl", "%edi,(%esi)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(12),
      ".p2align", "2",
  INL_LABEL(9),
      "movl", "fd_unify_with_integer,%eax",
      "movl", "%edi,4(%esp)",	/* original was pushl %edi */
      "shrl", "$3,%ecx",
      "movl", "%ecx,0(%esp)",	/* original was pushl %ecx */
      "call", "*%eax",
      "jmp", INL_LABEL(11),
      ".p2align", "2",
  INL_LABEL(10),
      "xorl", "%eax,%eax",
  INL_LABEL(11),
  INL_LABEL(12),
  INL_END_FUNC,

/* The original code copies the double in the stack to call 
 * Global_Push_Float, the offset should be adjusted. NB: it is double
 * thus it needs 2 words (c.f. original pushl 44(%esp) twice, adjusted
 * to pushl $4(%esp). Optimization: do not touch the double already in the
 * stack */

  "Get_Float", INL_NEXT, INL_LEVEL(2), INL_INFO(0),
      "movl", "%ecx,%edx",
      "xorl", "%esi,%esi",
      "movl", "%edx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "shrl", "$3,%ecx",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%ecx,%esi",
      "movl", "(%esi),%edx",
      "movl", "%edx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "movl", "%edx,%ecx",
      "shrl", "$3,%ecx",
      "cmpl", "%esi,%ecx",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%eax",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$4,%eax",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$4,%al",
      "movl", "%eax,(%esi)",
#if 0
      "pushl", "4(%esp)",	/* adjust offset - 1st dbl arg (+4 and +0) */
      "pushl", "4(%esp)",
      "call", "Global_Push_Float",
      "addl", "$8,%esp",	/* added to recover stack */
#else
      "call", "Global_Push_Float",
#endif
      "movl", "$1,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(7),
      "shrl", "$3,%edx",
      "movl", "%edx,%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
#endif
      "call", "Obtain_Float",
      "fldl", "32(%esp)",
      "fucompp", "",
      "fnstsw", "%ax",
      "andb", "$69,%ah",
      "cmpb", "$64,%ah",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(8),
      "xorl", "%eax,%eax",
  INL_LABEL(9),
  INL_END_FUNC,

  "Get_Nil", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%eax,%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(5),
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(5),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(5),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(3),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(4),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(4),
      ".p2align", "2",
  INL_LABEL(3),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(4),
      "movl", "$14131,(%esi)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "cmpl", "$14131,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
  INL_LABEL(6),
  INL_END_FUNC,

  "Get_List", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "%eax,%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(3),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%edx",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$5,%edx",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$5,%al",
      "movl", "%eax,(%esi)",
      "movl", "$0,1048(%ebx)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(7),
      "shrl", "$3,%ecx",
      "movl", "$1,%eax",
      "movl", "%ecx,1048(%ebx)",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(8),
      "xorl", "%eax,%eax",
  INL_LABEL(9),
  INL_END_FUNC,

  "Get_Structure", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%edx,%edi",
      "movl", "%ecx,%edx",
      "movl", "%eax,%ebp",
      "xorl", "%esi,%esi",
      "movl", "%edx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "shrl", "$3,%ecx",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%ecx,%esi",
      "movl", "(%esi),%edx",
      "movl", "%edx,%eax",
      "andl", "$7,%eax",
      "cmpl", "$1,%eax",
      "jne", INL_LABEL(3),
      "movl", "%edx,%ecx",
      "shrl", "$3,%ecx",
      "cmpl", "%esi,%ecx",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%eax",
      "je", INL_LABEL(4),
  INL_LABEL(3),
      "cmpl", "$6,%eax",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%ecx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(6),
      "cmpl", "%ecx,%esi",
      "jae", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$6,%al",
      "movl", "%eax,(%esi)",
      "movl", "1024(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "sall", "$16,%edi",
      "movl", "%edx,1024(%ebx)",
      "addl", "%ebp,%edi",
      "movl", "%edi,(%eax)",
      "movl", "$0,1048(%ebx)",
      "movl", "$1,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(7),
      "movl", "%edx,%esi",
      "sall", "$16,%edi",
      "shrl", "$3,%esi",
      "leal", "(%ebp,%edi),%eax",
      "cmpl", "%eax,(%esi)",
      "jne", INL_LABEL(8),
      "addl", "$4,%esi",
      "movl", "$1,%eax",
      "movl", "%esi,1048(%ebx)",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(8),
      "xorl", "%eax,%eax",
  INL_LABEL(9),
  INL_END_FUNC,

  "Put_X_Variable", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1024(%ebx),%eax",
      "movl", "1024(%ebx),%edx",
      "leal", "4(%eax),%ecx",
      "sall", "$3,%edx",
      "movl", "%ecx,1024(%ebx)",
      "orl", "$1,%edx",
      "movl", "%edx,(%eax)",
      "movl", "%edx,%eax",
  INL_END_FUNC,

  "Put_Y_Variable", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "%eax,%edx",
      "leal", "0(,%edx,8),%eax",
      "orb", "$1,%al",
      "movl", "%eax,(%edx)",
  INL_END_FUNC,

  "Put_Unsafe_Value", INL_NEXT, INL_LEVEL(2), INL_INFO(0),
      "movl", "%eax,%edx",
      "xorl", "%esi,%esi",
      "movl", "%edx,%ecx",
      "andl", "$7,%ecx",
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(5),
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%edx",
      "movl", "%edx,%ecx",
      "andl", "$7,%ecx",
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(5),
      "movl", "%edx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "cmpl", "$1,%ecx",
      "jne", INL_LABEL(5),
      "movl", "1040(%ebx),%eax",
      "cmpl", "-12(%eax),%esi",
      "jb", INL_LABEL(5),
      "movl", "1024(%ebx),%ecx",
      "sall", "$3,%ecx",
      "orl", "$1,%ecx",
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(3),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(4),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(4),
      ".p2align", "2",
  INL_LABEL(3),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(4),
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%eax",
      "orb", "$1,%al",
      "movl", "%eax,(%esi)",
      "movl", "1024(%ebx),%edx",
      "movl", "1024(%ebx),%eax",
      "sall", "$3,%edx",
      "leal", "4(%eax),%esi",
      "orl", "$1,%edx",
      "movl", "%esi,1024(%ebx)",
      "movl", "%edx,(%eax)",
      "jmp", INL_LABEL(6),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "%ecx,%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
      "movl", "%edx,4(%esp)",
#endif
      "call", "Make_Copy_Of_Word",
      "movl", "%eax,%ecx",
  INL_LABEL(6),
      "movl", "%ecx,%eax",
  INL_END_FUNC,

  "Put_Atom", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "sall", "$3,%eax",
      "orb", "$3,%al",
  INL_END_FUNC,

  "Put_Integer", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "sall", "$3,%eax",
  INL_END_FUNC,

/* The original code: obtain the double and re-put it on the stack
 * to call Global_Push_Float (keep as #if 0). 
 * Optimization: simply do not touch the double ! */

  "Put_Float", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
#if 0
      "fldl", "0(%esp)",	/* adjust offset - 1st arg */
      "movl", "1024(%ebx),%esi",
      "sall", "$3,%esi",
      "fstpl", "(%esp)",
      "orl", "$4,%esi",
      "call", "Global_Push_Float",
      "movl", "%esi,%eax",
#else
      "movl", "1024(%ebx),%esi",
      "sall", "$3,%esi",
      "orl", "$4,%esi",
      "call", "Global_Push_Float",
      "movl", "%esi,%eax",
#endif
  INL_END_FUNC,

  "Put_Nil", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "$14131,%eax",
  INL_END_FUNC,

  "Put_List", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1024(%ebx),%eax",
      "movl", "$0,1048(%ebx)",
      "sall", "$3,%eax",
      "orb", "$5,%al",
  INL_END_FUNC,

  "Put_Structure", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%eax,%esi",
      "movl", "1024(%ebx),%ecx",
      "movl", "1024(%ebx),%eax",
      "sall", "$16,%edx",
      "leal", "4(%eax),%edi",
      "addl", "%esi,%edx",
      "movl", "%edi,1024(%ebx)",
      "sall", "$3,%ecx",
      "movl", "%edx,(%eax)",
      "orl", "$6,%ecx",
      "movl", "$0,1048(%ebx)",
      "movl", "%ecx,%eax",
  INL_END_FUNC,


  "Unify_Variable", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1048(%ebx),%eax",
      "testl", "%eax,%eax",
      "jne", INL_LABEL(1),
      "movl", "1024(%ebx),%eax",
      "movl", "1024(%ebx),%ecx",
      "leal", "4(%eax),%edx",
      "sall", "$3,%ecx",
      "movl", "%edx,1024(%ebx)",
      "orl", "$1,%ecx",
      "movl", "%ecx,(%eax)",
      "movl", "%ecx,%eax",
      "jmp", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "(%eax),%ecx",
      "movl", "%ecx,%edx",
      "addl", "$4,1048(%ebx)",
      "andl", "$7,%ecx",
      "movl", "%ecx,%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
      "movl", "%edx,4(%esp)",
#endif
      "call", "Make_Copy_Of_Word",
  INL_LABEL(2),
  INL_END_FUNC,

  "Unify_Value", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "%eax,%ecx",
      "movl", "1048(%ebx),%eax",
      "testl", "%eax,%eax",
      "je", INL_LABEL(2),
      "movl", "(%eax),%edx",
      "movl", "%ecx,%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
      "movl", "%edx,4(%esp)",
#endif
      "call", "Unify",
      "testl", "%eax,%eax",
      "jne", INL_LABEL(1),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(4),
      ".p2align", "2",
  INL_LABEL(1),
      "addl", "$4,1048(%ebx)",
      "jmp", INL_LABEL(3),
      ".p2align", "2",
  INL_LABEL(2),
      "movl", "1024(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1024(%ebx)",
      "movl", "%ecx,(%eax)",
  INL_LABEL(3),
      "movl", "$1,%eax",
  INL_LABEL(4),
  INL_END_FUNC,

  "Unify_Atom", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "movl", "%eax,%ebp",
      "movl", "1048(%ebx),%edi",
      "testl", "%edi,%edi",
      "je", INL_LABEL(8),
      "movl", "(%edi),%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "addl", "$4,%edi",
      "movl", "%edi,1048(%ebx)",
      "cmpl", "$1,%edx",
      "je", INL_LABEL(3),
      "cmpl", "$3,%edx",
      "je", INL_LABEL(6),
      "jmp", INL_LABEL(7),
      ".p2align", "2",
  INL_LABEL(3),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(4),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(5),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(5),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(5),
      "leal", "0(,%ebp,8),%eax",
      "orb", "$3,%al",
      "movl", "%eax,(%esi)",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(6),
      "sarl", "$3,%ecx",
      "cmpl", "%ebp,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(10),
      ".p2align", "2",
  INL_LABEL(7),
      "xorl", "%eax,%eax",
      "jmp", INL_LABEL(10),
      ".p2align", "2",
  INL_LABEL(8),
      "movl", "1024(%ebx),%edx",
      "leal", "4(%edx),%eax",
      "movl", "%eax,1024(%ebx)",
      "leal", "0(,%ebp,8),%eax",
      "orb", "$3,%al",
      "movl", "%eax,(%edx)",
  INL_LABEL(9),
      "movl", "$1,%eax",
  INL_LABEL(10),
  INL_END_FUNC,

  "Unify_Nil", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "1048(%ebx),%edi",
      "testl", "%edi,%edi",
      "je", INL_LABEL(6),
      "movl", "(%edi),%ecx",
      "xorl", "%esi,%esi",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%esi",
      "movl", "(%esi),%ecx",
      "movl", "%ecx,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%ecx,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%esi,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "addl", "$4,%edi",
      "movl", "%edi,1048(%ebx)",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(5),
      "movl", "1028(%ebx),%eax",
      "movl", "%eax,%edx",
      "cmpl", "-24(%eax),%esi",
      "jb", INL_LABEL(3),
      "cmpl", "1060(%ebx),%esi",
      "jb", INL_LABEL(4),
      "cmpl", "%edx,%esi",
      "jae", INL_LABEL(4),
      ".p2align", "2",
  INL_LABEL(3),
      "movl", "1032(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1032(%ebx)",
      "movl", "%esi,(%eax)",
  INL_LABEL(4),
      "movl", "$14131,(%esi)",
      "jmp", INL_LABEL(7),
      ".p2align", "2",
  INL_LABEL(5),
      "cmpl", "$14131,%ecx",
      "sete", "%al",
      "movzbl", "%al,%eax",
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(6),
      "movl", "1024(%ebx),%eax",
      "leal", "4(%eax),%edx",
      "movl", "%edx,1024(%ebx)",
      "movl", "$14131,(%eax)",
  INL_LABEL(7),
      "movl", "$1,%eax",
  INL_LABEL(8),
  INL_END_FUNC,

  "Allocate", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "leal", "0(,%eax,4),%ecx",
      "movl", "1040(%ebx),%edi",
      "movl", "%edi,%eax",
      "movl", "1028(%ebx),%edx",
      "movl", "%eax,%esi",
      "cmpl", "%eax,%edx",
      "jb", INL_LABEL(1),
      "leal", "12(%edx,%ecx),%eax",
      "jmp", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "leal", "12(%esi,%ecx),%eax",
  INL_LABEL(2),
      "movl", "%eax,1040(%ebx)",
      "movl", "%eax,%edx",
      "movl", "1036(%ebx),%eax",
      "movl", "%eax,-4(%edx)",
      "movl", "1056(%ebx),%eax",
      "movl", "1040(%ebx),%edx",
      "movl", "%eax,-8(%edx)",
      "movl", "1040(%ebx),%eax",
      "movl", "%edi,-12(%eax)",
  INL_END_FUNC,

  "Deallocate", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1040(%ebx),%eax",
      "movl", "-4(%eax),%eax",
      "movl", "%eax,1036(%ebx)",
      "movl", "1040(%ebx),%eax",
      "movl", "-8(%eax),%eax",
      "movl", "%eax,1056(%ebx)",
      "movl", "1040(%ebx),%eax",
      "movl", "-12(%eax),%eax",
      "movl", "%eax,1040(%ebx)",
  INL_END_FUNC,

  "Switch_On_Atom", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "leal", "1(%edx),%ebp",
      "movl", "%eax,%edi",
      "movl", "(%ebx),%esi",
      "sarl", "$3,%esi",
      "movl", "%esi,%eax",
      "cltd", "",
      "idivl", "%ebp",
      "leal", "(%edi,%edx,8),%ecx",
      "leal", "(%edi,%ebp,8),%eax",
      "jmp", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "addl", "$8,%ecx",
      "cmpl", "%eax,%ecx",
      "jne", INL_LABEL(2),
      "movl", "%edi,%ecx",
  INL_LABEL(2),
      "cmpl", "$0,4(%ecx)",
      "je", INL_LABEL(3),
      "cmpl", "%esi,(%ecx)",
      "jne", INL_LABEL(1),
  INL_LABEL(3),
      "movl", "4(%ecx),%eax",
      "testl", "%eax,%eax",
      "jne", INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "-4(%eax),%eax",
  INL_LABEL(4),
  INL_END_FUNC,

  "Switch_On_Integer", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "(%ebx),%eax",
      "sarl", "$3,%eax",
  INL_END_FUNC,

  "Switch_On_Structure", INL_NEXT, INL_LEVEL(2), INL_INFO(1),
      "leal", "1(%edx),%ebp",
      "movl", "%eax,%edi",
      "movl", "(%ebx),%eax",
      "shrl", "$3,%eax",
      "movl", "(%eax),%esi",
      "movl", "%esi,%eax",
      "cltd", "",
      "idivl", "%ebp",
      "leal", "(%edi,%edx,8),%ecx",
      "leal", "(%edi,%ebp,8),%eax",
      "jmp", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "addl", "$8,%ecx",
      "cmpl", "%eax,%ecx",
      "jne", INL_LABEL(2),
      "movl", "%edi,%ecx",
  INL_LABEL(2),
      "cmpl", "$0,4(%ecx)",
      "je", INL_LABEL(3),
      "cmpl", "%esi,(%ecx)",
      "jne", INL_LABEL(1),
  INL_LABEL(3),
      "movl", "4(%ecx),%eax",
      "testl", "%eax,%eax",
      "jne", INL_LABEL(4),
      "movl", "1028(%ebx),%eax",
      "movl", "-4(%eax),%eax",
  INL_LABEL(4),
  INL_END_FUNC,

  "Load_Cut_Level", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "1028(%ebx),%edx",
      "sall", "$3,%edx",
      "movl", "%edx,(%eax)",
  INL_END_FUNC,

  "Cut", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "shrl", "$3,%eax",
      "movl", "%eax,1028(%ebx)",
  INL_END_FUNC,

  "Create_Choice_Point", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%eax,%ebp",
      "movl", "%edx,%edi",
      "movl", "1028(%ebx),%eax",
      "leal", "0(,%edi,4),%edx",
      "movl", "%eax,12(%esp)",	/* a local var - not changed could be at 0 */
      "movl", "1040(%ebx),%eax",
      "movl", "1028(%ebx),%esi",
      "movl", "%eax,%ecx",
      "cmpl", "%eax,%esi",
      "jb", INL_LABEL(1),
      "leal", "32(%esi,%edx),%ecx",
      "jmp", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "leal", "32(%ecx,%edx),%ecx",
  INL_LABEL(2),
      "movl", "%ecx,1028(%ebx)",
      "xorl", "%edx,%edx",
      "movl", "%ebp,-4(%ecx)",
      "movl", "1036(%ebx),%eax",
      "movl", "%eax,-8(%ecx)",
      "movl", "1056(%ebx),%eax",
      "movl", "%eax,-12(%ecx)",
      "movl", "1040(%ebx),%eax",
      "movl", "%eax,-16(%ecx)",
      "movl", "12(%esp),%eax",
      "movl", "%eax,-20(%ecx)",
      "movl", "1024(%ebx),%eax",
      "movl", "%eax,-24(%ecx)",
      "movl", "1032(%ebx),%eax",
      "movl", "%eax,-28(%ecx)",
      "movl", "1044(%ebx),%eax",
      "movl", "%eax,-32(%ecx)",
      "incl", "1052(%ebx)",
      "cmpl", "%edi,%edx",
      "jge", INL_LABEL(4),
      "addl", "$-36,%ecx",
      ".p2align", "2",
  INL_LABEL(3),
      "movl", "(%ebx,%edx,4),%eax",
      "movl", "%eax,(%ecx)",
      "incl", "%edx",
      "addl", "$-4,%ecx",
      "cmpl", "%edi,%edx",
      "jl", INL_LABEL(3),
  INL_LABEL(4),
  INL_END_FUNC,

  "Update_Choice_Point", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%edx,%edi",
      "movl", "1028(%ebx),%esi",
      "movl", "%eax,-4(%esi)",
      "movl", "-28(%esi),%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
#endif
      "call", "Untrail",
      "movl", "-8(%esi),%eax",
      "xorl", "%ecx,%ecx",
      "movl", "%eax,1036(%ebx)",
      "movl", "-12(%esi),%eax",
      "movl", "%eax,1056(%ebx)",
      "movl", "-16(%esi),%eax",
      "movl", "%eax,1040(%ebx)",
      "movl", "-24(%esi),%eax",
      "movl", "%eax,1024(%ebx)",
      "movl", "-32(%esi),%eax",
      "movl", "%eax,1044(%ebx)",
      "cmpl", "%edi,%ecx",
      "jge", INL_LABEL(2),
      "leal", "-36(%esi),%edx",
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "(%edx),%eax",
      "movl", "%eax,(%ebx,%ecx,4)",
      "addl", "$-4,%edx",
      "incl", "%ecx",
      "cmpl", "%edi,%ecx",
      "jl", INL_LABEL(1),
  INL_LABEL(2),
  INL_END_FUNC,

  "Delete_Choice_Point", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%eax,%edi",
      "movl", "1028(%ebx),%esi",
      "movl", "-28(%esi),%eax",
#ifndef FC_USED_TO_COMPILE_CORE
      "movl", "%eax,0(%esp)",
#endif
      "call", "Untrail",
      "movl", "-8(%esi),%eax",
      "xorl", "%ecx,%ecx",
      "movl", "%eax,1036(%ebx)",
      "movl", "-16(%esi),%eax",
      "movl", "%eax,1040(%ebx)",
      "movl", "-20(%esi),%eax",
      "movl", "%eax,1028(%ebx)",
      "movl", "-24(%esi),%eax",
      "movl", "%eax,1024(%ebx)",
      "movl", "-32(%esi),%eax",
      "movl", "%eax,1044(%ebx)",
      "movl", "-12(%esi),%eax",
      "movl", "%eax,1056(%ebx)",
      "decl", "1052(%ebx)",
      "cmpl", "%edi,%ecx",
      "jge", INL_LABEL(2),
      "leal", "-36(%esi),%edx",
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "(%edx),%eax",
      "movl", "%eax,(%ebx,%ecx,4)",
      "addl", "$-4,%edx",
      "incl", "%ecx",
      "cmpl", "%edi,%ecx",
      "jl", INL_LABEL(1),
  INL_LABEL(2),
  INL_END_FUNC,

/* Switch_On_Term has been modified as described in the general comment */

  "Switch_On_Term", INL_NEXT, INL_LEVEL(1), INL_INFO(1),
      "movl", "%edx,%edi",
      "movl", "%eax,16(%esp)",	/* adjust offset - local var */
      "movl", "%ecx,%ebp",
      "movl", "(%ebx),%esi",
      "movl", "%esi,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%esi,%eax",
      "shrl", "$3,%eax",
      "je", INL_LABEL(2),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "%eax,%ecx",
      "movl", "(%ecx),%esi",
      "movl", "%esi,%edx",
      "andl", "$7,%edx",
      "cmpl", "$1,%edx",
      "jne", INL_LABEL(2),
      "movl", "%esi,%eax",
      "shrl", "$3,%eax",
      "cmpl", "%ecx,%eax",
      "jne", INL_LABEL(1),
  INL_LABEL(2),
      "movl", "%esi,(%ebx)",
      "cmpl", "$3,%edx",
      "je", INL_LABEL(5),
      "jg", INL_LABEL(3),
      "testl", "%edx,%edx",
      "je", INL_LABEL(4),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(3),
      "cmpl", "$5,%edx",
      "je", INL_LABEL(6),
      "cmpl", "$6,%edx",
      "je", INL_LABEL(7),
      "jmp", INL_LABEL(8),
      ".p2align", "2",
  INL_LABEL(4),
      "movl", "%ebp,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(5),
      "movl", "%edi,%eax",
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(6),
      "movl", "0(%esp),%eax",	/* adjust offset - 4th arg (1st in stack) */
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(7),
      "movl", "4(%esp),%eax",	/* adjust offset - 5th arg (2nd in stack) */
      "jmp", INL_LABEL(9),
      ".p2align", "2",
  INL_LABEL(8),
      "movl", "16(%esp),%eax",	/* adjust offset - local var */
  INL_LABEL(9),
      "testl", "%eax,%eax",
      "jne", INL_LABEL(10),
      "movl", "1028(%ebx),%eax",
      "movl", "-4(%eax),%eax",
  INL_LABEL(10),
  INL_END_FUNC,


/* Unify_Void is generated separately without -e ret since it has an 
 * intermediate ret. Replace first ret by jmp INL_LABEL(3) (corresponds to 
 * the final ret) and remove final ret */

  "Unify_Void", INL_NEXT, INL_LEVEL(1), INL_INFO(0),
      "movl", "%eax,%edx",
      "movl", "1048(%ebx),%eax",
      "testl", "%eax,%eax",
      "je", INL_LABEL(1),
      "leal", "(%eax,%edx,4),%eax",
      "movl", "%eax,1048(%ebx)",
      "jmp", INL_LABEL(3),
      ".p2align", "2",
  INL_LABEL(1),
      "movl", "1024(%ebx),%eax",
      "leal", "(%eax,%edx,4),%ecx",
      "cmpl", "%ecx,%eax",
      "jae", INL_LABEL(3),
      ".p2align", "2",
  INL_LABEL(2),
      "movl", "1024(%ebx),%edx",
      "movl", "%edx,%eax",
      "sall", "$3,%eax",
      "orb", "$1,%al",
      "movl", "%eax,(%edx)",
      "addl", "$4,1024(%ebx)",
      "cmpl", "%ecx,1024(%ebx)",
      "jb", INL_LABEL(2),
  INL_LABEL(3),
  INL_END_FUNC,

  NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static char *Off_Reg_Bank(int offset);




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK UN "reg_bank"
#elif defined(MAP_REG_BANK)
#define ASM_REG_BANK "%" MAP_REG_BANK
#else
#define ASM_REG_BANK "%ebx"
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%%%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "%edi");
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%%%s", MAP_REG_B);
#else
  strcpy(asm_reg_b, Off_Reg_Bank(MAP_OFFSET_B));
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%%%s", MAP_REG_CP);
#else
  strcpy(asm_reg_cp, Off_Reg_Bank(MAP_OFFSET_CP));
#endif

  Label_Printf(".text");

  Label("fail");
  Pl_Fail();
}




/*-------------------------------------------------------------------------*
 * OFF_REG_BANK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Off_Reg_Bank(int offset)
{
  static char str[16];

#ifdef NO_MACHINE_REG_FOR_REG_BANK
  sprintf(str, ASM_REG_BANK "+%d", offset);
#else
  sprintf(str, "%d(%s)", offset, ASM_REG_BANK);
#endif

  return str;
}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{
  Label_Printf("");
  Inst_Printf(".align", "4");
#if defined(M_ix86_linux) || defined(M_ix86_sco)
  Inst_Printf(".type", UN "%s,@function", label);
#endif

  if (global)
    Inst_Printf(".globl", UN "%s", label);

  Label(label);

  if (!prolog)
    Inst_Printf("subl", "$%d,%%esp", MAX_C_ARGS_IN_C_CODE * 4);
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(void)
{
}




/*-------------------------------------------------------------------------*
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("");
#if 0
  Inst_Printf(".align", "4");
#endif
  Label_Printf(UN "%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Inst_Printf("movl", "%s,%s", Off_Reg_Bank(MAP_OFFSET_E), asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * PL_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call(char *label)
{
  Inst_Printf("movl", "$.Lcont%d,%s", w_label, asm_reg_cp);
  Pl_Jump(label);
  Label_Printf(".Lcont%d:", w_label++);
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Inst_Printf("jmp", "*-4(%s)", asm_reg_b);
#else
  Inst_Printf("movl", "%s,%%eax", asm_reg_b);
  Inst_Printf("jmp", "*-4(%%eax)");
#endif
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("jmp", "*%s", asm_reg_cp);
#else
  Inst_Printf("jmp", "%s", asm_reg_cp);
#endif
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("movl", "%s,%%eax", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movl", "-%d(%s),%%eax", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movl", "%%eax,%s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movl", "%%eax,-%d(%s)", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline)
{
#ifndef FC_USED_TO_COMPILE_CORE
  if (p_inline == NULL)		/* inlined code used a fast call */
    fc = 0;
#endif
  inside_fc = fc;
  fc_off_in_stack = 0;
}



#define BEFORE_ARG                                                          \
{                                                                           \
  char r[10], *r_aux;                                                       \
  int  r_eq_r_aux = 0;                                                      \
  int off1 = offset;                                                        \
                                                                            \
  if (inside_fc)                                                            \
    {                                                                       \
      if (offset < FC_MAX_ARGS_IN_REGS)                                     \
        {                                                                   \
  	  strcpy(r, fc_arg_regs[offset]);                                   \
          r_aux = r;                                                        \
          r_eq_r_aux = 1;                                                   \
        }                                                                   \
      else                                                                  \
        {                                                                   \
          off1 = (offset - FC_MAX_ARGS_IN_REGS + fc_off_in_stack);          \
	  sprintf(r, "%d(%%esp)", off1 * 4);                                \
          r_aux = "%esi";                                                   \
        }                                                                   \
    }                                                                       \
  else                                                                      \
    {                                                                       \
      sprintf(r, "%d(%%esp)", offset * 4);                                  \
      r_aux = "%eax";                                                       \
    }


#define BEFORE_HALF_ARG_DOUBLE                                              \
{                                                                           \
  char r[10];                                                               \
  int off1 = offset;                                                        \
                                                                            \
  if (inside_fc)                                                            \
    {                                                                       \
      if (offset < FC_MAX_ARGS_IN_REGS)                                     \
        fc_off_in_stack++;                                                  \
      else                                                                  \
        off1 = (offset - FC_MAX_ARGS_IN_REGS + fc_off_in_stack);            \
    }                                                                       \
  sprintf(r, "%d(%%esp)", off1 * 4);
  

#define AFTER_ARG                                                           \
}
    




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  BEFORE_ARG;

  Inst_Printf("movl", "$%ld,%s", int_val, r);

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, double dbl_val)
{
  int *p = (int *) &dbl_val;


  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d,%s", p[0], r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d,%s", p[1], r);

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  BEFORE_ARG;

  Inst_Printf("movl", "$%s%d,%s", STRING_PREFIX, str_no, r);

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("movl", "$" UN "%s+%d,%s", name, index * 4, r);
  else
    {
      Inst_Printf("movl", UN "%s+%d,%s", name, index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s,%s", r_aux, r);
    }

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    {
      if (!r_eq_r_aux && index == 0)
	{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
	  Inst_Printf("movl", "$%s,%s", ASM_REG_BANK, r);
#else
	  Inst_Printf("movl", "%s,%s", ASM_REG_BANK, r);
#endif
	  goto finish;
	}
      Inst_Printf("leal", "%s,%s", Off_Reg_Bank(index * 4), r_aux);
    }
  else
    Inst_Printf("movl", "%s,%s", Off_Reg_Bank(index * 4), r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);

 finish:  
  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("leal", "-%d(%s),%s", (index + 4) * 4, asm_reg_e, r_aux);
  else
    Inst_Printf("movl", "-%d(%s),%s", (index + 4) * 4, asm_reg_e, r_aux);
  
  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);
  
  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("movl", "$" UN "foreign_long+%d,%s", index * 4, r);
  else
    {
      Inst_Printf("movl", UN "foreign_long+%d,%s", index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s,%s", r_aux, r);
    }
  
  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  if (adr_of)
    {
      BEFORE_ARG;

      Inst_Printf("movl", "$" UN "foreign_double+%d,%s", index * 8, r);

      AFTER_ARG;

      return 1;
    }

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8);
  Inst_Printf("movl", "%%eax,%s", r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8 + 4);
  Inst_Printf("movl", "%%eax,%s", r);

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int nb_args)
{
  Inst_Printf("call", UN "%s", fct_name);
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args, char **p_inline)
{
#ifndef MAP_REG_E
  if (p_inline && INL_ACCESS_INFO(p_inline))
    reload_e = 1;
#endif
}




/*-------------------------------------------------------------------------*
 * JUMP_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_Ret(void)
{
  Inst_Printf("jmp", "*%%eax");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("testl", "%%eax,%%eax");
  Inst_Printf("je", UN "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("movl", "%%eax," UN "%s+%d", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("movl", "%%eax,%s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("movl", "%%eax,-%d(%s)", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("movl", "%%eax," UN "foreign_long+%d", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("fstpl", UN "foreign_double+%d", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(long int_val)
{
  if (int_val == 0)
    Inst_Printf("testl", "%%eax,%%eax");
  else
    Inst_Printf("cmpl", "$%ld,%%eax", int_val);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("je", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("jg", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("addl", "$%d,%%esp", MAX_C_ARGS_IN_C_CODE * 4);
  Inst_Printf("ret", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
#if !defined(M_ix86_bsd)
  Label_Printf(".section\t.rodata");
#endif
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{
  Label_Printf("%s%d:", STRING_PREFIX, str_no);
  Inst_Printf(".string", "%s", asciiz);
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_STOP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Stop(int nb_consts)
{
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_START                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Start(int nb_longs)
{
  Label_Printf(".data");
  Inst_Printf(".align", "4");
}




/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, long value)
{
  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
#if defined(M_ix86_linux) || defined(M_ix86_sco)
      if (!global)
	Inst_Printf(".local", UN "%s", name);
      Inst_Printf(".comm", UN "%s,%ld,4", name, value * 4);
#else
      if (!global)
	Inst_Printf(".lcomm", UN "%s,%ld", name, value * 4);
      else
	Inst_Printf(".comm", UN "%s,%ld", name, value * 4);
#endif
      break;

    case INITIAL_VALUE:
      if (global)
	Inst_Printf(".globl", UN "%s", name);
#ifdef M_ix86_cygwin
      Inst_Printf(".align", "4");
#endif
      Label_Printf(UN "%s:", name);
      Inst_Printf(".long", "%ld", value);
      break;
    }
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_STOP                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Stop(int nb_longs)
{
}




/*-------------------------------------------------------------------------*
 * DATA_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Start(char *initializer_fct)
{
  if (initializer_fct == NULL)
    return;

  Label_Printf(".data");
  Label_Printf(UN "obj_chain_start:");

  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_1);
  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_2);
  Inst_Printf(".long", UN "obj_chain_stop");
  Inst_Printf(".long", UN "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
  if (initializer_fct == NULL)
    return;

  Label_Printf(".data");
  Label_Printf(UN "obj_chain_stop:");

  Inst_Printf(".long", UN "obj_chain_start");
}
