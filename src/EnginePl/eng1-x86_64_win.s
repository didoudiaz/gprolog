/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : eng1-x86_64_win.c                                               *
 * Descr.: general engine (assembly part)                                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2011 Daniel Diaz                                     *
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
