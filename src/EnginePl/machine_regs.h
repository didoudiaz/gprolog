/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine_regs.h                                                  *
 * Descr.: machine register usage - header file                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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
 * For a machine define the set of registers mappable to WAM registers
 * as Global Variable Registers (GRV) - currently only GCC support them.
 * Chose processor global registers or callee-saved registers.
 *
 * In case GRV are not available, it is however possible to set some
 * registers in the trampoline (see trampoline.S).
 * An interesting case is a register to store the address of pl_reg_bank.
 * For this, define M_TRAMPOLINE_REG_BANK with a callee-saved register
 * (do not use double-quotes as it is expanded in trampoline.S).
 */


#ifndef _MACHINE_REGS_H
#define _MACHINE_REGS_H

#if defined(M_sparc32)

#    define M_USED_REGS            { "%g6", "%g7", 0 }
#    define M_TRAMPOLINE_REG_BANK  %l0

#elif defined(M_sparc64)

#    define M_USED_REGS            { "%l0" }
#    define M_TRAMPOLINE_REG_BANK  %l0

#elif defined(M_mips32)

#define M_USED_REGS                { "$16", "$17", "$18", "$19", "$20", \
                                     "$21", "$22", "$23", 0 }
#    define M_TRAMPOLINE_REG_BANK     $16

#elif defined(M_alpha)

#    define M_USED_REGS            { "$9", "$10", "$11", "$12", "$13", "$14", 0 }
#    define M_TRAMPOLINE_REG_BANK     $9

/* on M_ix86_darwin : %ebx is used by gcc for pic base */
#elif defined(M_ix86) /* TODO CHECKME  && !defined(M_ix86_darwin) */

#ifdef NO_USE_EBP
#    define M_USED_REGS            { "%ebx", 0 }
#else
#    define M_USED_REGS            { "%ebx", "%ebp", 0 }
#endif
#    define M_TRAMPOLINE_REG_BANK    %ebx

#elif defined(M_ppc32)

#    define M_USED_REGS            { "15", "20", 0 }
#    define M_TRAMPOLINE_REG_BANK     15

#elif defined(M_arm32)

	/* do not use r7 (frame pointer in Thumb code) */
#    define M_USED_REGS            { "r5", "r6", "r8", "r9", "r10", 0 }
#    define M_TRAMPOLINE_REG_BANK    r10

#elif defined(M_arm64)

	/* do not use x29 (frame pointer) */
#    define M_USED_REGS            { "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", 0 }
#    define M_TRAMPOLINE_REG_BANK     x20


#elif defined(M_riscv64)

	/* using s1-s5 causes problem with CTRL+C with -O3 */
#    define M_USED_REGS            { "s6", "s7", "s8", "s9", "s10", "s11", 0 }
#    define M_TRAMPOLINE_REG_BANK     s6

#elif defined(M_x86_64)

#    define M_USED_REGS            { "%r12", "%r13", "%r14", "%r15", 0 }
#    define M_TRAMPOLINE_REG_BANK    %r12

#endif	/* end switch on processor */




	/* clang/llvm do not handle Global Register Variables */
#if (!defined(__GNUC__) || defined(__clang__)) && !defined(NO_USE_REGS)
#   define NO_USE_REGS
#endif



#ifdef NO_USE_REGS

#    undef M_USED_REGS
#    define M_USED_REGS            { PP_STR(M_TRAMPOLINE_REG_BANK), 0 }

#endif

#endif
