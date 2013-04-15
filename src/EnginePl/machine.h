/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.h                                                       *
 * Descr.: machine dependent features - Header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
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


#ifndef _MACHINE_H
#define _MACHINE_H

#include "bool.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Init_Machine(void);

char *Pl_M_Sys_Err_String(int ret_val);

PlLong Pl_M_User_Time(void);

PlLong Pl_M_System_Time(void);

PlLong Pl_M_Real_Time(void);

void Pl_M_Randomize(void);

void Pl_M_Set_Seed(int n);

int Pl_M_Get_Seed(void);

int Pl_M_Random_Integer(int n);

double Pl_M_Random_Float(double n);



char *Pl_M_Host_Name_From_Name(char *host_name);

char *Pl_M_Host_Name_From_Adr(char *host_address);

char *Pl_M_Get_Working_Dir(void);

Bool Pl_M_Set_Working_Dir(char *path);

char *Pl_M_Absolute_Path_Name(char *src);

Bool Pl_M_Is_Absolute_File_Name(char *path);

char *Pl_M_Decompose_File_Name(char *path, Bool del_trail_slashes, 
			       char **base, char **suffix);

#if defined(_WIN32) && !defined(__CYGWIN__)

int getpagesize(void);

#endif

void M_Check_Magic_Words(void); /* not compiled if not needed */




/*---------------------------------*
 * Register Definitions            *
 *---------------------------------*/

#if defined(M_sparc)

#    define M_USED_REGS            {"g6", "g7", 0}

#elif defined(M_mips)

#define M_USED_REGS                {"$16", "$17", "$18", "$19", "$20", \
                                    "$21", "$22", "$23", 0}

#elif defined(M_alpha)

#    define M_USED_REGS            {"$9", "$10", "$11", "$12", "$13", "$14", 0}

/* on M_ix86_darwin : %ebx is used by gcc for pic base */
#elif defined(M_ix86) && !defined(_MSC_VER) && !defined(M_ix86_darwin)

#ifdef NO_USE_EBP
#    define M_USED_REGS            {"ebx", 0}
#else
#    define M_USED_REGS            {"ebx", "ebp", 0}
#endif

#elif defined(M_powerpc)

#    define M_USED_REGS            {"15", "20", 0}

/* on M_x86_64_darwin Lion r12-r15 do not work (why ?) */
#elif defined(M_x86_64) && !defined(_MSC_VER) && !defined(M_x86_64_darwin)

#    define M_USED_REGS            {"r12", "r13", "r14", "r15", 0}

#else

#    define M_USED_REGS            {0}

#endif



#if defined(M_ix86) // && !defined(_WIN32) // && !defined(NO_USE_REGS)
#define NO_MACHINE_REG_FOR_REG_BANK
#endif

/* In any case M_x86_64_darwin needs a reg for pl_reg_bank (default is r12)
 * else Ma2Asm produces code ending with the following error:
 * '32-bit absolute addressing is not supported for x86-64'
 */
#if defined(NO_USE_REGS) && !defined(NO_MACHINE_REG_FOR_REG_BANK) && \
  defined(M_x86_64) && !defined(M_x86_64_darwin)
#define NO_MACHINE_REG_FOR_REG_BANK
#endif




/*---------------------------------*
 * Stacks Management               *
 *---------------------------------*/

#if WORD_SIZE == 32

#   define M_MMAP_HIGH_ADR1        0x0ffffff0
#   define M_MMAP_HIGH_ADR2        0x3ffffff0
#   define M_MMAP_HIGH_ADR3        0x7ffffff0

#elif defined(M_alpha_osf) || defined(M_alpha_linux)

#   define M_MMAP_HIGH_ADR1        0x3f800000000ULL

#elif defined(M_x86_64_linux) || defined(M_x86_64_solaris)

#   define M_MMAP_HIGH_ADR1        0x4000000000ULL

#endif

#if defined(M_sunos) || defined(M_solaris)
#   define MMAP_NEEDS_FIXED
#endif



/*---------------------------------*
 * Malloc Management               *
 *---------------------------------*/

#if defined(__OpenBSD__) || defined(M_bsd)
#define USE_DL_MALLOC
#endif


#endif
