/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.h                                                       *
 * Descr.: machine dependent features - Header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

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

void Init_Machine(void);

void M_Allocate_Stacks(void);

char *M_Sys_Err_String(int err_no);

long M_User_Time(void);

long M_System_Time(void);

long M_Real_Time(void);

void M_Randomize(void);

void M_Set_Seed(int n);

int M_Get_Seed(void);

int M_Random_Integer(int n);

double M_Random_Float(double n);



char *M_Host_Name_From_Name(char *host_name);

char *M_Host_Name_From_Adr(char *host_address);

char *M_Get_Working_Dir(void);

Bool M_Set_Working_Dir(char *path);

char *M_Absolute_Path_Name(char *src);

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

#elif defined(M_x86_64)

#    define M_USED_REGS            {"r12", "r13", "r14", "r15", 0}

#else

#    define M_USED_REGS            {0}

#endif


#if defined(M_ix86) && !defined(_WIN32) && !defined(NO_USE_REGS)
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

#ifdef __OpenBSD__
#define USE_DL_MALLOC
#endif


#endif
