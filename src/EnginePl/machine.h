/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.h                                                       *
 * Descr.: machine dependent features - Header file                        *
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

#ifdef M_ix86_win32

int getpagesize(void);

int Is_Win32_SEGV(void *exp);

void SIGSEGV_Handler(void);

#endif




/*---------------------------------*
 * Register Definitions            *
 *---------------------------------*/

#if defined(M_sparc_sunos)

#    define M_USED_REGS            {"g6", "g7", 0}

#elif defined(M_sparc_solaris)

#    define M_USED_REGS            {"g6", "g7", 0}

#elif defined(M_mips_irix)

#define M_USED_REGS                {"$16", "$17", "$18", "$19", "$20", \
                                    "$21", "$22", "$23", 0}

#elif defined(M_alpha_linux)

#    define M_USED_REGS            {"$9", "$10", "$11", "$12", "$13", "$14", 0}

#elif defined(M_alpha_osf)

#    define M_USED_REGS            {"$9", "$10", "$11", "$12", "$13", "$14", 0}

#elif defined(M_ix86_linux)   || defined(M_ix86_sco) || \
      defined(M_ix86_solaris) || defined(M_ix86_cygwin)

#ifdef NO_USE_EBP
#    define M_USED_REGS            {"ebx", 0}
#else
#    define M_USED_REGS            {"ebx", "ebp", 0}
#endif

#elif defined(M_powerpc_linux)

#    define M_USED_REGS            {"15", "20", 0}

#else

#    define M_USED_REGS            {0}

#endif


#if defined(M_ix86) && !defined(M_ix86_win32) && !defined(NO_USE_REGS)
#define NO_MACHINE_REG_FOR_REG_BANK
#endif




/*---------------------------------*
 * Stacks Management               *
 *---------------------------------*/

#if defined(M_sparc_sunos) || defined(M_sparc_solaris) || \
    defined(M_ix86_linux)  || defined(M_powerpc_linux) || \
    defined(M_ix86_sco)    || defined(M_ix86_solaris)  || \
    defined(M_mips_irix)   || \
    defined(M_ix86_win32)

#   define M_USE_MMAP
#   define M_MMAP_HIGH_ADR         0x0ffffff0
#   define M_MMAP_HIGH_ADR_ALT     0x3ffffff0
#   define M_Check_Stacks()

#elif defined(M_alpha_osf) || defined(M_alpha_linux)

#   define M_USE_MMAP
#   define M_MMAP_HIGH_ADR         0x3f800000000ULL
#   define M_Check_Stacks()

#else

#   define M_USE_MALLOC
#   define M_Check_Stacks()        M_Check_Magic_Words()

#endif




#ifdef M_USE_MALLOC

#define M_USE_MAGIC_NB_TO_DETECT_STACK_NAME
void M_Check_Magic_Words(void);

#endif
