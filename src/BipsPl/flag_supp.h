/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : flag_supp.h                                                     */
/* Descr.: Prolog flag and system variable support - header file           */
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


/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

#define FLAG_BOUNDED               0        /* flags defining integer type */
#define FLAG_MAX_INTEGER           1
#define FLAG_MIN_INTEGER           2
#define FLAG_ROUNDING_FCT          3

#define FLAG_CHAR_CONVERSION       4                        /* other flags */
#define FLAG_DEBUG                 5
#define FLAG_MAX_ARITY             6
#define FLAG_UNKNOWN               7
#define FLAG_DOUBLE_QUOTES         8

#define FLAG_SYNTAX_ERROR          9                      /* non ISO flags */
#define FLAG_OS_ERROR              10
#define FLAG_MAX_ATOM              11
#define FLAG_MAX_STREAM            12
#define FLAG_MAX_UNGET             13
#define FLAG_SINGLETON_WARNING     14
#define FLAG_STRICT_ISO            15

#define FLAG_PROLOG_NAME           16
#define FLAG_PROLOG_VERSION        17
#define FLAG_PROLOG_DATE           18
#define FLAG_PROLOG_COPYRIGHT      19

#define NB_OF_FLAGS                20



#define FLAG_VALUE_ERROR           0           /* same order as in read.pl */
#define FLAG_VALUE_WARNING         1
#define FLAG_VALUE_FAIL            2


#define FLAG_DOUBLE_QUOTES_CODES   0
#define FLAG_DOUBLE_QUOTES_CHARS   1
#define FLAG_DOUBLE_QUOTES_ATOM    2




#define Char_Conversion(c)         ((Flag_Value(FLAG_CHAR_CONVERSION) &&    \
                                    Is_Valid_Code(c)) ? char_conv[c] : (c))



#define SYS_VAR_OPTION_MASK         (sys_var[0])

#define SYS_VAR_WRITE_DEPTH         (sys_var[1])
#define SYS_VAR_SYNTAX_ERROR_ACTON  (sys_var[1])

#define SYS_VAR_FD_BCKTS            (sys_var[3])

#define SYS_VAR_PRINT_STM           (sys_var[6])

#define SYS_VAR_TOP_LEVEL           (sys_var[10])
#define SYS_VAR_LINEDIT             (sys_var[12])
#define SYS_VAR_DEBUGGER            (sys_var[13])

#define Flag_Value(flag)            (sys_var[200+(flag)])




/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

#ifdef FLAG_C_FILE

       long sys_var[MAX_SYS_VARS];

#else

extern long sys_var[];

#endif




/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

/*-------------------------------------------------------------------------*/
/* System variables (C and Prolog) - bank description                      */
/*                                                                         */
/*   0: temporary (e.g. masks for option lists (open/read/write)).         */
/*   1: temporary (e.g. depth in write).                                   */
/*   2: temporary (e.g. reorder in FD labeling).                           */
/*   3: temporary (e.g. backtracks counter in FD labeling).                */
/*                                                                         */
/*   6: pseudo-permanent current print stream (for get_print_stream/1)     */
/*                                                                         */
/*   7: permanent catch handler.                                           */
/*   8: permanent catch ball.                                              */
/*                                                                         */
/*  10: permanent top level depth (for top-level and stop/abort).          */
/*  11: permanent top level handler (B level) for abort and stop.          */
/*  12: permanent: is linedit present ?                                    */
/*  13: permanent: is the debugger present ?                               */
/*                                                                         */
/* 200..: some prolog flag values.                                         */
/*-------------------------------------------------------------------------*/

