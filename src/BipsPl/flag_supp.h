/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_supp.h                                                     *
 * Descr.: Prolog flag and system variable support - header file           *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

enum
{
  FLAG_BOUNDED,
  FLAG_MAX_INTEGER,
  FLAG_MIN_INTEGER,
  FLAG_ROUNDING_FCT,

  FLAG_CHAR_CONVERSION,
  FLAG_DEBUG,
  FLAG_MAX_ARITY,
  FLAG_UNKNOWN,
  FLAG_DOUBLE_QUOTES,
  FLAG_BACK_QUOTES,

  FLAG_SYNTAX_ERROR,
  FLAG_OS_ERROR,
  FLAG_MAX_ATOM,
  FLAG_MAX_UNGET,
  FLAG_SINGLETON_WARNING,
  FLAG_SUSPICIOUS_WARNING,
  FLAG_MULTIFILE_WARNING,
  FLAG_STRICT_ISO,

  FLAG_PROLOG_NAME,
  FLAG_PROLOG_VERSION,
  FLAG_PROLOG_DATE,
  FLAG_PROLOG_COPYRIGHT,
  FLAG_DIALECT,
  FLAG_HOME,
  FLAG_HOST_CPU,
  FLAG_HOST_VENDOR,
  FLAG_HOST_OS,
  FLAG_HOST,
  FLAG_ARCH,
  FLAG_COMPILED_AT,
  FLAG_C_CC,
  FLAG_C_CFLAGS,
  FLAG_C_LDFLAGS,

  FLAG_ADDRESS_BITS,

  FLAG_VERSION_DATA,
  FLAG_VERSION,
  FLAG_UNIX,

  FLAG_ARGV,

  NB_OF_FLAGS			/* this gives us the number of used flags */
};



#define FLAG_VALUE_ERROR           0	/* same order as in read.pl */
#define FLAG_VALUE_WARNING         1
#define FLAG_VALUE_FAIL            2


     /* values for double_quotes and back_quotes */
#define FLAG_AS_CODES              0	/* bit 2 is set if no_escape */
#define FLAG_AS_CHARS              1
#define FLAG_AS_ATOM               2
#define FLAG_NO_ESCAPE_BIT         2
#define FLAG_AS_PART_MASK          ((1 << FLAG_NO_ESCAPE_BIT) - 1)
#define FLAG_NO_ESCAPE_MASK        (1 << FLAG_NO_ESCAPE_BIT)


#define Char_Conversion(c)         ((Flag_Value(FLAG_CHAR_CONVERSION) &&    \
                                    Is_Valid_Code(c)) ? pl_char_conv[c] : (c))



#define SYS_VAR_OPTION_MASK         (pl_sys_var[0])

#define SYS_VAR_WRITE_DEPTH         (pl_sys_var[1])
#define SYS_VAR_SYNTAX_ERROR_ACTON  (pl_sys_var[1])

#define SYS_VAR_WRITE_PREC          (pl_sys_var[2])

#define SYS_VAR_WRITE_ABOVE         (pl_sys_var[3])

#define SYS_VAR_FD_BCKTS            (pl_sys_var[4])

#define SYS_VAR_TOP_LEVEL           (pl_sys_var[10])
#define SYS_VAR_LINEDIT             (pl_sys_var[12])
#define SYS_VAR_DEBUGGER            (pl_sys_var[13])

#define SYS_VAR_SAY_GETC            (pl_sys_var[20])
#define CHAR_TO_EMIT_WHEN_CHAR      '\1'

#define Flag_Value(flag)            (pl_sys_var[200 + (flag)])




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef FLAG_C_FILE

PlLong pl_sys_var[MAX_SYS_VARS];

#else

extern PlLong pl_sys_var[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

Bool Pl_Read_Pl_State_File(WamWord file_word);
Bool Pl_Write_Pl_State_File(WamWord file_word);




/*-------------------------------------------------------------------------*
 * System variables (C and Prolog) - bank description                      *
 *                                                                         *
 *   0: temporary (e.g. masks for option lists (open/read/write)).         *
 *   1: temporary (e.g. depth in write).                                   *
 *   2: temporary (e.g. reorder in FD labeling).                           *
 *   3: temporary (e.g. write '$above' limit).                             *
 *   4: temporary (e.g. backtracks counter in FD labeling).                *
 *                                                                         *
 *   7: permanent catch handler.                                           *
 *   8: permanent catch ball.                                              *
 *                                                                         *
 *  10: permanent top level depth (for top-level and stop/abort).          *
 *  11: permanent top level handler (B level) for abort and stop.          *
 *  12: permanent: is linedit present ?                                    *
 *  13: permanent: is the debugger present ?                               *
 *                                                                         *
 *  20: permanent: should stream fcts emit a char before calling fgetc ?   *
 *                                                                         *
 * 100..199: free for users (who know pl_sys_var[] exists !)               *
 * 200..: some prolog flag values.                                         *
 *-------------------------------------------------------------------------*/
