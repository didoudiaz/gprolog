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

	/* values for integer_rounding_function */
#define PF_ROUND_ZERO              0
#define PF_ROUND_DOWN              1

	/* values for unknown, syntax_error, os_error */
#define PF_ERR_ERROR               0	/* same order as in read.pl */
#define PF_ERR_WARNING             1
#define PF_ERR_FAIL                2


     /* values for double_quotes and back_quotes */
#define PF_QUOT_AS_CODES           0	/* bit 2 is set if no_escape */
#define PF_QUOT_AS_CHARS           1
#define PF_QUOT_AS_ATOM            2
#define PF_QUOT_NO_ESCAPE_BIT      2
#define PF_QUOT_AS_PART_MASK       ((1 << PF_QUOT_NO_ESCAPE_BIT) - 1)
#define PF_QUOT_NO_ESCAPE_MASK     ((1 << PF_QUOT_NO_ESCAPE_BIT))




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct flag_inf *FlagInfP;

typedef WamWord (*FlagFctGet)(FlagInfP flag);
typedef Bool    (*FlagFctChk)(FlagInfP flag, WamWord tag_mask, WamWord value_word);
typedef Bool    (*FlagFctSet)(FlagInfP flag, WamWord value_word);


typedef enum
{
  PF_TYPE_INTEGER,		/* an integer (value = int)        */
  PF_TYPE_ATOM,			/* an atom (value = int of atom)   */
  PF_TYPE_ROUND,		/* toward_zero/down (see PF_ROUND_)*/
  PF_TYPE_BOOL,			/* false/true                      */
  PF_TYPE_ON_OFF,		/* off/on (value = 0/1)            */
  PF_TYPE_ERR,			/* error,warning,fail (see PF_ERR_)*/
  PF_TYPE_QUOTES,		/* chars,... (see PF_QUOTES_)      */
  PF_TYPE_ANY			/* other (value = user handled)    */
}FlagType;



typedef struct flag_inf		/* flag information                */
{				/* ------------------------------- */
  int atom_name;		/* atom of the flag name           */
  Bool modifiable;		/* is it modifiable ?              */
  FlagType type;		/* type see PF_TYPE_xxx            */
  PlLong value;			/* flag value (generic value)      */
  FlagFctGet fct_get;		/* value -> term (curr prolog flag)*/
  FlagFctChk fct_chk;		/* check term (for set prolog flag)*/
  FlagFctSet fct_set;		/* term -> value  (set prolog flag)*/
}FlagInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

FlagInf *Pl_New_Prolog_Flag(char *name, Bool modifiable, FlagType type, PlLong value,
			    FlagFctGet fct_get, FlagFctChk fct_chk, FlagFctSet fct_set);


/* macros to create flags of predefined types.
 * modifiable flags give rise to global variable pl_flag_xxx
 */

#define NEW_FLAG_INTEGER(f, v) Pl_New_Prolog_Flag(#f, FALSE, PF_TYPE_INTEGER, v, NULL, NULL, NULL)

#define NEW_FLAG_ATOM_A(f, v)  Pl_New_Prolog_Flag(#f, FALSE, PF_TYPE_ATOM, v, NULL, NULL, NULL)

#define NEW_FLAG_ATOM(f, v)    Pl_New_Prolog_Flag(#f, FALSE, PF_TYPE_ATOM, Pl_Create_Atom(v), NULL, NULL, NULL)

#define NEW_FLAG_ROUND(f, v)   Pl_New_Prolog_Flag(#f, FALSE, PF_TYPE_ROUND, v, NULL, NULL, NULL)

#define NEW_FLAG_BOOL(f, v)    Pl_New_Prolog_Flag(#f, FALSE, PF_TYPE_BOOL, v, NULL, NULL, NULL)

#define NEW_FLAG_ON_OFF(f, v)  pl_flag_##f = Pl_New_Prolog_Flag(#f, TRUE, PF_TYPE_ON_OFF, v, NULL, NULL, NULL)

#define NEW_FLAG_ERR(f, v)     pl_flag_##f = Pl_New_Prolog_Flag(#f, TRUE, PF_TYPE_ERR, v, NULL, NULL, NULL)

#define NEW_FLAG_QUOTES(f, v)  pl_flag_##f = Pl_New_Prolog_Flag(#f, TRUE, PF_TYPE_QUOTES, v, NULL, NULL, NULL)




#define Flag_Value(f) ((pl_flag_##f)->value)




/*-------------------------------------------------------------------------*
 * Things related to flag_c.c:                                             *
 *   - concrete implementation of flags (e.g. variables pl_flag_xxx)       *
 *   - system variables management (sys_var)                               *
 * All of this should be better in another .h file but here for simplicity *
 *-------------------------------------------------------------------------*/


#define Char_Conversion(c)         ((Flag_Value(char_conversion) &&    \
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




#ifdef FLAG_C_FILE

PlLong pl_sys_var[MAX_SYS_VARS];

FlagInf *pl_flag_back_quotes;
FlagInf *pl_flag_char_conversion;
FlagInf *pl_flag_debug;
FlagInf *pl_flag_double_quotes;
FlagInf *pl_flag_multifile_warning;
FlagInf *pl_flag_os_error;
FlagInf *pl_flag_singleton_warning;
FlagInf *pl_flag_strict_iso;
FlagInf *pl_flag_suspicious_warning;
FlagInf *pl_flag_syntax_error;
FlagInf *pl_flag_unknown;

#else

extern PlLong pl_sys_var[];

extern FlagInf *pl_flag_back_quotes;
extern FlagInf *pl_flag_char_conversion;
extern FlagInf *pl_flag_debug;
extern FlagInf *pl_flag_double_quotes;
extern FlagInf *pl_flag_multifile_warning;
extern FlagInf *pl_flag_os_error;
extern FlagInf *pl_flag_singleton_warning;
extern FlagInf *pl_flag_strict_iso;
extern FlagInf *pl_flag_suspicious_warning;
extern FlagInf *pl_flag_syntax_error;
extern FlagInf *pl_flag_unknown;

#endif




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
 * 100..: free for users (who know pl_sys_var[] exists !)                  *
 *-------------------------------------------------------------------------*/
