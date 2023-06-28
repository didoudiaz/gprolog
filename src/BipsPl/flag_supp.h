/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_supp.h                                                     *
 * Descr.: Prolog flag and system variable support - header file           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
#define PF_QUOT_AS_CODES           0
#define PF_QUOT_AS_CHARS           1
#define PF_QUOT_AS_ATOM            2
#define PF_QUOT_AS_CODES_NO_ESCAPE 3
#define PF_QUOT_AS_CHARS_NO_ESCAPE 4
#define PF_QUOT_AS_ATOM_NO_ESCAPE  5




     /* values for show_information */
#define PF_SHOW_INFO_SILENT        0
#define PF_SHOW_INFO_NORMAL        1
#define PF_SHOW_INFO_INFORMATIONAL 2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct flag_inf *FlagInfP;

typedef WamWord (*FlagFctGet)(FlagInfP flag);
typedef Bool    (*FlagFctChk)(FlagInfP flag, WamWord tag_mask, WamWord value_word);
typedef Bool    (*FlagFctSet)(FlagInfP flag, WamWord value_word);


typedef enum
{
  PF_TYPE_INTEGER,		/* an integer (value = int)           */
  PF_TYPE_ATOM,			/* any atom (value = int of atom)     */
  PF_TYPE_ATOM_TBL,		/* an atom in tbl (value = tbl index) */
  PF_TYPE_ANY			/* other (value = user handled)       */
}FlagType;


typedef struct flag_inf		/* flag information                   */
{				/* ---------------------------------- */
  int atom_name;		/* atom of the flag name              */
  Bool modifiable;		/* is it modifiable ?                 */
  FlagType type;		/* type see PF_TYPE_xxx               */
  PlLong value;			/* flag value (generic value)         */
  int *tbl_atom;		/* tbl possib atoms (modifiable flag) */
  FlagFctGet fct_get;		/* value -> term (curr prolog flag)   */
  FlagFctChk fct_chk;		/* check term (for set prolog flag)   */
  FlagFctSet fct_set;		/* term -> value  (set prolog flag)   */
}FlagInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

FlagInf *Pl_New_Prolog_Flag(char *name, Bool modifiable, FlagType type, PlLong value,
			    FlagFctGet fct_get, FlagFctChk fct_chk, FlagFctSet fct_set, ...);

/* macros to create flags of predefined types.
 * modifiable flags give rise to global variable pl_flag_xxx
 */

	 /* do not include set fct to avoid empty __VA_ARGS__ 
	  * since trick ,##__VA_ARGS__ to remove comma is not normalized */
#define NEW_FLAG0(flag, modif, type, val, get, chk, ...) \
  Pl_New_Prolog_Flag(#flag, modif, type, val, get, chk, __VA_ARGS__)

#define NEW_FLAG_R(flag, type, val, get, chk, ...) \
  NEW_FLAG0(flag, FALSE, type, val, get, chk, __VA_ARGS__)

#define NEW_FLAG_W(flag, type, val, get, chk, ...) \
  pl_flag_##flag = NEW_FLAG0(flag, TRUE, type, val, get, chk, __VA_ARGS__)




#define NEW_FLAG_R_INTEGER(f, v)       NEW_FLAG_R(f, PF_TYPE_INTEGER, v, NULL, NULL, NULL)

#define NEW_FLAG_R_ATOM(f, v)          NEW_FLAG_R(f, PF_TYPE_ATOM, Pl_Create_Atom(v), NULL, NULL, NULL)

#define NEW_FLAG_R_ATOM_TBL(f, v, ...) NEW_FLAG_R(f, PF_TYPE_ATOM_TBL, v, NULL, NULL, NULL, __VA_ARGS__, -1);

#define NEW_FLAG_W_ATOM_TBL(f, v, ...) NEW_FLAG_W(f, PF_TYPE_ATOM_TBL, v, NULL, NULL, NULL, __VA_ARGS__, -1);

	/* bootstrapped (special case of ATOM_TBL) */
#define NEW_FLAG_R_BOOL(f, v)          NEW_FLAG_R_ATOM_TBL(f, v, pl_atom_false, pl_atom_true)

#define NEW_FLAG_W_ON_OFF(f, v)        NEW_FLAG_W_ATOM_TBL(f, v, atom_off, atom_on)




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

#define SYS_VAR_LISTING_ANY         (pl_sys_var[5])

#define SYS_VAR_TOP_LEVEL           (pl_sys_var[10])
#define SYS_VAR_LINEDIT             (pl_sys_var[12])
#define SYS_VAR_DEBUGGER            (pl_sys_var[13])

#define SYS_VAR_SAY_GETC            (pl_sys_var[20]) /* is it activated (0 or 1) */
#define CHAR_TO_EMIT_ON_PIPED_GETC  '\1'




#ifdef FLAG_C_FILE

PlLong pl_sys_var[MAX_SYS_VARS];

FlagInf *pl_flag_back_quotes;
FlagInf *pl_flag_char_conversion;
FlagInf *pl_flag_debug;
FlagInf *pl_flag_double_quotes;
FlagInf *pl_flag_multifile_warning;
FlagInf *pl_flag_os_error;
FlagInf *pl_flag_singleton_warning;
FlagInf *pl_flag_suspicious_warning;
FlagInf *pl_flag_show_banner;
FlagInf *pl_flag_show_error;
FlagInf *pl_flag_show_information;
FlagInf *pl_flag_strict_iso;
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
extern FlagInf *pl_flag_suspicious_warning;
extern FlagInf *pl_flag_show_banner;
extern FlagInf *pl_flag_show_error;
extern FlagInf *pl_flag_show_information;
extern FlagInf *pl_flag_strict_iso;
extern FlagInf *pl_flag_syntax_error;
extern FlagInf *pl_flag_unknown;

#endif




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
