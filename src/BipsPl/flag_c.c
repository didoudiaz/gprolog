/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_c.c                                                        *
 * Descr.: Prolog flag and system variable management - C Part             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OBJ_INIT Flag_Initializer

#define FLAG_C_FILE

#include "engine_pl.h"
#include "gprolog_cst.h"
#include "bips_pl.h"

#ifndef _WIN32
#include <unistd.h>
extern char **environ;
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int type;
  int prec;
  int left;
  int right;
  int length;
}
SFOp;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_flag_tbl[NB_OF_FLAGS];

static int atom_down;
static int atom_toward_zero;

static int atom_on;
static int atom_off;

/* pl_atom_error is already defined in the set of often used atoms */
static int atom_warning;
static int atom_fail;

static int atom_chars;
static int atom_codes;
static int atom_atom;
static int atom_chars_no_escape;
static int atom_codes_no_escape;
static int atom_atom_no_escape;

static int atom_the_dialect;
static int atom_the_home;

static int atom_prolog[15];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Unif_Flag(int i, WamWord value_word);



#define CURRENT_PROLOG_FLAG_ALT    X1_2463757272656E745F70726F6C6F675F666C61675F616C74

#define ENVIRON_ALT                X1_24656E7669726F6E5F616C74

Prolog_Prototype(CURRENT_PROLOG_FLAG_ALT, 0);
Prolog_Prototype(ENVIRON_ALT, 0);




/*-------------------------------------------------------------------------*
 * FLAG_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Flag_Initializer(void)
{
  atom_flag_tbl[FLAG_BOUNDED] = Pl_Create_Atom("bounded");
  atom_flag_tbl[FLAG_MAX_INTEGER] = Pl_Create_Atom("max_integer");
  atom_flag_tbl[FLAG_MIN_INTEGER] = Pl_Create_Atom("min_integer");
  atom_flag_tbl[FLAG_ROUNDING_FCT] = Pl_Create_Atom("integer_rounding_function");

  atom_flag_tbl[FLAG_CHAR_CONVERSION] = Pl_Create_Atom("char_conversion");
  atom_flag_tbl[FLAG_DEBUG] = Pl_Create_Atom("debug");
  atom_flag_tbl[FLAG_MAX_ARITY] = Pl_Create_Atom("max_arity");
  atom_flag_tbl[FLAG_UNKNOWN] = Pl_Create_Atom("unknown");
  atom_flag_tbl[FLAG_DOUBLE_QUOTES] = Pl_Create_Atom("double_quotes");
  atom_flag_tbl[FLAG_BACK_QUOTES] = Pl_Create_Atom("back_quotes");

  atom_flag_tbl[FLAG_SYNTAX_ERROR] = Pl_Create_Atom("syntax_error");
  atom_flag_tbl[FLAG_OS_ERROR] = Pl_Create_Atom("os_error");
  atom_flag_tbl[FLAG_MAX_ATOM] = Pl_Create_Atom("max_atom");
  atom_flag_tbl[FLAG_MAX_UNGET] = Pl_Create_Atom("max_unget");
  atom_flag_tbl[FLAG_SINGLETON_WARNING] = Pl_Create_Atom("singleton_warning");
  atom_flag_tbl[FLAG_SUSPICIOUS_WARNING] = Pl_Create_Atom("suspicious_warning");
  atom_flag_tbl[FLAG_MULTIFILE_WARNING] = Pl_Create_Atom("multifile_warning");
  atom_flag_tbl[FLAG_STRICT_ISO] = Pl_Create_Atom("strict_iso");

  atom_flag_tbl[FLAG_PROLOG_NAME] = Pl_Create_Atom("prolog_name");
  atom_flag_tbl[FLAG_PROLOG_VERSION] = Pl_Create_Atom("prolog_version");
  atom_flag_tbl[FLAG_PROLOG_DATE] = Pl_Create_Atom("prolog_date");
  atom_flag_tbl[FLAG_PROLOG_COPYRIGHT] = Pl_Create_Atom("prolog_copyright");
  atom_flag_tbl[FLAG_DIALECT] = Pl_Create_Atom("dialect");
  atom_flag_tbl[FLAG_HOME] = Pl_Create_Atom("home");
  atom_flag_tbl[FLAG_HOST_OS] = Pl_Create_Atom("host_os");
  atom_flag_tbl[FLAG_HOST_VENDOR] = Pl_Create_Atom("host_vendor");
  atom_flag_tbl[FLAG_HOST_CPU] = Pl_Create_Atom("host_cpu");
  atom_flag_tbl[FLAG_HOST] = Pl_Create_Atom("host");
  atom_flag_tbl[FLAG_ARCH] = Pl_Create_Atom("arch");
  atom_flag_tbl[FLAG_ADDRESS_BITS] = Pl_Create_Atom("address_bits");

  atom_flag_tbl[FLAG_COMPILED_AT] = Pl_Create_Atom("compiled_at");
  atom_flag_tbl[FLAG_C_CC] = Pl_Create_Atom("c_cc");
  atom_flag_tbl[FLAG_C_CFLAGS] = Pl_Create_Atom("c_cflags");
  atom_flag_tbl[FLAG_C_LDFLAGS] = Pl_Create_Atom("c_ldflags");

  atom_flag_tbl[FLAG_VERSION] = Pl_Create_Atom("version");
  atom_flag_tbl[FLAG_VERSION_DATA] = Pl_Create_Atom("version_data");
  atom_flag_tbl[FLAG_UNIX] = Pl_Create_Atom("unix");

  atom_flag_tbl[FLAG_ARGV] = Pl_Create_Atom("argv");

  atom_down = Pl_Create_Atom("down");
  atom_toward_zero = Pl_Create_Atom("toward_zero");

  atom_on = Pl_Create_Atom("on");
  atom_off = Pl_Create_Atom("off");

  atom_warning = Pl_Create_Atom("warning");
  atom_fail = Pl_Create_Atom("fail");

  atom_chars = Pl_Create_Atom("chars");
  atom_codes = Pl_Create_Atom("codes");
  atom_atom = Pl_Create_Atom("atom");
  atom_chars_no_escape = Pl_Create_Atom("chars_no_escape");
  atom_codes_no_escape = Pl_Create_Atom("codes_no_escape");
  atom_atom_no_escape = Pl_Create_Atom("atom_no_escape");

  atom_the_dialect = Pl_Create_Atom(PROLOG_DIALECT);

  atom_the_home = Pl_Create_Atom(pl_home ? pl_home : "");

  atom_prolog[0] = Pl_Create_Atom(PROLOG_NAME);
  atom_prolog[1] = Pl_Create_Atom(PROLOG_VERSION);
  atom_prolog[2] = Pl_Create_Atom(PROLOG_DATE);
  atom_prolog[3] = Pl_Create_Atom(PROLOG_COPYRIGHT);
  atom_prolog[4] = atom_the_dialect;
  atom_prolog[5] = atom_the_home;
  atom_prolog[6] = Pl_Create_Atom(M_CPU);
  atom_prolog[7] = Pl_Create_Atom(M_VENDOR);
  atom_prolog[8] = Pl_Create_Atom(M_OS);
  atom_prolog[9] = Pl_Create_Atom(M_CPU "-" M_VENDOR "-" M_OS);
  atom_prolog[10] = Pl_Create_Atom(M_CPU "-" M_OS);
#if defined(__DATE__) && defined(__TIME__)
  atom_prolog[11] = Pl_Create_Atom(__DATE__ ", " __TIME__);
#else
  atom_prolog[11] = Pl_Create_Atom("unknown date");
#endif
  atom_prolog[12] = Pl_Create_Atom(CC);
  atom_prolog[13] = Pl_Create_Atom(CFLAGS_MACHINE " " CFLAGS);
  atom_prolog[14] = Pl_Create_Atom(LDFLAGS);
 

  Flag_Value(FLAG_SINGLETON_WARNING) = 1;
  Flag_Value(FLAG_SUSPICIOUS_WARNING) = 1;
  Flag_Value(FLAG_MULTIFILE_WARNING) = 1;
  Flag_Value(FLAG_STRICT_ISO) = 1;

  Flag_Value(FLAG_DOUBLE_QUOTES) = FLAG_AS_CODES;

  /* DON'T CHANGE this FLAG_BACK_QUOTES default: NO_ESCAPE is useful under
   * Windows when assoc .pl to gprolog (see InnoSetup) and avoid \
   * to be misinterpreted in pathnames (e.g. c:\foo\bar).
   */
  Flag_Value(FLAG_BACK_QUOTES) = FLAG_AS_ATOM | FLAG_NO_ESCAPE_MASK; 

  SYS_VAR_LINEDIT = pl_stream_use_linedit;
}




/*-------------------------------------------------------------------------*
 * PL_SET_PROLOG_FLAG_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord term;
  int atom;
  int i;
  PredInf *pred;

  atom = Pl_Rd_Atom_Check(flag_word);

  for (i = 0; i < NB_OF_FLAGS; i++)
    if (atom_flag_tbl[i] == atom)
      break;

  if (i >= NB_OF_FLAGS)
    Pl_Err_Domain(pl_domain_prolog_flag, flag_word);

  DEREF(value_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);

  switch (i)
    {
    case FLAG_BOUNDED:
    case FLAG_UNIX:
      if (tag_mask != TAG_ATM_MASK ||
	  (atom != pl_atom_true && atom != pl_atom_false))
	goto err_value;
      goto err_perm;

    case FLAG_MAX_INTEGER:
    case FLAG_MIN_INTEGER:
    case FLAG_MAX_ARITY:
    case FLAG_MAX_ATOM:
    case FLAG_MAX_UNGET:
    case FLAG_VERSION:
    case FLAG_ADDRESS_BITS:
      if (tag_mask != TAG_INT_MASK)
	goto err_value;
      goto err_perm;

    case FLAG_ROUNDING_FCT:
      if (tag_mask != TAG_ATM_MASK ||
	  (atom != atom_down && atom != atom_toward_zero))
	goto err_value;
      goto err_perm;

    case FLAG_CHAR_CONVERSION:
    case FLAG_DEBUG:
    case FLAG_SINGLETON_WARNING:
    case FLAG_SUSPICIOUS_WARNING:
    case FLAG_MULTIFILE_WARNING:
    case FLAG_STRICT_ISO:
      if (tag_mask != TAG_ATM_MASK || (atom != atom_on && atom != atom_off))
	goto err_value;

      if (i != FLAG_DEBUG)
	{
	  Flag_Value(i) = atom == atom_on;
	  break;
	}
      /* if no debugger the flag must be off */
      if (!SYS_VAR_DEBUGGER)
	return atom == atom_off;

      Flag_Value(i) = atom == atom_on;

      pred = Pl_Lookup_Pred(Pl_Create_Atom((atom == atom_on)
				     ? "debug" : "nodebug"), 0);

      if (pred != NULL)
	Pl_Call_Prolog((CodePtr) (pred->codep));
      else
	Flag_Value(i) = FALSE;	/* should not occurs */
      break;

    case FLAG_UNKNOWN:
    case FLAG_SYNTAX_ERROR:
    case FLAG_OS_ERROR:
      if (tag_mask != TAG_ATM_MASK)
	goto err_value;

      if (atom == pl_atom_error)
	Flag_Value(i) = FLAG_VALUE_ERROR;
      else if (atom == atom_warning)
	Flag_Value(i) = FLAG_VALUE_WARNING;
      else if (atom == atom_fail)
	Flag_Value(i) = FLAG_VALUE_FAIL;
      else
	goto err_value;
      break;

    case FLAG_DOUBLE_QUOTES:
    case FLAG_BACK_QUOTES:
      if (tag_mask != TAG_ATM_MASK)
	goto err_value;

      if (atom == atom_codes)
	Flag_Value(i) = FLAG_AS_CODES;
      else if (atom == atom_codes_no_escape)
	Flag_Value(i) = FLAG_AS_CODES | FLAG_NO_ESCAPE_MASK;
      else if (atom == atom_chars)
	Flag_Value(i) = FLAG_AS_CHARS;
      else if (atom == atom_chars_no_escape)
	Flag_Value(i) = FLAG_AS_CHARS | FLAG_NO_ESCAPE_MASK;
      else if (atom == atom_atom)
	Flag_Value(i) = FLAG_AS_ATOM;
      else if (atom == atom_atom_no_escape)
	Flag_Value(i) = FLAG_AS_ATOM | FLAG_NO_ESCAPE_MASK;
      else
	goto err_value;
      break;

    case FLAG_PROLOG_NAME:
    case FLAG_PROLOG_VERSION:
    case FLAG_PROLOG_DATE:
    case FLAG_PROLOG_COPYRIGHT:
    case FLAG_DIALECT:
    case FLAG_HOME:
    case FLAG_HOST_OS:
    case FLAG_HOST_VENDOR:
    case FLAG_HOST_CPU:
    case FLAG_HOST:
    case FLAG_ARCH:
    case FLAG_COMPILED_AT:
    case FLAG_C_CC:
    case FLAG_C_CFLAGS:
    case FLAG_C_LDFLAGS:
      if (tag_mask != TAG_ATM_MASK)
	goto err_value;
      goto err_perm;


    case FLAG_VERSION_DATA:
      if (tag_mask != TAG_STC_MASK)
	goto err_value;
      adr = UnTag_STC(word);
      if (Functor(adr) != atom_the_dialect || Arity(adr) != 4)
	goto err_value;
      goto err_perm;
    }

  return TRUE;

err_value:
  term = Pl_Put_Structure(ATOM_CHAR('+'), 2);
  Pl_Unify_Value(flag_word);
  Pl_Unify_Value(value_word);

  Pl_Err_Domain(pl_domain_flag_value, term);


err_perm:
  Pl_Err_Permission(pl_permission_operation_modify, pl_permission_type_flag,
		    flag_word);

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PROLOG_FLAG_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  int i;
  int atom;

  DEREF(flag_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      atom = Pl_Rd_Atom_Check(word);

      for (i = 0; i < NB_OF_FLAGS; i++)
	if (atom_flag_tbl[i] == atom)
	  break;

      if (i >= NB_OF_FLAGS)
	Pl_Err_Domain(pl_domain_prolog_flag, flag_word);

      return Unif_Flag(i, value_word);
    }

  /* non deterministic case */
  i = 0;

  A(0) = flag_word;
  A(1) = value_word;
  A(2) = i + 1;
  Pl_Create_Choice_Point((CodePtr)
		      Prolog_Predicate(CURRENT_PROLOG_FLAG_ALT, 0), 3);

  Pl_Get_Atom(atom_flag_tbl[i], flag_word);

  return Unif_Flag(i, value_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PROLOG_FLAG_ALT_0                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Prolog_Flag_Alt_0(void)
{
  WamWord flag_word, value_word;
  int i;


  Pl_Update_Choice_Point((CodePtr)
		      Prolog_Predicate(CURRENT_PROLOG_FLAG_ALT, 0), 0);

  flag_word = AB(B, 0);
  value_word = AB(B, 1);
  i = AB(B, 2);

  if (i + 1 == NB_OF_FLAGS)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = flag_word;
      AB(B, 1) = value_word;
#endif
      AB(B, 2) = i + 1;
    }

  Pl_Get_Atom(atom_flag_tbl[i], flag_word);

  return Unif_Flag(i, value_word);
}




/*-------------------------------------------------------------------------*
 * UNIF_FLAG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Unif_Flag(int i, WamWord value_word)
{
  int atom = -1;
  PlLong n;

  switch (i)
    {
    case FLAG_BOUNDED:
      atom = pl_atom_true;
      break;

    case FLAG_MAX_INTEGER:
      n = INT_GREATEST_VALUE;
      break;

    case FLAG_MIN_INTEGER:
      n = INT_LOWEST_VALUE;
      break;

    case FLAG_ROUNDING_FCT:
      atom = ((-3 / 2) == -1) ? atom_toward_zero : atom_down;
      break;

    case FLAG_CHAR_CONVERSION:
    case FLAG_DEBUG:
    case FLAG_SINGLETON_WARNING:
    case FLAG_SUSPICIOUS_WARNING:
    case FLAG_MULTIFILE_WARNING:
    case FLAG_STRICT_ISO:
      atom = (Flag_Value(i)) ? atom_on : atom_off;
      break;

    case FLAG_MAX_ARITY:
      n = MAX_ARITY;
      break;

    case FLAG_MAX_ATOM:
      n = pl_max_atom;
      break;

    case FLAG_MAX_UNGET:
      n = STREAM_PB_SIZE;
      break;

    case FLAG_UNKNOWN:
    case FLAG_SYNTAX_ERROR:
    case FLAG_OS_ERROR:
      switch (Flag_Value(i))
	{
	case FLAG_VALUE_ERROR:
	  atom = pl_atom_error;
	  break;

	case FLAG_VALUE_WARNING:
	  atom = atom_warning;
	  break;

	case FLAG_VALUE_FAIL:
	  atom = atom_fail;
	  break;
	}
      break;

    case FLAG_DOUBLE_QUOTES:
    case FLAG_BACK_QUOTES:
      switch (Flag_Value(i))
	{
	case FLAG_AS_CODES:
	  atom = atom_codes;
	  break;

	case FLAG_AS_CODES | FLAG_NO_ESCAPE_MASK:
	  atom = atom_codes_no_escape;
	  break;

	case FLAG_AS_CHARS:
	  atom = atom_chars;
	  break;

	case FLAG_AS_CHARS | FLAG_NO_ESCAPE_MASK:
	  atom = atom_chars_no_escape;
	  break;

	case FLAG_AS_ATOM:
	  atom = atom_atom;
	  break;

	case FLAG_AS_ATOM | FLAG_NO_ESCAPE_MASK:
	  atom = atom_atom_no_escape;
	  break;
	}
      break;

    case FLAG_PROLOG_NAME:
    case FLAG_PROLOG_VERSION:
    case FLAG_PROLOG_DATE:
    case FLAG_PROLOG_COPYRIGHT:
    case FLAG_DIALECT:
    case FLAG_HOME:
    case FLAG_HOST_OS:
    case FLAG_HOST_CPU:
    case FLAG_HOST_VENDOR:
    case FLAG_HOST:
    case FLAG_ARCH:
    case FLAG_COMPILED_AT:
    case FLAG_C_CC:
    case FLAG_C_CFLAGS:
    case FLAG_C_LDFLAGS:
      atom = atom_prolog[i - FLAG_PROLOG_NAME];
      break;

    case FLAG_ADDRESS_BITS:
      n = WORD_SIZE;
      break;

    case FLAG_VERSION:
      n = __GPROLOG_VERSION__;
      break;

    case FLAG_VERSION_DATA:
      return Pl_Get_Structure(atom_the_dialect, 4, value_word) &&
	Pl_Unify_Integer(__GPROLOG__) &&
	Pl_Unify_Integer(__GPROLOG_MINOR__) &&
	Pl_Unify_Integer(__GPROLOG_PATCHLEVEL__) &&
	Pl_Unify_Nil();

    case FLAG_UNIX:
#if defined(__unix__) || defined(__CYGWIN__) || defined(unix)
      atom = pl_atom_true;
#else
      atom = pl_atom_false;
#endif
      break;

    case FLAG_ARGV:
      for (i = 0; i < pl_os_argc; i++)
	{
	  if (!Pl_Get_List(value_word) || !Pl_Unify_Atom(Pl_Create_Atom(pl_os_argv[i])))
	    return FALSE;

	  value_word = Pl_Unify_Variable();
	}
      return Pl_Get_Nil(value_word);
    }

  if (atom < 0)
    return Pl_Get_Integer(n, value_word);

  return Pl_Get_Atom(atom, value_word);
}





/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_WRITE_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Write_2(WamWord var_word, WamWord n_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)] = Pl_Rd_Integer(n_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_READ_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sys_Var_Read_2(WamWord var_word, WamWord n_word)
{
  return Pl_Get_Integer(pl_sys_var[Pl_Rd_Integer(var_word)], n_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_INC_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Inc_1(WamWord var_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)]++;
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_DEC_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Dec_1(WamWord var_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)]--;
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_SET_BIT_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Set_Bit_2(WamWord var_word, WamWord bit_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)] |= (1 << Pl_Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_RESET_BIT_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Reset_Bit_2(WamWord var_word, WamWord bit_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)] &= ~(1 << Pl_Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_SET_BIT_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sys_Var_Get_Bit_3(WamWord var_word, WamWord bit_word, WamWord value_word)
{
  unsigned x;

  x = (pl_sys_var[Pl_Rd_Integer(var_word)] >> Pl_Rd_Integer(bit_word))  & 1;
  return Pl_Un_Integer(x, value_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_PUT_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Put_2(WamWord var_word, WamWord term_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int sv;
  int size;

  sv = Pl_Rd_Integer(var_word);

  word = pl_sys_var[sv];
  tag_mask = Tag_Mask_Of(word);
  if (tag_mask == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      if (adr != NULL)
	Free(adr);		/* recover the Malloc: don't mix sys_var_put/get and others sys_var_write... on same sys variable */
    }

  DEREF(term_word, word, tag_mask);

  if (tag_mask == TAG_ATM_MASK || tag_mask == TAG_INT_MASK)
    {
      pl_sys_var[sv] = word;
      return;
    }

  size = Pl_Term_Size(word);
  adr = (WamWord *) Malloc(size * sizeof(WamWord));	/* recovered at next sys_var_put */
  Pl_Copy_Term(adr, &word);
  pl_sys_var[sv] = Tag_REF(adr);
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_GET_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sys_Var_Get_2(WamWord var_word, WamWord term_word)
{
  WamWord word;
  WamWord *adr;
  int size;

  word = pl_sys_var[Pl_Rd_Integer(var_word)];

  if (Tag_Mask_Of(word) == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      size = Pl_Term_Size(*adr);
      Pl_Copy_Contiguous_Term(H, adr);
      word = *H;
      H += size;
    }

  return Pl_Unify(word, term_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_CURRENT_B_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Get_Current_B_1(WamWord b_word)
{
  WamWord word;

  word = Pl_Get_Current_Choice();
  Pl_Unify(word, b_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_CURRENT_B_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Current_B_1(WamWord b_word)
{
  WamWord word, tag_mask;

  DEREF(b_word, word, tag_mask);
  Pl_Cut(word);
}




/*-------------------------------------------------------------------------*
 * PL_WRITE_PL_STATE_FILE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/* these macros are to avoid gcc warning warn_unused_result */
#define FWRITE(b, sz, n, f) if (fwrite(b, sz, n, f) != n) {}
#define FREAD(b, sz, n, f)  if (fread(b, sz, n, f) != n) {}


Bool
Pl_Write_Pl_State_File(WamWord file_word)
{
  char *file;
  FILE *f;
  int i;
  HashScan scan;
  OperInf *oper;
  SFOp sf_op;
  int c;
/* 'static' is because gcc allocates a frame even with -fomit-frame-pointer.
 * This corrupts ebp on ix86 */
  static char cv[2];


  file = pl_atom_tbl[Pl_Rd_Atom_Check(file_word)].name;
  file = Pl_M_Absolute_Path_Name(file);

  f = fopen(file, "wb");
  Os_Test_Error_Null(f);

  i = Pl_Hash_Nb_Elements(pl_oper_tbl);
  FWRITE(&i, sizeof(i), 1, f);

  for (oper = (OperInf *) Pl_Hash_First(pl_oper_tbl, &scan); oper;
       oper = (OperInf *) Pl_Hash_Next(&scan))
    {
      sf_op.type = Type_Of_Oper(oper->a_t);
      sf_op.prec = oper->prec;
      sf_op.left = oper->left;
      sf_op.right = oper->right;
      sf_op.length = pl_atom_tbl[Atom_Of_Oper(oper->a_t)].prop.length;
      FWRITE(&sf_op, sizeof(sf_op), 1, f);
      FWRITE(pl_atom_tbl[Atom_Of_Oper(oper->a_t)].name, sf_op.length, 1, f);
    }

  i = Flag_Value(FLAG_DOUBLE_QUOTES);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_BACK_QUOTES);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_CHAR_CONVERSION);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_SINGLETON_WARNING);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_SUSPICIOUS_WARNING);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_MULTIFILE_WARNING);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_STRICT_ISO);
  FWRITE(&i, sizeof(i), 1, f);

  i = SYS_VAR_SAY_GETC;
  FWRITE(&i, sizeof(i), 1, f);

  for (c = 0; c < 256; c++)
    if (pl_char_conv[c] != c)
      {
	cv[0] = c;
	cv[1] = pl_char_conv[c];
	FWRITE(&cv, 2, 1, f);
      }

  cv[0] = 0;
  cv[1] = 0;
  FWRITE(&cv, 2, 1, f);

  fclose(f);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_READ_PL_STATE_FILE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Read_Pl_State_File(WamWord file_word)
{
  char *file;
  FILE *f;
  int i;
  SFOp sf_op;
  int c;
  char cv[2];


  file = pl_atom_tbl[Pl_Rd_Atom_Check(file_word)].name;
  file = Pl_M_Absolute_Path_Name(file);

  f = fopen(file, "rb");
  Os_Test_Error_Null(f);

  Pl_Hash_Delete_All(pl_oper_tbl);

  FREAD(&i, sizeof(i), 1, f);

  while (i--)
    {
      FREAD(&sf_op, sizeof(sf_op), 1, f);
      FREAD(pl_glob_buff, sf_op.length, 1, f);
      pl_glob_buff[sf_op.length] = '\0';
      Pl_Create_Oper(Pl_Create_Allocate_Atom(pl_glob_buff),
		  sf_op.type, sf_op.prec, sf_op.left, sf_op.right);
    }

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_DOUBLE_QUOTES) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_BACK_QUOTES) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_CHAR_CONVERSION) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_SINGLETON_WARNING) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_SUSPICIOUS_WARNING) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_MULTIFILE_WARNING) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_STRICT_ISO) = i;

  FREAD(&i, sizeof(i), 1, f);
  SYS_VAR_SAY_GETC = i;

  for (;;)
    {
      FREAD(&cv, 2, 1, f);
      c = cv[0];
      if (c == 0 && cv[1] == 0)
	break;

      pl_char_conv[c] = cv[1];
    }

  fclose(f);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_ARGUMENT_COUNTER_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Argument_Counter_1(WamWord n_word)
{
  return Pl_Un_Integer_Check(pl_os_argc, n_word);
}




/*-------------------------------------------------------------------------*
 * PL_ARGUMENT_VALUE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Argument_Value_2(WamWord i_word, WamWord a_word)
{
  int i;

  i = Pl_Rd_Positive_Check(i_word);
  if (i >= pl_os_argc)
    return FALSE;

  return Pl_Un_Atom_Check(Pl_Create_Atom(pl_os_argv[i]), a_word);
}




/*-------------------------------------------------------------------------*
 * PL_ARGUMENT_LIST_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Argument_List_1(WamWord list_word)
{
  int i;

  Pl_Check_For_Un_List(list_word);

  for (i = 1; i < pl_os_argc; i++)
    {
      if (!Pl_Get_List(list_word) || !Pl_Unify_Atom(Pl_Create_Atom(pl_os_argv[i])))
	return FALSE;

      list_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * PL_ENVIRON_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Environ_2(WamWord var_name_word, WamWord value_word)
{
  WamWord word, tag_mask;
  char *var_name;
  char *value;
  char **cur_env;
  char *one_env;
  int lg;

  Pl_Check_For_Un_Atom(value_word);

  DEREF(var_name_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      var_name = Pl_Rd_String_Check(word);
      value = (char *) getenv(var_name);

      return value && Pl_Un_String_Check(value, value_word);
    }

  /* non deterministic case */
  cur_env = environ;

  one_env = *cur_env++;
  if (one_env == NULL)
    return FALSE;

  if (*cur_env)
    {
      A(0) = var_name_word;
      A(1) = value_word;
      A(2) = (WamWord) cur_env;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(ENVIRON_ALT, 0), 3);
    }


  value = strchr(one_env, '=');
  lg = value - one_env;
  var_name = pl_glob_buff;

  strncpy(var_name, one_env, lg);
  var_name[lg] = '\0';

  value++;			/* skip = */

  return Pl_Un_String_Check(var_name, var_name_word) &&
    Pl_Get_Atom(Pl_Create_Atom(value), value_word);
}




/*-------------------------------------------------------------------------*
 * PL_ENVIRON_ALT_0                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Environ_Alt_0(void)
{
  WamWord var_name_word, value_word;
  char *var_name;
  char *value;
  char **cur_env;
  char *one_env;
  int lg;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(ENVIRON_ALT, 0), 0);

  var_name_word = AB(B, 0);
  value_word = AB(B, 1);
  cur_env = (char **) AB(B, 2);

  one_env = *cur_env++;

  if (*cur_env == NULL)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B,0)=var_name_word;
      AB(B,1)=value_word;
#endif
      AB(B, 2) = (WamWord) cur_env;
    }

  value = strchr(one_env, '=');
  lg = value - one_env;
  var_name = pl_glob_buff;

  strncpy(var_name, one_env, lg);
  var_name[lg] = '\0';

  value++;			/* skip = */

  return Pl_Un_String_Check(var_name, var_name_word) &&
    Pl_Get_Atom(Pl_Create_Atom(value), value_word);
}
