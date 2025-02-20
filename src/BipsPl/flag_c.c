/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_c.c                                                        *
 * Descr.: Prolog flag and system variable management - C Part             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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

static int atom_on;
static int atom_off;

static int atom_the_dialect;
static int atom_the_cc;

static int atom_toward_zero;
static int atom_down;

static int atom_error;
static int atom_warning;
static int atom_fail;

static int atom_chars;
static int atom_codes;
static int atom_atom;
static int atom_chars_no_escape;
static int atom_codes_no_escape;
static int atom_atom_no_escape;

static int atom_silent;
static int atom_normal;
static int atom_informational;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Fct_Set_Debug(FlagInf *flag, WamWord value_word);

static WamWord Fct_Get_Version_Data(FlagInf *flag);
static Bool Fct_Chk_Version_Data(FlagInf *flag, WamWord tag_mask, WamWord value_word);

static WamWord Fct_Get_Argv(FlagInf *flag);
static Bool Fct_Chk_Argv(FlagInf *flag, WamWord tag_mask, WamWord value_word);




#define ENVIRON_ALT                X1_24656E7669726F6E5F616C74

Prolog_Prototype(ENVIRON_ALT, 0);




/*-------------------------------------------------------------------------*
 * FLAG_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Flag_Initializer(void)
{
  Bool is_unix = FALSE;

#if defined(__unix__) || defined(__CYGWIN__) || defined(unix)
  is_unix = TRUE;
#endif

  atom_on = Pl_Create_Atom("on");
  atom_off = Pl_Create_Atom("off");

  atom_the_dialect = Pl_Create_Atom(PROLOG_DIALECT);
  atom_the_cc = Pl_Create_Atom(CC);

  atom_toward_zero = Pl_Create_Atom("toward_zero");
  atom_down = Pl_Create_Atom("down");

  atom_error = Pl_Create_Atom("error");
  atom_warning = Pl_Create_Atom("warning");
  atom_fail = Pl_Create_Atom("fail");

  atom_chars = Pl_Create_Atom("chars");
  atom_codes = Pl_Create_Atom("codes");
  atom_atom = Pl_Create_Atom("atom");
  atom_chars_no_escape = Pl_Create_Atom("chars_no_escape");
  atom_codes_no_escape = Pl_Create_Atom("codes_no_escape");
  atom_atom_no_escape = Pl_Create_Atom("atom_no_escape");

  atom_silent = Pl_Create_Atom("silent");
  atom_normal = Pl_Create_Atom("normal");
  atom_informational = Pl_Create_Atom("informational");

  /* Unchangeable flags */

  NEW_FLAG_R_ATOM    (prolog_name,               PROLOG_NAME);
  NEW_FLAG_R_ATOM    (prolog_version,            PROLOG_VERSION);
  NEW_FLAG_R_ATOM    (prolog_date,               PROLOG_DATE);
  NEW_FLAG_R_ATOM    (prolog_copyright,          PROLOG_COPYRIGHT);

  NEW_FLAG_R_ATOM    (dialect,                   PROLOG_DIALECT);

  NEW_FLAG_R_INTEGER (version,                   __GPROLOG_VERSION__);
  NEW_FLAG_R         (version_data,              PF_TYPE_ANY, 0, Fct_Get_Version_Data, Fct_Chk_Version_Data, NULL);
  NEW_FLAG_R_BOOL    (bounded,                   TRUE);

  NEW_FLAG_R_INTEGER (max_integer,               INT_GREATEST_VALUE);    
  NEW_FLAG_R_INTEGER (min_integer,               INT_LOWEST_VALUE);
  NEW_FLAG_R_ATOM_TBL(integer_rounding_function, ((-3 / 2) == -1) ? 0 : 1, atom_toward_zero, atom_down);

  NEW_FLAG_R_INTEGER (max_arity,                 MAX_ARITY);
  NEW_FLAG_R_INTEGER (max_atom,                  pl_max_atom);
  NEW_FLAG_R_INTEGER (max_unget,                 STREAM_PB_SIZE);

  NEW_FLAG_R_ATOM    (home,                      pl_home ? pl_home : "");
  NEW_FLAG_R_ATOM    (host_os,                   M_OS);
  NEW_FLAG_R_ATOM    (host_vendor,               M_VENDOR);
  NEW_FLAG_R_ATOM    (host_cpu,                  M_CPU);
  NEW_FLAG_R_ATOM    (host,                      M_CPU "-" M_VENDOR "-" M_OS);
  NEW_FLAG_R_ATOM    (arch,                      M_CPU "-" M_OS);
  NEW_FLAG_R_INTEGER (address_bits,              WORD_SIZE);
  NEW_FLAG_R_BOOL    (unix,                      is_unix);

  NEW_FLAG_R_ATOM    (compiled_at,               COMPILED_AT); /* see arch_dep.h */
  NEW_FLAG_R_ATOM    (c_cc,                      CC);
  NEW_FLAG_R         (c_cc_version_data,         PF_TYPE_ANY, 1, Fct_Get_Version_Data, Fct_Chk_Version_Data,  NULL);
  NEW_FLAG_R_ATOM    (c_cflags,                  CFLAGS_MACHINE " " CFLAGS);
  NEW_FLAG_R_ATOM    (c_ldflags,                 LDFLAGS);                            

  NEW_FLAG_R         (argv,                      PF_TYPE_ANY, 0, Fct_Get_Argv, Fct_Chk_Argv, NULL);

  /* Changeable flags */

  NEW_FLAG_W_ON_OFF  (char_conversion,           0);
  NEW_FLAG_W_ON_OFF  (singleton_warning,         1);
  NEW_FLAG_W_ON_OFF  (suspicious_warning,        1);
  NEW_FLAG_W_ON_OFF  (multifile_warning,         1);
  NEW_FLAG_W_ON_OFF  (show_banner,               1);
  NEW_FLAG_W_ATOM_TBL(show_information,          PF_SHOW_INFO_NORMAL, atom_silent, atom_normal, atom_informational);
  //  NEW_FLAG_W_ATOM_TBL(show_error,                PF_SHOW_ERR_ERROR, atom_silent, atom_warning, atom_error);
  NEW_FLAG_W_ON_OFF  (strict_iso,                1);
#if 0
  NEW_FLAG_W_ON_OFF  (debug,                     0);
#else  /* to have a customized Set function */
  NEW_FLAG_W         (debug,                     PF_TYPE_ATOM_TBL, 0, NULL, NULL, Fct_Set_Debug, atom_off, atom_on, -1);
#endif


  NEW_FLAG_W_ATOM_TBL(double_quotes,             PF_QUOT_AS_CODES, atom_codes, atom_chars, atom_atom,
		                                 atom_codes_no_escape, atom_chars_no_escape, atom_atom_no_escape);

  /* DON'T CHANGE back_quotes default: no_escape is useful under
   * Windows when assoc .pl to gprolog (see InnoSetup) and avoid \ (backslash)
   * to be misinterpreted in pathnames (e.g. c:\foo\bar).
   */
  NEW_FLAG_W_ATOM_TBL(back_quotes,               PF_QUOT_AS_ATOM_NO_ESCAPE, atom_codes, atom_chars, atom_atom,
		                                 atom_codes_no_escape, atom_chars_no_escape, atom_atom_no_escape);

  NEW_FLAG_W_ATOM_TBL(unknown,                   PF_ERR_ERROR, atom_error, atom_warning, atom_fail);
  NEW_FLAG_W_ATOM_TBL(syntax_error,              PF_ERR_ERROR, atom_error, atom_warning, atom_fail);
  NEW_FLAG_W_ATOM_TBL(os_error,                  PF_ERR_ERROR, atom_error, atom_warning, atom_fail);

  SYS_VAR_LINEDIT = pl_stream_use_linedit;
}




/*-------------------------------------------------------------------------*
 * FCT_SET_DEBUG                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Fct_Set_Debug(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);
  PlLong value = (atom == atom_on);
  PredInf *pred;

  if (!SYS_VAR_DEBUGGER)
    return value == 0;

  pred = Pl_Lookup_Pred(Pl_Create_Atom(value ? "debug" : "nodebug"), 0);

  if (pred != NULL)
    Pl_Call_Prolog((CodePtr) (pred->codep));
  else
    value = 0;  /* should not occurs */

  flag->value = value;

  return TRUE;
}





/*-------------------------------------------------------------------------*
 * FCT_GET_VERSION_DATA FCT_CHK_VERSION_DATA                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Fct_Get_Version_Data(FlagInf *flag)
{
  int atom;
  int major, minor, patchlevel;
  WamWord value_word;

  if (flag->value == 0)		/* GNU Prolog version */
    {
      atom = atom_the_dialect;
      major = __GPROLOG__;
      minor = __GPROLOG_MINOR__;
      patchlevel = __GPROLOG_PATCHLEVEL__;
    }
  else				/* C compiler version */
    {
      atom = atom_the_cc;
      major = CC_MAJOR;
      minor = CC_MINOR;
      patchlevel = CC_PATCHLEVEL;
    }

  value_word = Pl_Put_Structure(atom, 4);
  Pl_Unify_Integer(major);
  Pl_Unify_Integer(minor);
  Pl_Unify_Integer(patchlevel);
  Pl_Unify_Nil();   

  return value_word;
}

static Bool
Fct_Chk_Version_Data(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom;
  WamWord *adr;

  if (tag_mask != TAG_STC_MASK)
    return FALSE;

  if (flag->value == 0)		/* GNU Prolog version */
    atom = atom_the_dialect;
  else				/* C compiler version */
    atom = atom_the_cc;

  adr = UnTag_STC(value_word);
  return Functor(adr) == atom && Arity(adr) == 4;
}




/*-------------------------------------------------------------------------*
 * FCT_GET_ARGV FCT_CHK_ARGV                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Fct_Get_Argv(FlagInf *flag)
{
  WamWord value_word, word;
  int i;

  value_word = word = Pl_Put_X_Variable();
  for (i = 0; i < pl_os_argc; i++)
    {
      Pl_Get_List(word);
      Pl_Unify_Atom(Pl_Create_Atom(pl_os_argv[i]));
      word = Pl_Unify_Variable();
    }
  Pl_Get_Nil(word);

  return value_word;
}

static Bool
Fct_Chk_Argv(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  return (tag_mask == TAG_LST_MASK || value_word == NIL_WORD);
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
  pl_sys_var[Pl_Rd_Integer(var_word)] |= ((PlULong) 1 << Pl_Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_RESET_BIT_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sys_Var_Reset_Bit_2(WamWord var_word, WamWord bit_word)
{
  pl_sys_var[Pl_Rd_Integer(var_word)] &= ~((PlULong) 1 << Pl_Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * PL_SYS_VAR_GET_BIT_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sys_Var_Get_Bit_3(WamWord var_word, WamWord bit_word, WamWord value_word)
{
  PlULong x;

  x = (pl_sys_var[Pl_Rd_Integer(var_word)] >> Pl_Rd_Integer(bit_word)) & 1;
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

  sv = (int) Pl_Rd_Integer(var_word);

  word = pl_sys_var[sv];
  tag_mask = Tag_Mask_Of(word);
  if (tag_mask == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      if (adr != NULL)
	Free(adr);   /* recover the Malloc: don't mix sys_var_put/get and others sys_var_write... on same sys variable */
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

  i = Pl_Rd_C_Int_Positive_Check(i_word);
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
  lg = (int) (value - one_env);
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
  lg = (int) (value - one_env);
  var_name = pl_glob_buff;

  strncpy(var_name, one_env, lg);
  var_name[lg] = '\0';

  value++;			/* skip = */

  return Pl_Un_String_Check(var_name, var_name_word) &&
    Pl_Get_Atom(Pl_Create_Atom(value), value_word);
}
