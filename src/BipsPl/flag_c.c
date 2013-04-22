/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_c.c                                                        *
 * Descr.: Prolog flag and system variable management - C Part             *
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
static int atom_the_dialect;
static int atom_the_cc;



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
  atom_the_dialect = Pl_Create_Atom(PROLOG_DIALECT);
  atom_the_cc = Pl_Create_Atom(CC);

  /* Unchangeable flags */

  NEW_FLAG_ATOM   (prolog_name,           PROLOG_NAME);
  NEW_FLAG_ATOM   (prolog_version,        PROLOG_VERSION);
  NEW_FLAG_ATOM   (prolog_date,           PROLOG_DATE);
  NEW_FLAG_ATOM   (prolog_copyright,      PROLOG_COPYRIGHT);

  NEW_FLAG_ATOM   (dialect,               PROLOG_DIALECT);

  NEW_FLAG_INTEGER(version,               __GPROLOG_VERSION__);
  Pl_New_Prolog_Flag("version_data",      FALSE, PF_TYPE_ANY, 0, Fct_Get_Version_Data, Fct_Chk_Version_Data, NULL);
  NEW_FLAG_BOOL   (bounded,               TRUE);

  NEW_FLAG_INTEGER(max_integer,           INT_GREATEST_VALUE);    
  NEW_FLAG_INTEGER(min_integer,           INT_LOWEST_VALUE);
  NEW_FLAG_ROUND  (integer_rounding_function, ((-3 / 2) == -1) ? PF_ROUND_ZERO : PF_ROUND_DOWN);

  NEW_FLAG_INTEGER(max_arity,             MAX_ARITY);
  NEW_FLAG_INTEGER(max_atom,              pl_max_atom);
  NEW_FLAG_INTEGER(max_unget,             STREAM_PB_SIZE);

  NEW_FLAG_ATOM   (home,                  pl_home ? pl_home : "");
  NEW_FLAG_ATOM   (host_os,               M_OS);
  NEW_FLAG_ATOM   (host_vendor,           M_VENDOR);
  NEW_FLAG_ATOM   (host_cpu,              M_CPU);
  NEW_FLAG_ATOM   (host,                  M_CPU "-" M_VENDOR "-" M_OS);
  NEW_FLAG_ATOM   (arch,                  M_CPU "-" M_OS);
  NEW_FLAG_INTEGER(address_bits,          WORD_SIZE);
  NEW_FLAG_BOOL   (unix,                  is_unix);

  NEW_FLAG_ATOM   (compiled_at,           COMPILED_AT); /* see arch_dep.h */
  NEW_FLAG_ATOM   (c_cc,                  CC);
  Pl_New_Prolog_Flag("c_cc_version_data", FALSE, PF_TYPE_ANY, 1, Fct_Get_Version_Data, Fct_Chk_Version_Data,  NULL);
  NEW_FLAG_ATOM   (c_cflags,              CFLAGS_MACHINE " " CFLAGS);
  NEW_FLAG_ATOM   (c_ldflags,             LDFLAGS);                            

  Pl_New_Prolog_Flag("argv",              FALSE, PF_TYPE_ANY, 0, Fct_Get_Argv, Fct_Chk_Argv, NULL);

  /* changeable flags */

  NEW_FLAG_ON_OFF (char_conversion,       0);
  NEW_FLAG_ON_OFF (singleton_warning,     1);
  NEW_FLAG_ON_OFF (suspicious_warning,    1);
  NEW_FLAG_ON_OFF (multifile_warning,     1);
  NEW_FLAG_ON_OFF (strict_iso,            1);
#if 0
  NEW_FLAG_ON_OFF (debug,                 0);
#else  /* to have a customized Set function */
  pl_flag_debug = Pl_New_Prolog_Flag("debug", TRUE, PF_TYPE_ON_OFF, 0, NULL, NULL, Fct_Set_Debug);
#endif


  NEW_FLAG_QUOTES(double_quotes,          PF_QUOT_AS_CODES);

  /* DON'T CHANGE back_quotes default: no_escape is useful under
   * Windows when assoc .pl to gprolog (see InnoSetup) and avoid \ (backslash)
   * to be misinterpreted in pathnames (e.g. c:\foo\bar).
   */
  NEW_FLAG_QUOTES(back_quotes,            PF_QUOT_AS_ATOM | PF_QUOT_NO_ESCAPE_MASK);

  NEW_FLAG_ERR    (unknown,               PF_ERR_ERROR);
  NEW_FLAG_ERR    (syntax_error,          PF_ERR_ERROR);
  NEW_FLAG_ERR    (os_error,              PF_ERR_ERROR);


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

  i = Flag_Value(double_quotes);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(back_quotes);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(char_conversion);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(singleton_warning);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(suspicious_warning);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(multifile_warning);
  FWRITE(&i, sizeof(i), 1, f);

  i = Flag_Value(strict_iso);
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
  Flag_Value(double_quotes) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(back_quotes) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(char_conversion) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(singleton_warning) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(suspicious_warning) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(multifile_warning) = i;

  FREAD(&i, sizeof(i), 1, f);
  Flag_Value(strict_iso) = i;

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
