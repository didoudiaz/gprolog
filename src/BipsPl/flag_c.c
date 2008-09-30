/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_c.c                                                        *
 * Descr.: Prolog flag and system variable management - C Part             *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OBJ_INIT Flag_Initializer

#define FLAG_C_FILE

#include "engine_pl.h"
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

/* atom_error is already defined in the set of often used atoms */
static int atom_warning;
static int atom_fail;

static int atom_chars;
static int atom_codes;
static int atom_atom;
static int atom_chars_no_escape;
static int atom_codes_no_escape;
static int atom_atom_no_escape;

static int atom_prolog[4];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Unif_Flag(int i, WamWord value_word);



#define CURRENT_PROLOG_FLAG_ALT    X2463757272656E745F70726F6C6F675F666C61675F616C74

#define ENVIRON_ALT                X24656E7669726F6E5F616C74

Prolog_Prototype(CURRENT_PROLOG_FLAG_ALT, 0);
Prolog_Prototype(ENVIRON_ALT, 0);




/*-------------------------------------------------------------------------*
 * FLAG_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Flag_Initializer(void)
{
  atom_flag_tbl[FLAG_BOUNDED] = Create_Atom("bounded");
  atom_flag_tbl[FLAG_MAX_INTEGER] = Create_Atom("max_integer");
  atom_flag_tbl[FLAG_MIN_INTEGER] = Create_Atom("min_integer");
  atom_flag_tbl[FLAG_ROUNDING_FCT] =
    Create_Atom("integer_rounding_function");

  atom_flag_tbl[FLAG_CHAR_CONVERSION] = Create_Atom("char_conversion");
  atom_flag_tbl[FLAG_DEBUG] = Create_Atom("debug");
  atom_flag_tbl[FLAG_MAX_ARITY] = Create_Atom("max_arity");
  atom_flag_tbl[FLAG_UNKNOWN] = Create_Atom("unknown");
  atom_flag_tbl[FLAG_DOUBLE_QUOTES] = Create_Atom("double_quotes");
  atom_flag_tbl[FLAG_BACK_QUOTES] = Create_Atom("back_quotes");

  atom_flag_tbl[FLAG_SYNTAX_ERROR] = Create_Atom("syntax_error");
  atom_flag_tbl[FLAG_OS_ERROR] = Create_Atom("os_error");
  atom_flag_tbl[FLAG_MAX_ATOM] = Create_Atom("max_atom");
  atom_flag_tbl[FLAG_MAX_UNGET] = Create_Atom("max_unget");
  atom_flag_tbl[FLAG_SINGLETON_WARNING] = Create_Atom("singleton_warning");
  atom_flag_tbl[FLAG_STRICT_ISO] = Create_Atom("strict_iso");

  atom_flag_tbl[FLAG_PROLOG_NAME] = Create_Atom("prolog_name");
  atom_flag_tbl[FLAG_PROLOG_VERSION] = Create_Atom("prolog_version");
  atom_flag_tbl[FLAG_PROLOG_DATE] = Create_Atom("prolog_date");
  atom_flag_tbl[FLAG_PROLOG_COPYRIGHT] = Create_Atom("prolog_copyright");


  atom_down = Create_Atom("down");
  atom_toward_zero = Create_Atom("toward_zero");

  atom_on = Create_Atom("on");
  atom_off = Create_Atom("off");

  atom_warning = Create_Atom("warning");
  atom_fail = Create_Atom("fail");

  atom_chars = Create_Atom("chars");
  atom_codes = Create_Atom("codes");
  atom_atom = Create_Atom("atom");
  atom_chars_no_escape = Create_Atom("chars_no_escape");
  atom_codes_no_escape = Create_Atom("codes_no_escape");
  atom_atom_no_escape = Create_Atom("atom_no_escape");

  atom_prolog[0] = Create_Atom(PROLOG_NAME);
  atom_prolog[1] = Create_Atom(PROLOG_VERSION);
  atom_prolog[2] = Create_Atom(PROLOG_DATE);
  atom_prolog[3] = Create_Atom(PROLOG_COPYRIGHT);

  Flag_Value(FLAG_SINGLETON_WARNING) = 1;
  Flag_Value(FLAG_STRICT_ISO) = 1;

  Flag_Value(FLAG_DOUBLE_QUOTES) = FLAG_AS_CODES;
  Flag_Value(FLAG_BACK_QUOTES) = FLAG_AS_ATOM | FLAG_NO_ESCAPE_MASK;

#ifndef NO_USE_LINEDIT
  SYS_VAR_LINEDIT = 1;
#else
  SYS_VAR_LINEDIT = 0;
#endif
}




/*-------------------------------------------------------------------------*
 * SET_PROLOG_FLAG_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Set_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  WamWord term;
  int atom;
  int i;
  PredInf *pred;

  atom = Rd_Atom_Check(flag_word);

  for (i = 0; i < NB_OF_FLAGS; i++)
    if (atom_flag_tbl[i] == atom)
      break;

  if (i >= NB_OF_FLAGS)
    Pl_Err_Domain(domain_prolog_flag, flag_word);

  DEREF(value_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);

  switch (i)
    {
    case FLAG_BOUNDED:
      if (tag_mask != TAG_ATM_MASK ||
	  (atom != atom_true && atom != atom_false))
	goto err_value;
      goto err_perm;

    case FLAG_MAX_INTEGER:
    case FLAG_MIN_INTEGER:
    case FLAG_MAX_ARITY:
    case FLAG_MAX_ATOM:
    case FLAG_MAX_UNGET:
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

      pred = Lookup_Pred(Create_Atom((atom == atom_on)
				     ? "debug" : "nodebug"), 0);

      if (pred != NULL)
	Call_Prolog((CodePtr) (pred->codep));
      else
	Flag_Value(i) = FALSE;	/* should not occurs */
      break;

    case FLAG_UNKNOWN:
    case FLAG_SYNTAX_ERROR:
    case FLAG_OS_ERROR:
      if (tag_mask != TAG_ATM_MASK)
	goto err_value;

      if (atom == atom_error)
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
      if (tag_mask != TAG_ATM_MASK)
	goto err_value;
      goto err_perm;
    }

  return TRUE;

err_value:
  term = Put_Structure(ATOM_CHAR('+'), 2);
  Unify_Value(flag_word);
  Unify_Value(value_word);

  Pl_Err_Domain(domain_flag_value, term);


err_perm:
  Pl_Err_Permission(permission_operation_modify, permission_type_flag,
		    flag_word);

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * CURRENT_PROLOG_FLAG_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  int i;
  int atom;

  DEREF(flag_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      atom = Rd_Atom_Check(word);

      for (i = 0; i < NB_OF_FLAGS; i++)
	if (atom_flag_tbl[i] == atom)
	  break;

      if (i >= NB_OF_FLAGS)
	Pl_Err_Domain(domain_prolog_flag, flag_word);

      return Unif_Flag(i, value_word);
    }

  /* non deterministic case */
  i = 0;

  A(0) = flag_word;
  A(1) = value_word;
  A(2) = i + 1;
  Create_Choice_Point((CodePtr)
		      Prolog_Predicate(CURRENT_PROLOG_FLAG_ALT, 0), 3);

  Get_Atom(atom_flag_tbl[i], flag_word);

  return Unif_Flag(i, value_word);
}




/*-------------------------------------------------------------------------*
 * CURRENT_PROLOG_FLAG_ALT_0                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Prolog_Flag_Alt_0(void)
{
  WamWord flag_word, value_word;
  int i;


  Update_Choice_Point((CodePtr)
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

  Get_Atom(atom_flag_tbl[i], flag_word);

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
  long n;

  switch (i)
    {
    case FLAG_BOUNDED:
      atom = atom_true;
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
    case FLAG_STRICT_ISO:
      atom = (Flag_Value(i)) ? atom_on : atom_off;
      break;

    case FLAG_MAX_ARITY:
      n = MAX_ARITY;
      break;

    case FLAG_UNKNOWN:
    case FLAG_SYNTAX_ERROR:
    case FLAG_OS_ERROR:
      switch (Flag_Value(i))
	{
	case FLAG_VALUE_ERROR:
	  atom = atom_error;
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
      atom = atom_prolog[i - FLAG_PROLOG_NAME];
      break;

    case FLAG_MAX_ATOM:
      n = MAX_ATOM;
      break;

    case FLAG_MAX_UNGET:
      n = STREAM_PB_SIZE;
      break;
    }

  if (atom < 0)
    return Get_Integer(n, value_word);

 return Get_Atom(atom, value_word);
}





/*-------------------------------------------------------------------------*
 * SYS_VAR_WRITE_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Write_2(WamWord var_word, WamWord n_word)
{
  sys_var[Rd_Integer(var_word)] = Rd_Integer(n_word);
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_READ_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Sys_Var_Read_2(WamWord var_word, WamWord n_word)
{
  return Get_Integer(sys_var[Rd_Integer(var_word)], n_word);
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_INC_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Inc_1(WamWord var_word)
{
  sys_var[Rd_Integer(var_word)]++;
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_DEC_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Dec_1(WamWord var_word)
{
  sys_var[Rd_Integer(var_word)]--;
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_SET_BIT_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Set_Bit_2(WamWord var_word, WamWord bit_word)
{
  sys_var[Rd_Integer(var_word)] |= (1 << Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_RESET_BIT_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Reset_Bit_2(WamWord var_word, WamWord bit_word)
{
  sys_var[Rd_Integer(var_word)] &= ~(1 << Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_SET_BIT_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Sys_Var_Get_Bit_3(WamWord var_word, WamWord bit_word, WamWord value_word)
{
  unsigned x;

  x = (sys_var[Rd_Integer(var_word)] >> Rd_Integer(bit_word))  & 1;
  return Un_Integer(x, value_word);
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_PUT_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Sys_Var_Put_2(WamWord var_word, WamWord term_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int sv;
  int size;

  sv = Rd_Integer(var_word);

  DEREF(term_word, word, tag_mask);

  if (tag_mask == TAG_ATM_MASK || tag_mask == TAG_INT_MASK)
    {
      sys_var[sv] = word;
      return;
    }

  size = Term_Size(word);
  adr = (WamWord *) Malloc(size * sizeof(WamWord));	/* never recovered */
  Copy_Term(adr, &word);
  sys_var[sv] = Tag_REF(adr);
}




/*-------------------------------------------------------------------------*
 * SYS_VAR_GET_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Sys_Var_Get_2(WamWord var_word, WamWord term_word)
{
  WamWord word;
  WamWord *adr;
  int size;

  word = sys_var[Rd_Integer(var_word)];

  if (Tag_Mask_Of(word) == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      size = Term_Size(*adr);
      Copy_Contiguous_Term(H, adr);
      word = *H;
      H += size;
    }

  return Unify(word, term_word);
}




/*-------------------------------------------------------------------------*
 * GET_CURRENT_B_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Get_Current_B_1(WamWord b_word)
{
  WamWord word;

  Load_Cut_Level(&word);
  Unify(word, b_word);
}




/*-------------------------------------------------------------------------*
 * SET_CURRENT_B_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Current_B_1(WamWord b_word)
{
  WamWord word, tag_mask;
  
  DEREF(b_word, word, tag_mask);
  Cut(word);
}




/*-------------------------------------------------------------------------*
 * WRITE_PL_STATE_FILE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Write_Pl_State_File(WamWord file_word)
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


  file = atom_tbl[Rd_Atom_Check(file_word)].name;
  file = M_Absolute_Path_Name(file);

  f = fopen(file, "wb");
  Os_Test_Error(f == NULL);

  i = Hash_Nb_Elements(oper_tbl);
  fwrite(&i, sizeof(i), 1, f);

  for (oper = (OperInf *) Hash_First(oper_tbl, &scan); oper;
       oper = (OperInf *) Hash_Next(&scan))
    {
      sf_op.type = Type_Of_Oper(oper->a_t);
      sf_op.prec = oper->prec;
      sf_op.left = oper->left;
      sf_op.right = oper->right;
      sf_op.length = atom_tbl[Atom_Of_Oper(oper->a_t)].prop.length;
      fwrite(&sf_op, sizeof(sf_op), 1, f);
      fwrite(atom_tbl[Atom_Of_Oper(oper->a_t)].name, sf_op.length, 1, f);
    }

  i = Flag_Value(FLAG_DOUBLE_QUOTES);
  fwrite(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_BACK_QUOTES);
  fwrite(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_CHAR_CONVERSION);
  fwrite(&i, sizeof(i), 1, f);

  i = Flag_Value(FLAG_SINGLETON_WARNING);
  fwrite(&i, sizeof(i), 1, f);

  i = SYS_VAR_SAY_GETC;
  fwrite(&i, sizeof(i), 1, f);

  for (c = 0; c < 256; c++)
    if (char_conv[c] != c)
      {
	cv[0] = c;
	cv[1] = char_conv[c];
	fwrite(&cv, 2, 1, f);
      }

  cv[0] = 0;
  cv[1] = 0;
  fwrite(&cv, 2, 1, f);

  fclose(f);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * READ_PL_STATE_FILE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Read_Pl_State_File(WamWord file_word)
{
  char *file;
  FILE *f;
  int i;
  SFOp sf_op;
  int c;
  char cv[2];


  file = atom_tbl[Rd_Atom_Check(file_word)].name;
  file = M_Absolute_Path_Name(file);

  f = fopen(file, "rb");
  Os_Test_Error(f == NULL);

  Hash_Delete_All(oper_tbl);

  fread(&i, sizeof(i), 1, f);

  while (i--)
    {
      fread(&sf_op, sizeof(sf_op), 1, f);
      fread(glob_buff, sf_op.length, 1, f);
      glob_buff[sf_op.length] = '\0';
      Create_Oper(Create_Allocate_Atom(glob_buff),
		  sf_op.type, sf_op.prec, sf_op.left, sf_op.right);
    }

  fread(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_DOUBLE_QUOTES) = i;

  fread(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_BACK_QUOTES) = i;

  fread(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_CHAR_CONVERSION) = i;

  fread(&i, sizeof(i), 1, f);
  Flag_Value(FLAG_SINGLETON_WARNING) = i;

  fread(&i, sizeof(i), 1, f);
  SYS_VAR_SAY_GETC = i;

  for (;;)
    {
      fread(&cv, 2, 1, f);
      c = cv[0];
      if (c == 0 && cv[1] == 0)
	break;

      char_conv[c] = cv[1];
    }

  fclose(f);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * ARGUMENT_COUNTER_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Argument_Counter_1(WamWord n_word)
{
  return Un_Integer_Check(os_argc, n_word);
}




/*-------------------------------------------------------------------------*
 * ARGUMENT_VALUE_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Argument_Value_2(WamWord i_word, WamWord a_word)
{
  int i;

  i = Rd_Positive_Check(i_word);
  if (i >= os_argc)
    return FALSE;

  return Un_Atom_Check(Create_Atom(os_argv[i]), a_word);
}




/*-------------------------------------------------------------------------*
 * ARGUMENT_LIST_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Argument_List_1(WamWord list_word)
{
  int i;

  Check_For_Un_List(list_word);

  for (i = 1; i < os_argc; i++)
    {
      if (!Get_List(list_word) || !Unify_Atom(Create_Atom(os_argv[i])))
	return FALSE;

      list_word = Unify_Variable();
    }

  return Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * ENVIRON_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Environ_2(WamWord var_name_word, WamWord value_word)
{
  WamWord word, tag_mask;
  char *var_name;
  char *value;
  char **cur_env;
  char *one_env;
  int lg;

  Check_For_Un_Atom(value_word);

  DEREF(var_name_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      var_name = Rd_String_Check(word);
      value = (char *) getenv(var_name);

      return value && Un_String_Check(value, value_word);
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
      Create_Choice_Point((CodePtr) Prolog_Predicate(ENVIRON_ALT, 0), 3);
    }


  value = strchr(one_env, '=');
  lg = value - one_env;
  var_name = glob_buff;

  strncpy(var_name, one_env, lg);
  var_name[lg] = '\0';

  value++;			/* skip = */

  return Un_String_Check(var_name, var_name_word) &&
    Get_Atom(Create_Atom(value), value_word);
}




/*-------------------------------------------------------------------------*
 * ENVIRON_ALT_0                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Environ_Alt_0(void)
{
  WamWord var_name_word, value_word;
  char *var_name;
  char *value;
  char **cur_env;
  char *one_env;
  int lg;

  Update_Choice_Point((CodePtr) Prolog_Predicate(ENVIRON_ALT, 0), 0);

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
  var_name = glob_buff;

  strncpy(var_name, one_env, lg);
  var_name[lg] = '\0';

  value++;			/* skip = */

  return Un_String_Check(var_name, var_name_word) &&
    Get_Atom(Create_Atom(value), value_word);
}
