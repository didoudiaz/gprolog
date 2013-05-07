/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_parser.c                                                     *
 * Descr.: mini-assembler parser                                           *
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
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>


#define MA_PARSER_FILE

#include "ma_parser.h"
#include "ma_protos.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_LINE_LEN               32767
#define MAX_STR_LEN                2048
#define MAX_ARGS                   128
#define MAX_SWITCH_CASES           10240



enum
{
  PL_CODE,
  PL_JUMP,
  PREP_CP,
  HERE_CP,
  PL_CALL,
  PL_FAIL,
  PL_RET,
  JUMP,
  MOVE,
  CALL_C,
  JUMP_RET,
  FAIL_RET,
  MOVE_RET,
  SWITCH_RET,
  C_CODE,
  C_RET,
  LONG
};




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

int needs_pre_pass;		/* can be overwritten by mappers */

char *inst[] = {
  "pl_code", "pl_jump", "prep_cp", "here_cp", "pl_call", "pl_fail",
  "pl_ret", "jump", "move", "call_c", "jump_ret", "fail_ret", "move_ret",
  "switch_ret", "c_code", "c_ret", "long", NULL };


int reload_e;


char fct_name[MAX_STR_LEN];
int fc;
int nb_args;
int nb_args_in_words;		/* args counted in words (e.g. 32 bits) */
ArgInf arg[MAX_ARGS];


int nb_swt;
SwtInf swt[MAX_SWITCH_CASES];


jmp_buf jumper;




	  /* scanner variables */

int keep_source_lines;

FILE *file_in;

int cur_line_no;
char cur_line_str[MAX_LINE_LEN];
char *cur_line_p;
char *beg_last_token;

char str_val[MAX_STR_LEN];
PlLong int_val;
double dbl_val;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Parser(int pass_no, int nb_passes);

static int Read_If_Global(int initializer);

static void Read_Function(void);

static void Read_Switch(void);

static int Read_Index(void);

static int Read_Optional_Index(void);

static int Read_Token(int what);

static int Scanner(void);




/*-------------------------------------------------------------------------*
 * PARSE_MA_FILE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Parse_Ma_File(char *file_name_in, int comment)
{
  int ret_val;
  int i, nb_passes = needs_pre_pass + 1;


  if (file_name_in == NULL)
    {
      file_name_in = "stdin";
      file_in = stdin;
    }
  else if ((file_in = fopen(file_name_in, "rt")) == NULL)
    {
      fprintf(stderr, "cannot open input file %s\n", file_name_in);
      return 0;
    }

  for(i = 1; i <= nb_passes; i++)
    {
      if (i == 2 && fseek(file_in, 0, SEEK_SET) == -1)
	{
	  fprintf(stderr, "cannot reposition file %s (needed for 2 passes)\n",
		  file_name_in);
	  return 0;
	}

      keep_source_lines = comment;

      if ((ret_val = setjmp(jumper)) == 0)
	Parser(i, nb_passes);

      if (ret_val != 0)
	return 0;
    }

  if (file_in != stdin)
    fclose(file_in);

  return 1;
}




/*-------------------------------------------------------------------------*
 * PARSER                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/

#define Pre_Pass() (pass_no < nb_passes)

static void
Parser(int pass_no, int nb_passes)
{
  int init_already_read = 0;
  char **in, *name;
  int k, i;
  int global;

  if (Pre_Pass())
    keep_source_lines = 0;

  cur_line_p = cur_line_str;
  cur_line_str[0] = '\0';
  cur_line_no = 0;

  for (;;)
    {
      k = Scanner();
      if (k == 0)		/* end of file */
	break;

      if (k != IDENTIFIER)
	Syntax_Error("miniasm declaration or instruction expected");

      for (in = inst; *in; in++)
	if (strcmp(str_val, *in) == 0)
	  break;

      k = in - inst;

      /* ignore it in Pre_Pass() or long decl if Pre_Pass() done before */
      if ((Pre_Pass() && k != PL_CODE && k != C_CODE && k != LONG && *in != NULL) ||
	  (pass_no > 1 && k == LONG)) {
	*cur_line_p = '\0';	/* skip rest of line */
	continue;
      }

      switch (k)
	{
	case PL_CODE:
	  global = Read_If_Global(0);
	  Read_Token(IDENTIFIER);
	  if (Pre_Pass())
	    Decl_Code(strdup(str_val), 1, global);
	  else
	    {
	      Code_Start(str_val, 1, global);
	      reload_e = 1;
	    }
	  break;

	case PL_JUMP:
	  Read_Token(IDENTIFIER);
	  Pl_Jump(str_val);
	  reload_e = 1;
	  break;

	case PREP_CP:
	  Prep_CP();
	  break;

	case HERE_CP:
	  Here_CP();
	  break;

	case PL_CALL:
	  Read_Token(IDENTIFIER);
	  Pl_Call(str_val);
	  reload_e = 1;
	  break;

	case PL_FAIL:
	  Pl_Fail();
	  reload_e = 1;
	  break;

	case PL_RET:
	  Pl_Ret();
	  reload_e = 1;
	  break;

	case JUMP:
	  Read_Token(IDENTIFIER);
	  Jump(str_val);
	  reload_e = 1;
	  break;

	case MOVE:
	  k = Read_Token(X_REG);
	  i = Read_Index();
	  if (k == X_REG)
	    Move_From_Reg_X(i);
	  else
	    Move_From_Reg_Y(i);

	  Read_Token(',');

	  k = Read_Token(X_REG);
	  i = Read_Index();
	  if (k == X_REG)
	    Move_To_Reg_X(i);
	  else
	    Move_To_Reg_Y(i);
	  break;

	case CALL_C:
	  Read_Function();
	  Call_C(fct_name, fc, nb_args, nb_args_in_words, arg);
	  break;

	case JUMP_RET:
	  Jump_Ret();
	  reload_e = 1;
	  break;

	case FAIL_RET:
	  Fail_Ret();
	  break;

	case MOVE_RET:
	  switch ((k = Scanner()))
	    {
	    case IDENTIFIER:
	      Move_Ret_To_Mem_L(str_val, Read_Optional_Index());
	      break;

	    case X_REG:
	      Move_Ret_To_Reg_X(Read_Index());
	      break;

	    case Y_REG:
	      Move_Ret_To_Reg_Y(Read_Index());
	      break;

	    case FL_ARRAY:
	      Move_Ret_To_Foreign_L(Read_Index());
	      break;

	    case FD_ARRAY:
	      Move_Ret_To_Foreign_D(Read_Index());
	      break;

	    default:
	      Syntax_Error("identifier, X(...), Y(...), FL(...) or FD(...) expected");
	      break;

	    }
	  break;

	case SWITCH_RET:
	  Read_Switch();
	  Switch_Ret(nb_swt, swt);
	  break;

	case C_CODE:
	  global = Read_If_Global(!init_already_read);

	  Read_Token(IDENTIFIER);
	  if (global == 2)
	    {
	      init_already_read = 1;
	      global = 0;
	      if (!Pre_Pass())
		Declare_Initializer(strdup(str_val));
	    }
	  if (Pre_Pass())
	    Decl_Code(strdup(str_val), 0, global);
	  else
	    Code_Start(str_val, 0, global);
	  break;

	case C_RET:
	  C_Ret();
	  break;

	case LONG:
	  global = Read_If_Global(1);

	  Read_Token(IDENTIFIER);
	  name = strdup(str_val);
	  if ((i = Read_Optional_Index()) > 0)	/* array */
	    {
	      Decl_Long(name, global, ARRAY_SIZE, i);
	      break;
	    }

	  while (isspace(*cur_line_p))
	    cur_line_p++;
	  if (*cur_line_p != '=')
	    {
	      Decl_Long(name, global, NONE, 1); /* default: NONE as value = 1 (value = array size = 1) */
	      break;
	    }
	  cur_line_p++;		/* skip the = */
	  Read_Token(INTEGER);
	  Decl_Long(name, global, INITIAL_VALUE, int_val);
	  break;

	default:
	  if (*in == NULL)
	    {
	      Read_Token(':');
	      if (Pre_Pass())
		Decl_Code(strdup(str_val), 1, global);
	      else
		Label(str_val);
	    }
	}
    }
}




/*-------------------------------------------------------------------------*
 * READ_IF_GLOBAL                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_If_Global(int initializer)
{
  if (Scanner() != IDENTIFIER)
    goto err;

  if (strcmp(str_val, "local") == 0)
    return 0;

  if (strcmp(str_val, "global") == 0)
    return 1;

  if (initializer && strcmp(str_val, "initializer") == 0)
    return 2;

err:
  if (!initializer)
    Syntax_Error("local / global expected");
  else
    Syntax_Error("local / global / initializer expected");

  return 0;
}




/*-------------------------------------------------------------------------*
 * READ_FUNCTION                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Read_Function(void)
{
  int k;

  fc = 0;
  Read_Token(IDENTIFIER);
  if (strcmp(str_val, "fast") == 0)
    {
      fc = 1;
      Read_Token(IDENTIFIER);
    }

  strcpy(fct_name, str_val);
  nb_args = 0;
  nb_args_in_words = 0;
  Read_Token('(');
  k = Scanner();
  if (k == ')')
    return;

  for (;;)
    {
      arg[nb_args].type = k;
      arg[nb_args].adr_of = 0;
    one_arg:
      switch (k)
	{
	case '&':
	  k = Scanner();
	  if (k != IDENTIFIER && k != X_REG && k != Y_REG && k != FL_ARRAY
	      && k != FD_ARRAY)
	    Syntax_Error("identifier, X(...), Y(...), FL(...) or FD(...) expected");
	  arg[nb_args].type = k;
	  arg[nb_args].adr_of = 1;
	  goto one_arg;

	case STRING:
	  arg[nb_args].t.str_val = strdup(str_val);
	  break;

	case INTEGER:
	  arg[nb_args].t.int_val = int_val;
	  break;

	case FLOAT:
	  nb_args_in_words++;	/* double count 1 word more */
	  arg[nb_args].t.dbl_val = dbl_val;
	  break;

	case IDENTIFIER:
	  arg[nb_args].type = MEM;
	  arg[nb_args].t.mem.name = strdup(str_val);
	  arg[nb_args].t.mem.index = Read_Optional_Index();
	  break;

	case FD_ARRAY:
	  if (arg[nb_args].adr_of == 0)
	    nb_args_in_words++;	/* double count 1 word more */
	case FL_ARRAY:
	case X_REG:
	case Y_REG:
	  arg[nb_args].t.index = Read_Index();
	  break;
	}

      k = Scanner();
      nb_args++;
      nb_args_in_words++;
      if (k == ')')
	break;

      if (k != ',')
	Syntax_Error(") or , expected");
      k = Scanner();
    }
}




/*-------------------------------------------------------------------------*
 * READ_SWITCH                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Read_Switch(void)
{
  int k;

  Read_Token('(');
  nb_swt = 0;
  for (;;)
    {
      if (Scanner() != INTEGER)
	Syntax_Error("integer expected");

      Read_Token('=');
      Read_Token(IDENTIFIER);
      swt[nb_swt].int_val = int_val;
      swt[nb_swt].label = strdup(str_val);

      nb_swt++;
      k = Scanner();
      if (k == ')')
	break;
      if (k != ',')
	Syntax_Error(") or , expected");
    }
}




/*-------------------------------------------------------------------------*
 * READ_INDEX                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Index(void)
{
  Read_Token('(');
  Read_Token(INTEGER);
  Read_Token(')');

  return int_val;
}




/*-------------------------------------------------------------------------*
 * READ_OPTIONAL_INDEX                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Optional_Index(void)
{
  return (*cur_line_p == '(') ? Read_Index() : 0;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Token(int what)
{
  char str[80];
  int k;

  k = Scanner();

  if (k == what || (what == X_REG && k == Y_REG)
      || (what == FL_ARRAY && k == FD_ARRAY))
    return k;

  switch (what)
    {
    case IDENTIFIER:
      Syntax_Error("identifier expected");
      break;

    case STRING:
      Syntax_Error("string expected");
      break;

    case INTEGER:
      Syntax_Error("integer expected");
      break;

    case FLOAT:
      Syntax_Error("float expected");
      break;

    case X_REG:
      Syntax_Error("X(...) or Y(...) expected");
      break;

    case FL_ARRAY:
      Syntax_Error("FL(...) or FD(...) expected");
      break;

    default:
      sprintf(str, "%c expected", what);
      Syntax_Error(str);
      break;
    }

  return k;			/* for the compiler */
}




/*-------------------------------------------------------------------------*
 * SCANNER                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Scanner(void)
{
  char *p, *p1;
  PlLong i;
  double d;
  double strtod();


  for (;;)
    {
      while (isspace(*cur_line_p))
	cur_line_p++;

      if (*cur_line_p != '\0' && *cur_line_p != ';')
	break;

      if (fgets(cur_line_str, sizeof(cur_line_str), file_in)) /* to avoid gcc warning warn_unused_result */
	{
	}


      if (feof(file_in))
	return 0;

      cur_line_no++;
      cur_line_p = cur_line_str;

      if (keep_source_lines)
	{
	  while (isspace(*cur_line_p))
	    cur_line_p++;

	  if (*cur_line_p)
	    {
	      p = cur_line_p + strlen(cur_line_p) - 1;
	      if (*p == '\n')
		*p = '\0';
	      Label_Printf("\t%s %6d: %s", comment_prefix, cur_line_no,
			   cur_line_p);
	    }
	}
    }

  beg_last_token = cur_line_p;

  if (*cur_line_p == '"')	/* string */
    {
      p = str_val;
      *p++ = '"';
      cur_line_p++;
      while (*cur_line_p != '"')
	{
	  if ((*p++ = *cur_line_p++) == '\\')
	    *p++ = *cur_line_p++;
	}

      cur_line_p++;
      if (strings_need_null)
	{
	  *p++ = '\\';
	  *p++ = '0';
	}
      *p++ = '"';
      *p = '\0';
      return STRING;
    }

  if (isalpha(*cur_line_p) || *cur_line_p == '_')	/* identifier */
    {
      p = str_val;
      while (isalnum(*cur_line_p) || *cur_line_p == '_')
	*p++ = *cur_line_p++;

      *p = '\0';

      if (str_val[0] == 'X' && str_val[1] == '\0' && *cur_line_p == '(')
	return X_REG;

      if (str_val[0] == 'Y' && str_val[1] == '\0' && *cur_line_p == '(')
	{
	  if (reload_e)
	    {
	      Reload_E_In_Register();
	      reload_e = 0;
	    }
	  return Y_REG;
	}

      if (strcmp(str_val, "FL") == 0 && *cur_line_p == '(')
	return FL_ARRAY;

      if (strcmp(str_val, "FD") == 0 && *cur_line_p == '(')
	return FD_ARRAY;

      return IDENTIFIER;
    }

  i = Str_To_PlLong(cur_line_p, &p, 0);
  if (p == cur_line_p)		/* not an integer return that character */
    return *cur_line_p++;
  d = strtod(cur_line_p, &p1);

  if (p1 == p)			/* integer */
    {
      int_val = i;
      cur_line_p = p;
      return INTEGER;
    }
  /* float */
  dbl_val = d;
  cur_line_p = p1;
  return FLOAT;
}




/*-------------------------------------------------------------------------*
 * PL_SYNTAX_ERROR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Syntax_Error(char *s)
{
  char *p = cur_line_str + strlen(cur_line_str) - 1;

  if (*p == '\n')
    *p = '\0';

  fprintf(stderr, "line %d: %s\n", cur_line_no, s);
  fprintf(stderr, "%s\n", cur_line_str);

  for (p = cur_line_str; p < beg_last_token; p++)
    if (!isspace(*p))
      *p = ' ';

  *p = '\0';
  fprintf(stderr, "%s^ here\n", cur_line_str);

  longjmp(jumper, 1);
}
