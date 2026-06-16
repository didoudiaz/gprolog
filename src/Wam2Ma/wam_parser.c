/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam_parser.c                                                    *
 * Descr.: parser                                                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "../EnginePl/gp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <locale.h>
#include <setjmp.h>

#include "wam_parser.h"
#include "wam_protos.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_FCT_ARITY              10
#define MAX_LINE_LEN               65536
#define MAX_STR_LEN                32768
#define MAX_ARGS                   1000000 /* for big swith_on_... */




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  char *keyword;
  void (*fct) (ArgVal arg[]);
  int nb_args;
  ArgTyp arg_type[MAX_FCT_ARITY];
}
ParseInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

ParseInf decl[] = {
  {"file_name", F_file_name, 1, {ATOM}},
  {"directive", F_directive, 3, {C_INT, ATOM, LIST_INST}},
  {"predicate", F_predicate, 7, {MP_N, C_INT, ATOM, ATOM, ATOM, ATOM, LIST_INST}},
  {"ensure_linked", F_ensure_linked, 1, {L1(MP_N)}},
  {NULL, NULL, 0, {0}}
};



ParseInf inst[] = {
  {"get_variable", F_get_variable, 2, {X_Y, C_INT}},
  {"get_value", F_get_value, 2, {X_Y, C_INT}},
  {"get_atom", F_get_atom, 2, {ATOM, C_INT}},
  {"get_integer", F_get_integer, 2, {INTEGER, C_INT}},
  {"get_float", F_get_float, 2, {FLOAT, C_INT}},
  {"get_nil", F_get_nil, 1, {C_INT}},
  {"get_list", F_get_list, 1, {C_INT}},
  {"get_structure", F_get_structure, 2, {F_N, C_INT}},

  {"put_variable", F_put_variable, 2, {X_Y, C_INT}},
  {"put_void", F_put_void, 1, {C_INT}},
  {"put_value", F_put_value, 2, {X_Y, C_INT}},
  {"put_unsafe_value", F_put_unsafe_value, 2, {X_Y, C_INT}},
  {"put_atom", F_put_atom, 2, {ATOM, C_INT}},
  {"put_integer", F_put_integer, 2, {INTEGER, C_INT}},
  {"put_float", F_put_float, 2, {FLOAT, C_INT}},
  {"put_nil", F_put_nil, 1, {C_INT}},
  {"put_list", F_put_list, 1, {C_INT}},
  {"put_structure", F_put_structure, 2, {F_N, C_INT}},
  {"put_meta_term", F_put_meta_term, 3, {ATOM, C_INT, C_INT}},
  {"math_load_value", F_math_load_value, 2, {X_Y, C_INT}},
  {"math_fast_load_value", F_math_fast_load_value, 2, {X_Y, C_INT}},

  {"unify_variable", F_unify_variable, 1, {X_Y}},
  {"unify_void", F_unify_void, 1, {C_INT}},
  {"unify_value", F_unify_value, 1, {X_Y}},
  {"unify_local_value", F_unify_local_value, 1, {X_Y}},
  {"unify_atom", F_unify_atom, 1, {ATOM}},
  {"unify_integer", F_unify_integer, 1, {INTEGER}},
  {"unify_nil", F_unify_nil, 0, {0}},
  {"unify_list", F_unify_list, 0, {0}},
  {"unify_structure", F_unify_structure, 1, {F_N}},

  {"allocate", F_allocate, 1, {C_INT}},
  {"deallocate", F_deallocate, 0, {0}},

  {"call", F_call, 1, {MP_N}},
  {"execute", F_execute, 1, {MP_N}},
  {"proceed", F_proceed, 0, {0}},
  {"fail", F_fail, 0, {0}},

  {"label", F_label, 1, {C_INT}},

  {"switch_on_term", F_switch_on_term, 5, {LABEL, LABEL, LABEL, LABEL, LABEL}},
  {"switch_on_atom", F_switch_on_atom, 1, {L2(ATOM, C_INT)}},
  {"switch_on_integer", F_switch_on_integer, 1, {L2(INTEGER, C_INT)}},
  {"switch_on_structure", F_switch_on_structure, 1, {L2(F_N, C_INT)}},

  {"try_me_else", F_try_me_else, 1, {C_INT}},
  {"retry_me_else", F_retry_me_else, 1, {C_INT}},
  {"trust_me_else_fail", F_trust_me_else_fail, 0, {0}},

  {"try", F_try, 1, {C_INT}},
  {"retry", F_retry, 1, {C_INT}},
  {"trust", F_trust, 1, {C_INT}},

  {"pragma_arity", F_pragma_arity, 1, {C_INT}},

  {"get_current_choice", F_get_current_choice, 1, {X_Y}},
  {"cut", F_cut, 1, {X_Y}},
  {"soft_cut", F_soft_cut, 1, {X_Y}},

  {"call_c", F_call_c, 3, {ATOM, L1(ANY), L1(ANY)}},

  {"foreign_call_c", F_foreign_call_c, 5, {ATOM, ATOM, F_N, C_INT, L2(ATOM, ATOM)}},

  {NULL, NULL, 0, {0}}
};


ArgVal arg[MAX_ARGS];


jmp_buf jumper;




	  /* lexer variables */

Bool keep_source_lines;

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

static void Parser(void);

static Bool Parse_And_Treat_Decl_Or_Inst(ParseInf *in);

static void Read_Argument(ArgTyp arg_type, ArgVal **top);

static void Read_Token(ArgTyp what);

static ArgTyp Lexer(Bool complex_atom);

static char Peek_Char(Bool skip_spaces);



/*-------------------------------------------------------------------------*
 * PARSE_MAM_FILE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Parse_Wam_File(char *file_name_in, Bool comment)
{
  int ret_val;

  keep_source_lines = comment;

  if (file_name_in == NULL)
    file_in = stdin;
  else if ((file_in = fopen(file_name_in, "rt")) == NULL)
    {
      fprintf(stderr, "cannot open input file %s\n", file_name_in);
      return FALSE;
    }

  cur_line_p = cur_line_str;
  cur_line_str[0] = '\0';
  cur_line_no = 0;

  if ((ret_val = setjmp(jumper)) == 0)
    Parser();

  if (file_in != stdin)
    fclose(file_in);

  return ret_val == 0;
}




/*-------------------------------------------------------------------------*
 * PARSER                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parser(void)
{
  for(;;)
    {
      if (!Parse_And_Treat_Decl_Or_Inst(decl)) /* end of file */
	break;
      Read_Token('.');
    }
}




/*-------------------------------------------------------------------------*
 * READ_DECL_OR_INST                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Parse_And_Treat_Decl_Or_Inst(ParseInf *what)
{
  ParseInf *in = what;
  ArgVal *top = arg;
  ArgTyp t;
  int i;
  Bool fct_called = FALSE;

  t = Lexer(FALSE);
  if (t == END_OF_FILE && what == decl)
    return FALSE;

  if (t != ATOM)
    Syntax_Error((what == decl) ? "wam declaration expected" : "wam instruction expected");

  for(in = what; in->keyword && strcmp(str_val, in->keyword) != 0; in++)
    ;

  if (in->keyword == NULL)
    Syntax_Error((what == decl) ? "unknown wam declaration" : "unknown wam instruction");

  if (in->nb_args)
    {
      Read_Token('(');
      for (i = 0; i < in->nb_args; i++)
	{
	  if (i > 0)
	    Read_Token(',');

	  if (in->arg_type[i] == LIST_INST)
	    {
	      (*in->fct) (arg);
	      fct_called = TRUE;

	      Read_Token('[');
	      for (;;)
		{
		  /* recursive call works because LIST_INST is the last argument of decl/predicate */
		  Parse_And_Treat_Decl_Or_Inst(inst); 
		  t = Lexer(FALSE);
		  if (t == ']')
		    break;
		  if (t != ',')
		    Syntax_Error("] or , expected");
		}
	    }
	  else
	    Read_Argument(in->arg_type[i], &top);
	}
      Read_Token(')');
    }

  if (!fct_called)
    (*in->fct) (arg);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * READ_ARGUMENT                                                           *
 *                                                                         *
 * arguments are loaded in the array 'arg' as follows:                     *
 *                                                                         *
 * ATOM      : the (char *) pointing to a copy of the associated string    *
 * INTEGER   : the associated (PlLong)                                     *
 * C_INT     : the associated (int)                                        *
 * FLOAT     : the associated (double)                                     *
 * X_Y       : the (int) associated to the var no (Y vars from 5000)       *
 * F_N       : the loading of ATOM (F) and the loading of C_INT (N)        *
 * MP_N      : the loading of ATOM (M) or NULL (no module) followed by F_N *
 *             the loading of ATOM (F) and the loading of C_INT (N)        *
 * LABEL     : the associated (int) or -1 for 'fail'                       *
 * ANY       : the type of the arg (a C_INT) and the loading of arg        *
 * L1(T)     : an (int) n associated to the number of elements and         *
 *             n * the loading of T                                        *
 * L2(T1, T2): an (int) n associated to the number of elements and         *
 *             n * (the loading of T1 followed by the loading of T2)       *
 *-------------------------------------------------------------------------*/
static void
Read_Argument(ArgTyp arg_type, ArgVal **top)
{
  int n;
  ArgVal *top1;
  ArgTyp t, t1, t2;
  Bool adr_of = FALSE;

  switch (arg_type)
    {
    case ATOM:
      Read_Token(ATOM);
    rest_of_atom:
      Add_Arg(*top, char *, strdup(str_val));
      return;

    case INTEGER:
      Read_Token(INTEGER);
    rest_of_integer:
      Add_Arg(*top, PlLong, int_val);
      return;

    case C_INT:
      Read_Token(INTEGER);
      Add_Arg(*top, int, (int) int_val);
      return;

    case FLOAT:
      Read_Token(FLOAT);
    rest_of_float:
      Add_Arg(*top, double, dbl_val);
      return;

    case X_Y:
      if (Lexer(FALSE) != ATOM ||
	  (*str_val != 'x' && *str_val != 'y') || str_val[1] != '\0')
	Syntax_Error("x(...) or y(...) expected");
    rest_of_x_y:
      Read_Token('(');
      Read_Token(INTEGER);
      Read_Token(')');
      if (*str_val == 'x')
	Add_Arg(*top, int, (int) int_val);
      else
	Add_Arg(*top, int, 5000 + (int) int_val);
      if (adr_of)
	Read_Token(')');	/* closing parenthesis of &(...) */
      return;

    case F_N:
      Read_Token(ATOM);
    rest_of_f_n:
      Add_Arg(*top, char *, strdup(str_val));
      Read_Token('/');
      Read_Argument(INTEGER, top);
      return;

    case MP_N:
      Read_Token(ATOM);
    rest_of_mp_n:
      t = Lexer(FALSE);
      if (t == ':')
	{
	  Add_Arg(*top, char *, strdup(str_val));
	  Read_Token(ATOM);
	  Add_Arg(*top, char *, strdup(str_val));
	  Read_Token('/');
	}
      else if (t == '/')
	{
	  Add_Arg(*top, char *, NULL);
	  Add_Arg(*top, char *, strdup(str_val));
	}
      else
	Syntax_Error("/ or : expected");

      Read_Argument(INTEGER, top);
      if (adr_of)
	Read_Token(')');	/* closing parenthesis of &(...) */
      return;

    case LABEL:
      t = Lexer(FALSE);
      if (t != INTEGER)
	{
	  if (t != ATOM || strcmp(str_val, "fail") != 0)
	    Syntax_Error("label or fail expected");
	  else
	    int_val = -1;
	}
      Add_Arg(*top, int, (int) int_val);
      return;

    case ANY:
      t = Lexer(TRUE);
      top1 = *top;              /* to update type if needed */
      Add_Arg(*top, int, t);

      if (t == INTEGER)
	goto rest_of_integer;
	
      if (t == FLOAT)
	goto rest_of_float;

      if (t != ATOM)
	Syntax_Error("x(...), y(...), &(...), atom, integer or float expected");

				/* t is an ATOM */
      adr_of = FALSE;
      if (*str_val == '&' && str_val[1] == '\0' && Peek_Char(FALSE) == '(')
	{
	  adr_of = TRUE;
	  Read_Token('(');	/* opening parenthesis of &(...) */
	  t = Lexer(TRUE);	/* should be x(...), y(...) or MP_N */
	  if (t != ATOM)
	    Syntax_Error("x(...), y(...) or pred/n expected");
	}
      
      if ((*str_val == 'x' || *str_val == 'y') && str_val[1] == '\0' && Peek_Char(FALSE) == '(')
	{
	  Add_Arg(top1, int, X_Y + adr_of); /* patch: either X_Y or ADR_OF_X_Y */
	  goto rest_of_x_y;
	}

      if (adr_of)		/* case &(MP_N) */
	{
	  Add_Arg(top1, int, MP_N + adr_of); /* either MP_N or ADR_OF_MP_N */
	  goto rest_of_mp_n;
	}

				/* atom or func/n */
      if (Peek_Char(TRUE) != '/')
	goto rest_of_atom;

      Add_Arg(top1, int, F_N);	/* patch: F_N */
      goto rest_of_f_n;

    case END_OF_FILE:
    case ADR_OF_X_Y:
    case ADR_OF_MP_N:
    case LIST_INST:             /* should not occur */
      fprintf(stderr, "BAD Read_Argument(%d): invalid expected type!!!\n", arg_type);
      return;
    }

				/* arg_type is a list L1(t1) or L2(t1, t2) */
  DECODE_L2(arg_type, t1, t2);

  top1 = *top;
  Add_Arg(*top, int, 0);	/* reserve space for counter */

  n = 0;
  t = Lexer(TRUE);
  if (t == ATOM && strcmp(str_val, "[]") == 0)	/* empty list */
    return;
  if (t != '[')
    Syntax_Error("[] or [ expected");

  for (;;)
    {
      n++;
      if (t2 == 0)		/* case L1(t1) */
	Read_Argument(t1, top);
      else			/* case L2(t1, t2) */
	{
	  Read_Token('(');
	  Read_Argument(t1, top);
	  Read_Token(',');
	  Read_Argument(t2, top);
	  Read_Token(')');
	}

      t = Lexer(FALSE);
      if (t == ']')
	break;
      if (t != ',')
	Syntax_Error("] or , expected");
    }
  Add_Arg(top1, int, n);	/* patch the nb of elements */
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Read_Token(ArgTyp what)
{
  ArgTyp t = Lexer(what == ATOM);

  if (t == what)
    return;

  if (what >= 256 && t == '(')
    {
      Read_Token(what);		/* maybe ( what ) (useful for operators) */
      Read_Token(')');
      return;
    }

  switch (what)
    {
    case ATOM:
      Syntax_Error("atom expected");
      break;

    case INTEGER:
      Syntax_Error("integer expected");
      break;

    case FLOAT:
      Syntax_Error("float expected");
      break;

    default:
      Syntax_Error("%c expected", what);
      break;
    }
}




/*-------------------------------------------------------------------------*
 * LEXER                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static ArgTyp
Lexer(Bool complex_atom)
{
  char *p, *p1;
  PlLong i;
  double d;
  double strtod(const char *nptr, char **endptr); /* not always in stdlib.h ? */


  for (;;)
    {
      while (isspace(*cur_line_p))
	cur_line_p++;

      if (*cur_line_p != '\0' && *cur_line_p != '%')
	break;

      if (fgets(cur_line_str, sizeof(cur_line_str), file_in)) /* to avoid gcc warning warn_unused_result */
	{
	}

      if (feof(file_in))
	return END_OF_FILE;

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
	      Source_Line(cur_line_no, cur_line_p);
	    }
	}
    }

  beg_last_token = cur_line_p;

  if (*cur_line_p == '\'')	/* quoted atom */
    {
      p = str_val;
      cur_line_p++;
      while (*cur_line_p != '\'' || cur_line_p[1] == '\'')
	{
	  if (*cur_line_p == '\'')
	    {
	      *p++ = '\'';
	      cur_line_p += 2;
	      continue;
	    }

	  if (*cur_line_p == '\"')
	    {
	      *p++ = '\\';
	      *p++ = '"';
	      cur_line_p++;
	      continue;
	    }

	  if ((*p++ = *cur_line_p++) == '\\')
	    {
	      if (*cur_line_p == '\\' ||	/* \\ */
		  strchr("abfnrtv", *cur_line_p))	/* \a \b \f \n \r \t \v */
		*p++ = *cur_line_p++;
	      else
		{
		  if (*cur_line_p == 'x')
		    {
		      cur_line_p++;
		      i = 16;
		    }
		  else
		    i = 8;
		  i = strtol(cur_line_p, &p1, (int) i);	/* stop on the closing \ */
		  cur_line_p = p1 + 1;
		  sprintf(p, "%03" PL_FMT_o, i);
		  p += 3;
		}
	    }
	}

      cur_line_p++;
      *p = '\0';
      return ATOM;
    }

  if (isalpha(*cur_line_p) || *cur_line_p == '_')	/* atom */
    {
      p = str_val;
      while (isalnum(*cur_line_p) || *cur_line_p == '_')
	*p++ = *cur_line_p++;

      *p = '\0';
      return ATOM;
    }

  if (complex_atom)
    {
      if ((cur_line_p[0] == '[' && cur_line_p[1] == ']') ||	/* [] and {} */
	  (cur_line_p[0] == '{' && cur_line_p[1] == '}'))
	{
	  str_val[0] = *cur_line_p++;
	  str_val[1] = *cur_line_p++;
	  str_val[2] = '\0';
	  return ATOM;
	}

      if (strchr("#$&*+-./:<=>?@\\^~", *cur_line_p))	/* symbol char */
	{
	  p = str_val;
	  do
	    {
	      if (*cur_line_p == '"' || *cur_line_p == '\\')
		*p++ = '\\';
	      *p++ = *cur_line_p++;
	    }
	  while (strchr("#$&*+-./:<=>?@\\^~", *cur_line_p));
	  *p = '\0';
	  return ATOM;
	}

      if (strchr("!;,", *cur_line_p))	/* solo char */
	{
	  str_val[0] = *cur_line_p++;
	  str_val[1] = '\0';
	  return ATOM;
	}
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
 * PEEK_CHAR                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char
Peek_Char(Bool skip_spaces)
{
  char *p = cur_line_p;
  if (skip_spaces)
    while(isspace(*p))
      p++;

  return *p;
}




/*-------------------------------------------------------------------------*
 * PL_SYNTAX_ERROR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Syntax_Error(char *format, ...)
{
  va_list arg_ptr;
  char *p = cur_line_str + strlen(cur_line_str) - 1;

  if (*p == '\n')
    *p = '\0';

  fprintf(stderr, "line %d: ", cur_line_no);

  va_start(arg_ptr, format);
  vfprintf(stderr, format, arg_ptr);
  va_end(arg_ptr);

  fprintf(stderr, "\n%s\n", cur_line_str);

  for (p = cur_line_str; p < beg_last_token; p++)
    if (!isspace(*p))
      *p = ' ';

  *p = '\0';
  fprintf(stderr, "%s^ here\n", cur_line_str);

  longjmp(jumper, 1);
}
