/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_parser.c                                                     *
 * Descr.: mini-assembler parser                                           *
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

#define MAX_LINE_LEN               65536
#define MAX_STR_LEN                32768
#define MAX_ARGS                   128
#define MAX_SWITCH_CASES           1000000



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
  LONG,
  LABEL
};




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *inst[] = {
  "pl_code", "pl_jump", "prep_cp", "here_cp", "pl_call", "pl_fail",
  "pl_ret", "jump", "move", "call_c", "jump_ret", "fail_ret", "move_ret",
  "switch_ret", "c_code", "c_ret", "long", NULL }; /* NULL corresponds to a label def (LABEL) */


Bool reload_e;


char fct_name[MAX_STR_LEN];
Bool fc;
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
int cur_approx_inst_line;
char cur_line_str[MAX_LINE_LEN];
char *cur_line_p;
char *beg_last_token;
int nb_appox_inst_line;

char str_val[MAX_STR_LEN];
PlLong int_val;
double dbl_val;




/* About approx_inst_line: the goal is to provide a measure to take into
 * account some limitations of target processors (e.g. on arm, conditional
 * branching have a limit on the offset, see arm64_any.c). As MA is quite 
 * low-level (on average one MA instruction give rise to N asm instruction), 
 * the idea is to count the number of MA instructions. There is normally, 
 * 1 MA inst per line (but counting lines is not a good idea since some line
 * can be blank, some can be split with a final \ and some con only contain 
 * a comment). We thus count MA instructions (including directives, ...).
 * For the switch_ret instruction (which can be very large), we also count 
 * the number of pairs (value,label) since each will give rise to asm inst. 
 * BTW: we could do the same for call_c (adding the number of arguments) but
 * there is in general too much arguments. 
 * This mechanism needs a pre-pass and provides a sufficient approximation 
 * in practice (see usage in ma2asm.c / arm64_any.c).
 */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Parser(int pass_no, int nb_passes);

static int Read_If_Global(Bool initializer_accepted);

static void Read_Function(void);

static void Read_Switch(void);

static int Read_Index(void);

static int Read_Optional_Index(void);

static int Read_Token(int what);

static int Scanner(void);

static void Skip_Rest_Of_Line(void);




/*-------------------------------------------------------------------------*
 * PARSE_MA_FILE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Parse_Ma_File(char *file_name_in, int comment)
{
  int ret_val;
  int i, nb_passes = mi.needs_pre_pass + 1; /* pre-pass is to discover all defined symbols */


  if (file_name_in == NULL)
    {
      file_name_in = "stdin";
      file_in = stdin;
    }
  else if ((file_in = fopen(file_name_in, "rt")) == NULL)
    {
      fprintf(stderr, "cannot open input file %s\n", file_name_in);
      return FALSE;
    }

  for(i = 1; i <= nb_passes; i++)
    {
      if (i == 2)
	{
	  if (fseek(file_in, 0, SEEK_SET) == -1)
	    {
	      fprintf(stderr, "cannot reposition file %s (needed for 2 passes)\n", file_name_in);
	      return FALSE;
	    }
	  Init_Mapper();  /* Allow to adapt parameters wrt pre-pass (e.g. nb_appox_inst_line) */
	}

      keep_source_lines = comment;

      if ((ret_val = setjmp(jumper)) == 0)
	Parser(i, nb_passes);

      if (ret_val != 0)
	return FALSE;
    }

  if (file_in != stdin)
    fclose(file_in);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PARSER                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/

#define Pre_Pass() (pass_no < nb_passes)

#define Stop_Previous_Code()			\
{						\
  if (!Pre_Pass() && inside_code)		\
    {						\
      Code_Stop(&cur_code);			\
      inside_code = FALSE;			\
    }						\
}


static void
Parser(int pass_no, int nb_passes)
{
  Bool inside_code = FALSE;
  Bool initializer_defined = FALSE;
  CodeInf cur_code;
  LongInf l;
  char **in;
  int k, i;

  if (Pre_Pass())
    keep_source_lines = FALSE;

  cur_line_p = cur_line_str;
  cur_line_str[0] = '\0';
  cur_line_no = 0;
  cur_approx_inst_line = 0;
  
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

      k = (int) (in - inst);

      
      cur_approx_inst_line++; /* count 1 for each MA inst (could be more precise, e.g. 0 for label, ...) */
      if (Pre_Pass())
	{			/* special case in in pre-pass */
	  if (k != PL_CODE && k != C_CODE && k != LONG && k != SWITCH_RET && k != LABEL)
	    {
	    ignore_eol:
	      Skip_Rest_Of_Line();
	      continue;
	    }
	}
      else if (pass_no > 1)		/* special case in second pass */
	{
	  if (k == LONG)		/* ignore long decl (treated in pre-pass)  */
	    goto ignore_eol;
	}

      switch (k)
	{
	case PL_CODE:
	  Stop_Previous_Code();
	  cur_code.type = CODE_TYPE_PROLOG;
	  cur_code.global = Read_If_Global(FALSE);
	  Read_Token(IDENTIFIER);
	  cur_code.name = strdup(str_val);
	  cur_code.approx_inst_line = cur_approx_inst_line;
	  if (Pre_Pass())
	    Decl_Code(&cur_code); /* a malloc+copy done by Decl_Code */
	  else
	    {
	      Code_Start(&cur_code);
	      reload_e = TRUE;
	      inside_code = TRUE;
	    }
	  break;

	case C_CODE:
	  Stop_Previous_Code();
	  cur_code.type = CODE_TYPE_C;
	  cur_code.global = Read_If_Global(!initializer_defined);
	  Read_Token(IDENTIFIER);
	  cur_code.name = strdup(str_val);
	  cur_code.approx_inst_line = cur_approx_inst_line;
	  if (cur_code.global == 2) /* initializer ? */
	    {
	      initializer_defined = TRUE;
	      cur_code.type = CODE_TYPE_INITIALIZER;
	      cur_code.global = FALSE;
	      if (!Pre_Pass())
		Declare_Initializer(cur_code.name);
	    }
	  if (Pre_Pass())
	    Decl_Code(&cur_code); /* a malloc+copy done by Decl_Code */
	  else
	    {
	      Code_Start(&cur_code);
	      inside_code = TRUE;
	    }
	  break;

	case PL_JUMP:
	  Read_Token(IDENTIFIER);
	  Pl_Jump(str_val);
	  reload_e = TRUE;
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
	  reload_e = TRUE;
	  break;

	case PL_FAIL:
	  Pl_Fail(TRUE);
	  reload_e = TRUE;
	  break;

	case PL_RET:
	  Pl_Ret();
	  reload_e = TRUE;
	  break;

	case JUMP:
	  Read_Token(IDENTIFIER);
	  Jump(str_val);
	  reload_e = TRUE;
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
	  reload_e = TRUE;
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
	  if (!Pre_Pass())
	    Switch_Ret(nb_swt, swt);
	  cur_approx_inst_line += nb_swt;/* count separately each pair (value=label) */
	  break;

	case C_RET:
	  C_Ret();
	  break;

	case LONG:
	  Stop_Previous_Code();
	  l.global = Read_If_Global(FALSE);
	  Read_Token(IDENTIFIER);
	  l.name = strdup(str_val);
	  if ((i = Read_Optional_Index()) > 0)	/* array */
	    {
	      l.vtype = ARRAY_SIZE;
	      l.value = i;
	    }
	  else
	    {
	      while (isspace(*cur_line_p))
		cur_line_p++;
	      if (*cur_line_p != '=')
		{
		  l.vtype = NONE;
		  l.value = 1; /* value = 1, so it can be considered by mappers as an ARRAY_SIZE with size = 1 */
		}
	      else
		{
		  cur_line_p++;		/* skip the = */
		  Read_Token(INTEGER);
		  l.vtype = INITIAL_VALUE;
		  l.value = int_val;
		}
	    }
	  Decl_Long(&l);	/* a malloc+copy done by Decl_Long */
	  break;

	case LABEL:		/* label: */
	  Read_Token(':');
	  if (Pre_Pass())
	    Decl_Label(str_val, cur_approx_inst_line); /* record label */
	  else
	    Label(str_val);
	  break;

	default:		/* should never occurs */
	  Syntax_Error("Unhandled MA element (pseudo:%s token id: %d)", str_val, k);
	}
    }
  Stop_Previous_Code();		/* in case the last code is not followed by any declaration */
  nb_appox_inst_line = cur_approx_inst_line;
#ifdef DEBUG
  if (pass_no == 1)
    printf("lines %d: approx inst: %d\n", cur_line_no, cur_approx_inst_line);
#endif
}




/*-------------------------------------------------------------------------*
 * READ_IF_GLOBAL                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_If_Global(Bool initializer_accepted)
{
  if (Scanner() != IDENTIFIER)
    goto err;

  if (strcmp(str_val, "local") == 0)
    return 0;

  if (strcmp(str_val, "global") == 0)
    return 1;

  if (initializer_accepted && strcmp(str_val, "initializer") == 0)
    return 2;

err:
  if (!initializer_accepted)
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

  fc = FALSE;
  Read_Token(IDENTIFIER);
  if (strcmp(str_val, "fast") == 0)
    {
      fc = TRUE;
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
      arg[nb_args].adr_of = FALSE;
    one_arg:
      switch (k)
	{
	case '&':
	  k = Scanner();
	  if (k != IDENTIFIER && k != X_REG && k != Y_REG && k != FL_ARRAY && k != FD_ARRAY)
	    Syntax_Error("identifier, X(...), Y(...), FL(...) or FD(...) expected");
	  arg[nb_args].type = k;
	  arg[nb_args].adr_of = TRUE;
	  goto one_arg;

	case STRING:
	  arg[nb_args].str_val = strdup(str_val);
	  break;

	case INTEGER:
	  arg[nb_args].int_val = int_val;
	  break;

	case FLOAT:
#if WORD_SIZE < 64
	  nb_args_in_words++;	/* double count 1 word more for 32 bits machines */
#endif
	  arg[nb_args].str_val = strdup(str_val);
	  arg[nb_args].dbl_val = dbl_val;
	  break;

	case IDENTIFIER:
	  arg[nb_args].type = MEM;
	  arg[nb_args].str_val = strdup(str_val);
	  arg[nb_args].index = Read_Optional_Index();
	  break;

	case FD_ARRAY:
#if WORD_SIZE < 64
	  if (arg[nb_args].adr_of == 0)
	    nb_args_in_words++;	/* double count 1 word more for 32 bits machines */
#endif
	case FL_ARRAY:
	case X_REG:
	case Y_REG:
	  arg[nb_args].index = Read_Index();
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
      if (nb_swt >= MAX_SWITCH_CASES)
	Syntax_Error("Too big switch_ret, max cases: %d", MAX_SWITCH_CASES);
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
  int k = Scanner();		/* accepts (index) or [index] syntax */
  if (k == '(')
    k = ')';
  else if (k == '[')
    k = ']';
  else
    Syntax_Error("( index ) or [ index ] expected");

  Read_Token(INTEGER);
  Read_Token(k);

  return (int) int_val;
}




/*-------------------------------------------------------------------------*
 * READ_OPTIONAL_INDEX                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Optional_Index(void)
{
  return (*cur_line_p == '(' || *cur_line_p == '[') ? Read_Index() : 0;
}




/*-------------------------------------------------------------------------*
 * PL_READ_TOKEN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Token(int what)
{
  int k = Scanner();

  if (k == what || (what == X_REG && k == Y_REG) || (what == FL_ARRAY && k == FD_ARRAY))
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
      Syntax_Error("%c expected", what);
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
  double strtod(const char *nptr, char **endptr); /* why ? */


  for (;;)
    {				/* a \ should be followed by \n (not tested) for a line continuation */
      while (isspace(*cur_line_p) || *cur_line_p == '\\')
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
              Label_Printf("\t%s %6d: %s", mi.comment_prefix, cur_line_no, cur_line_p);
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
      if (mi.strings_need_null)
	{
	  *p++ = '\\';
	  *p++ = '0';
	}
      *p++ = '"';
      *p = '\0';
      return STRING;
    }

  if (isalpha(*cur_line_p) || *cur_line_p == '_' || *cur_line_p == '.')	/* identifier */
    {
      p = str_val;
      if (*cur_line_p == '.') /* local label: replace . by target assembler local prefix */
	{
	  strcpy(p, mi.local_symb_prefix);
	  p += strlen(p);
	  cur_line_p++;
	}

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
	      reload_e = FALSE;
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
  strncpy(str_val, cur_line_p, p1 - cur_line_p);
  str_val[p1 - cur_line_p] = '\0';
  cur_line_p = p1;
  dbl_val = d;
  
  return FLOAT;
}




/*-------------------------------------------------------------------------*
 * SKIP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Skip_Rest_Of_Line(void)
{
  char *p;

  /* useful for pass 1 to ignore instructions (keep decl only) */
  while((p = strrchr(cur_line_p, '\\')) != NULL)
    {
      if (fgets(cur_line_str, sizeof(cur_line_str), file_in)) /* to avoid gcc warning warn_unused_result */
	{
	}

      if (feof(file_in))
	*cur_line_str = '\0';

      cur_line_no++;
      cur_line_p = cur_line_str;
    }

  *cur_line_p = '\0';		/* ignore this line  */
}




/*-------------------------------------------------------------------------*
 * SYNTAX_ERROR                                                            *
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
