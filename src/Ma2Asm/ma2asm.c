/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma2asm.c                                                        *
 * Descr.: code generation                                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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

#include "../EnginePl/gp_config.h"
#include "../Wam2Ma/bt_string.c"
#include "../TopComp/copying.c"

#define MA2ASM_FILE

#include "ma_parser.h"
#include "ma_protos.h"


#if 0
#define DEBUG
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DEFAULT_OUTPUT_SUFFIX      ASM_SUFFIX




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *file_name_in;
char *file_name_out;

Bool comment;
Bool pic_code;
Bool ignore_fc;

MapperInf mi;

LabelGen lg_cont; /* local label generator for continuations (always available) see macros Label_Cont_XXX() */

FILE *file_out;

BTString bt_code;		/* only filled if pre-pass is activated */
BTString bt_long;
BTString bt_string;
BTString bt_double;		/* only filled used if needed */

char *initializer_fct = NULL;

char local_label[64];
int local_label_count = 0;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Invoke_Dico_String(int no, char *str, void *info);

void Invoke_Dico_Double(int no, char *str, void *info);

void Invoke_Dico_Long(int no, char *str, void *info);

StringInf *Record_String(char *str);

DoubleInf *Record_Double(char *ma_str, double dbl_val);

void Switch_Rec(int start, int stop, SwtInf swt[]);

void Switch_Equal(SwtInf *c);

int Switch_Cmp_Int(SwtInf *c1, SwtInf *c2);

void Inst_Out(char *op, char *operands);

void Char_Out(char c);

void String_Out(char *s);

void Int_Out(int d);

void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);



#define Check_Arg(i, str)      (strncmp(argv[i], str, strlen(argv[i])) == 0)




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  int n;

  /* some default values */
  mi.can_produce_pic_code = FALSE;
  mi.needs_pre_pass = FALSE;
  mi.comment_prefix = "#";
  mi.local_symb_prefix = "L";
  mi.string_symb_prefix = "LC";
  mi.double_symb_prefix = "LD";
  mi.strings_need_null = FALSE;
  mi.call_c_reverse_args = FALSE;

  Init_Mapper();

  Parse_Arguments(argc, argv);

  if (file_name_out == NULL)
    file_out = stdout;
  else if ((file_out = fopen(file_name_out, "wt")) == NULL)
    {
      fprintf(stderr, "cannot open output file %s\n", file_name_out);
      exit(1);
    }

  BT_String_Init(&bt_code); 		/* only filled if pre_pass is activated */
  BT_String_Init(&bt_long);
  BT_String_Init(&bt_string);
  BT_String_Init(&bt_double);		/* only filled if dico double is needed */

  Label_Gen_Init(&lg_cont, "cont");	/* available for any mapper */
  
  Asm_Start();

  if (!Parse_Ma_File(file_name_in, comment))
    {
      fprintf(stderr, "Translation aborted\n");
      exit(1);
    }

  Data_Start(initializer_fct);

  n = bt_string.nb_elem;
  if (n)
    {
      Dico_String_Start(n);
      BT_String_List(&bt_string, Invoke_Dico_String);
      Dico_String_Stop(n);
    }

  n = bt_double.nb_elem;
  if (n)
    {
      Dico_Double_Start(n);
      BT_String_List(&bt_double, Invoke_Dico_Double);
      Dico_Double_Stop(n);
    }

  n = bt_long.nb_elem;
  if (n)
    {
      Dico_Long_Start(n);
      BT_String_List(&bt_long, Invoke_Dico_Long);
      Dico_Long_Stop(n);
    }

  Data_Stop(initializer_fct);

  Asm_Stop();

  if (file_out != stdout)
    fclose(file_out);

  exit(0);
}




/*-------------------------------------------------------------------------*
 * INVOKE_DICO_STRING                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Invoke_Dico_String(int no, char *str, void *info)
{
  StringInf *d = (StringInf *) info;
  Dico_String(d);
}




/*-------------------------------------------------------------------------*
 * INVOKE_DICO_DOUBLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Invoke_Dico_Double(int no, char *str, void *info)
{
  DoubleInf *d = (DoubleInf *) info;

  if (comment)
    Inst_Printf("", "%s %s", mi.comment_prefix, d->cmt_str);
      
  Dico_Double(d);
}




/*-------------------------------------------------------------------------*
 * INVOKE_DICO_LONG                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Invoke_Dico_Long(int no, char *name, void *info)
{
  LongInf *l = (LongInf *) info;
  Dico_Long(l);
}




/*-------------------------------------------------------------------------*
 * RECORD_STRING                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
StringInf *
Record_String(char *str)
{
  char label[32];
  BTNode *b = BT_String_Add(&bt_string, str);
  StringInf *s = (StringInf *) &b->info;
  s->no = b->no;
  s->str = str;
  sprintf(label, "%s%d", mi.string_symb_prefix, s->no);
  s->symb = strdup(label);
  return s;
}



/*-------------------------------------------------------------------------*
 * RECORD_DOUBLE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
DoubleInf *
Record_Double(char *ma_str, double dbl_val)
{
  char label[32];
  char *p;
  static char cmt[64];
  BTNode *b = BT_String_Add(&bt_double, ma_str);
  DoubleInf *d = (DoubleInf *) &b->info;
  d->no = b->no;
  d->ma_str = ma_str;
  sprintf(label, "%s%d", mi.double_symb_prefix, d->no);
  d->symb = strdup(label);
  d->v.dbl = dbl_val;

  /* check if the double read in MA file is given in a human-readable form */
  p = ma_str;
  while(*p && strchr("0123456789.-eE", *p))
    p++;

  d->is_ma_str_human = (*p == '\0');

  if (d->is_ma_str_human)
    d->cmt_str = d->ma_str;
  else
    {
      sprintf(cmt, "%s = %1.17g", ma_str, dbl_val);
      d->cmt_str = strdup(cmt);
    }

  return d;
}




/*-------------------------------------------------------------------------*
 * DECLARE_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Declare_Initializer(char *init_fct)
{				/* init_fct: strdup done by the parser */
  initializer_fct = init_fct;
}




/*-------------------------------------------------------------------------*
 * DECL_CODE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void				/* called if pre_pass */
Decl_Code(CodeInf *c)
{
  CodeInf *c1;
  /* needs to create a copy of c (malloc)
   * actually donne by BT_String_Add 
   */

		/* name: strdup done by the parser */
  c1 = (CodeInf *) &BT_String_Add(&bt_code, c->name)->info;
  *c1 = *c;
}




/*-------------------------------------------------------------------------*
 * DECL_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Decl_Long(LongInf *l)
{
  LongInf *l1;
  /* needs to create a copy of c (malloc)
   * actually donne by BT_String_Add 
   */

		/* name: strdup done by the parser */
  l1 = (LongInf *) &BT_String_Add(&bt_long, l->name)->info;
  *l1 = *l;
}




/*-------------------------------------------------------------------------*
 * IS_CODE_DEFINED                                                         *
 *                                                                         *
 * Needs a pre-pass.                                                       *
 *-------------------------------------------------------------------------*/
int
Is_Code_Defined(char *name)
{
#ifdef DEBUG
  if (!mi.needs_pre_pass)
    printf("WARNING: %s:%d needs a pre-pass\n",  __FILE__, __LINE__);
#endif

  return (BT_String_Lookup(&bt_code, name) != NULL);
}




/*-------------------------------------------------------------------------*
 * GET_CODE_INFOS                                                          *
 *                                                                         *
 * Needs a pre-pass.                                                       *
 *-------------------------------------------------------------------------*/
CodeInf *
Get_Code_Infos(char *name)
{
  BTNode *b = BT_String_Lookup(&bt_code, name);

#ifdef DEBUG
  if (!mi.needs_pre_pass)
    printf("WARNING: %s:%d needs a pre-pass\n",  __FILE__, __LINE__);
#endif

  return (b == NULL) ? NULL : (CodeInf *) &b->info;
}




/*-------------------------------------------------------------------------*
 * GET_LONG_INFOS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
LongInf *
Get_Long_Infos(char *name)
{
  BTNode *b = BT_String_Lookup(&bt_long, name);

  return (b == NULL) ? NULL : (LongInf *) &b->info;
}




/*-------------------------------------------------------------------------*
 * SCOPE_OF_SYMBOL                                                         *
 *                                                                         *
 * Need a pre-pass.                                                        *
 * Returns 0: global symbol, 1: local code, 2: other local symbol (long,..)*
 *-------------------------------------------------------------------------*/

/* A symbol not defined (not encountered neither as code nor long) can be:
 * - foreign_long, foreign_double (thus external)
 * - an external predicate or C function (e.g. function associated to a WAM 
 *   instruction like Get_Integer() or Pl_Un_Term() which is generated when 
 *   compiling foreign directive).
 * - a local label (for branching) or local symbol (strings, double constants)
 * Only the last case is local and we know it begins with . or L
 */
int
Scope_Of_Symbol(char *name)
{
  CodeInf *c;
  LongInf *l;

#ifdef DEBUG
  if (!mi.needs_pre_pass)
    printf("WARNING: %s:%d needs a pre-pass\n",  __FILE__, __LINE__);
#endif

  /* test local symbols first since they are not recoded in bt_code/bt_long */

  if (*name == '.' || strncmp(name, mi.local_symb_prefix, strlen(mi.local_symb_prefix)) == 0)
    return 2;

  c = Get_Code_Infos(name);
  if (c)
    return (c->global) ? 0 : 1;

  l = Get_Long_Infos(name);
  if (l)
    return (l->global) ? 0 : 2;

  return 0;			/* not defined, considered as global (code/data) */
}




/*-------------------------------------------------------------------------*
 * CALL_C                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C(char *fct_name, Bool fc, int nb_args, int nb_args_in_words, ArgInf arg[])
{
  unsigned i;			/* unsigned is important for the loop */
  int inc;
  int offset = 0;
  StringInf *s;
  DoubleInf *d;

  if (ignore_fc)
    fc = FALSE;

  Call_C_Start(fct_name, fc, nb_args, nb_args_in_words);

  if (!mi.call_c_reverse_args)
    i = 0, inc = 1;
  else
    i = nb_args - 1, inc = -1;

  for (; i < (unsigned) nb_args; i += inc)
    {
      switch (arg[i].type)
	{
	case INTEGER:
	  offset += Call_C_Arg_Int(offset, arg[i].int_val);
	  break;

	case FLOAT:		/* strdup done by the parser */
	  d = Record_Double(arg[i].str_val, arg[i].dbl_val);
	  if (comment)
	    Inst_Printf("", "%s %s", mi.comment_prefix, d->cmt_str);
	  offset += Call_C_Arg_Double(offset, d);
	  break;

	case STRING:		/* strdup done by the parser */
	  s = Record_String(arg[i].str_val);
	  if (comment)
	    Inst_Printf("", "%s %s", mi.comment_prefix, s->str);
	  offset += Call_C_Arg_String(offset, s);
	  break;

	case MEM:
	  offset += Call_C_Arg_Mem_L(offset, arg[i].adr_of, arg[i].str_val, arg[i].index);
	  break;

	case X_REG:
	  offset += Call_C_Arg_Reg_X(offset, arg[i].adr_of, arg[i].index);
	  break;

	case Y_REG:
	  offset += Call_C_Arg_Reg_Y(offset, arg[i].adr_of, arg[i].index);
	  break;

	case FL_ARRAY:
	  offset += Call_C_Arg_Foreign_L(offset, arg[i].adr_of, arg[i].index);
	  break;

	case FD_ARRAY:
	  offset += Call_C_Arg_Foreign_D(offset, arg[i].adr_of, arg[i].index);
	  break;

	default:		/* for the compiler */
	  ;
	}
    }

  Call_C_Invoke(fct_name, fc, nb_args, nb_args_in_words);

  Call_C_Stop(fct_name, nb_args);
}




/*-------------------------------------------------------------------------*
 * SWITCH_RET                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Switch_Ret(int nb_swt, SwtInf swt[])
{
  qsort((void *) swt, nb_swt, sizeof(SwtInf),
	(int (*)(const void *, const void *)) Switch_Cmp_Int);

  Switch_Rec(0, nb_swt - 1, swt);
}




/*-------------------------------------------------------------------------*
 * SWITCH_REC                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Switch_Rec(int start, int stop, SwtInf swt[])
{
  int mid;
  char lab_cont[32];

  switch (stop - start + 1)	/* nb elements */
    {
    case 1:
      Switch_Equal(swt + start);
      Pl_Fail();
      break;

    case 2:
      Switch_Equal(swt + start);
      Switch_Equal(swt + stop);
      Pl_Fail();
      break;

    case 3:
      Switch_Equal(swt + start);
      Switch_Equal(swt + start + 1);
      Switch_Equal(swt + stop);
      Pl_Fail();
      break;

    default:
      mid = (start + stop) / 2;
      Switch_Equal(swt + mid);
      strcpy(lab_cont, Label_Cont_New());
      Jump_If_Greater(lab_cont);
      Switch_Rec(start, mid - 1, swt);
      Label(lab_cont);
      Switch_Rec(mid + 1, stop, swt);
    }
}




/*-------------------------------------------------------------------------*
 * SWITCH_EQUAL                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Switch_Equal(SwtInf *c)
{
  Cmp_Ret_And_Int(c->int_val);
  Jump_If_Equal(c->label);
}




/*-------------------------------------------------------------------------*
 * SWITCH_CMP_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Switch_Cmp_Int(SwtInf *c1, SwtInf *c2)
{
  return c1->int_val - c2->int_val;
}




/*-------------------------------------------------------------------------*
 * LABEL_GEN_INIT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label_Gen_Init(LabelGen *g, char *prefix)
{
  g->prefix = prefix;
  g->no = 0;
  g->label[0] = '\0';
}




/*-------------------------------------------------------------------------*
 * LABEL_GEN_NEW                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Label_Gen_New(LabelGen *g)
{
  sprintf(g->label, "%s%s%d", mi.local_symb_prefix, g->prefix, ++g->no);
  return g->label;
}




/*-------------------------------------------------------------------------*
 * LABEL_GEN_GET                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Label_Gen_Get(LabelGen *g)
{
  return g->label;
}




/*-------------------------------------------------------------------------*
 * LABEL_GEN_NO                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Label_Gen_No(LabelGen *g)
{
  return g->no;
}




/*-------------------------------------------------------------------------*
 * LABEL_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label_Printf(char *label, ...)
{
  va_list arg_ptr;


  va_start(arg_ptr, label);

  vfprintf(file_out, label, arg_ptr);

  va_end(arg_ptr);
  fputc('\n', file_out);
}




/*-------------------------------------------------------------------------*
 * INST_PRINTF                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Inst_Printf(char *op, char *operands, ...)
{
  va_list arg_ptr;

  va_start(arg_ptr, operands);

  fprintf(file_out, "\t%s\t", op);
  vfprintf(file_out, operands, arg_ptr);

  va_end(arg_ptr);
  fputc('\n', file_out);
}




/*-------------------------------------------------------------------------*
 * INST_OUT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Inst_Out(char *op, char *operands)
{
  fprintf(file_out, "\t%s\t%s\n", op, operands);
}




/*-------------------------------------------------------------------------*
 * CHAR_OUT                                                                *
 *                                                                         *
 * Only needed by mappers inlining assembly code.                          *
 *-------------------------------------------------------------------------*/
void
Char_Out(char c)
{
  fprintf(file_out, "%c", c);
}




/*-------------------------------------------------------------------------*
 * STRING_OUT                                                              *
 *                                                                         *
 * Only needed by mappers inlining assembly code.                          *
 *-------------------------------------------------------------------------*/
void
String_Out(char *s)
{
  fprintf(file_out, "%s", s);
}




/*-------------------------------------------------------------------------*
 * INT_OUT                                                                 *
 *                                                                         *
 * Only needed by mappers inlining assembly code.                          *
 *-------------------------------------------------------------------------*/
void
Int_Out(int d)
{
  fprintf(file_out, "%d", d);
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGUMENTS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Parse_Arguments(int argc, char *argv[])
{
  static char str[1024];
  int i;


  file_name_in = file_name_out = NULL;
  comment = FALSE;
  pic_code = FALSE;
  ignore_fc = FALSE;

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "-o") || Check_Arg(i, "--output"))
	    {
	      if (++i >= argc)
		{
		  fprintf(stderr, "FILE missing after %s option\n", argv[i - 1]);
		  exit(1);
		}

	      file_name_out = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--pic") || Check_Arg(i, "-fPIC"))
	    {
	      if (mi.can_produce_pic_code)
		pic_code = TRUE;
	      else
		fprintf(stderr, "ignored option %s - cannot produce PIC code for this architecture\n", argv[i]);
	      continue;
	    }

	  if (Check_Arg(i, "--ignore-fast"))
	    {
	      ignore_fc = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--comment"))
	    {
	      comment = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      Display_Copying("Mini-Assembly to Assembly Compiler");
	      exit(0);
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }

	  fprintf(stderr, "unknown option %s - try ma2asm --help\n", argv[i]);
	  exit(1);
	}

      if (file_name_in != NULL)
	{
	  fprintf(stderr, "input file already specified (%s)\n",
		  file_name_in);
	  exit(1);
	}
      file_name_in = argv[i];
    }

  if (file_name_in != NULL && strcmp(file_name_in, "-") == 0)
    file_name_in = NULL;

  if (file_name_out == NULL && file_name_in != NULL)
    {
      strcpy(str, file_name_in);
      i = strlen(str);
      if (strcmp(str + i - 3, ".ma") == 0)
	strcpy(str + i - 3, DEFAULT_OUTPUT_SUFFIX);
      else
	strcpy(str + i, DEFAULT_OUTPUT_SUFFIX);
      file_name_out = str;
    }

  if (file_name_out != NULL && strcmp(file_name_out, "-") == 0)
    file_name_out = NULL;
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Help(void)
#define L(msg)  fprintf(stderr, "%s\n", msg)
{
  L("Usage: ma2asm [option...] file");
  L("");
  L("Options:");
  L("  -o FILE, --output FILE      set output file name");
  L("  --pic                       produce position independent code (PIC)");
  L("  --ignore-fast               ignore fast call (FC) declarations");
  L("  --comment                   include comments in the output file");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("");
  L("'-' can be given as <file> for the standard input/output");
  L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
