/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma2asm.c                                                        *
 * Descr.: code generation                                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "ma_parser.h"
#include "ma_protos.h"

#include "../EnginePl/gp_config.h"
#include "../Wam2Ma/bt_string.c"
#include "../TopComp/copying.c"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DEFAULT_OUTPUT_SUFFIX      ASM_SUFFIX

#define LONG_STACK_SIZE            8192

#define MASK_LONG_GLOBAL           1
#define MASK_LONG_INITIALIZED      2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct inflong *InfLongP;

typedef struct inflong
{
  char *name;
  int global;
  VType vtype;			/* NONE, INITIAL_VALUE, ARRAY_SZIE */
  long value;
  InfLongP next;
}
InfLong;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *file_name_in;
char *file_name_out;
int keep_source_lines;

FILE *file_out;

int work_label = 0;

BTString bt_string;

InfLong dummy_long_start;
InfLong *long_end = &dummy_long_start;
int nb_long;

char *initializer_fct = NULL;



	  /* os_cpu.c variables */

extern int call_c_reverse_args;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Switch_Rec(int start, int stop, SwtInf swt[]);

void Switch_Equal(SwtInf *c);

int Switch_Cmp_Int(SwtInf *c1, SwtInf *c2);

void Label_Printf(char *label, ...);

void Inst_Printf(char *op, char *operands, ...);

void Inst_Out(char *op, char *operands);

void Char_Out(char c);

void String_Out(char *s);

void Int_Out(int d);

void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);



#define Check_Arg(i,str)      (strncmp(argv[i],str,strlen(argv[i]))==0)




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  InfLong *p;
  int n;

  Parse_Arguments(argc, argv);

  if (file_name_out == NULL)
    file_out = stdout;
  else if ((file_out = fopen(file_name_out, "wt")) == NULL)
    {
      fprintf(stderr, "cannot open output file %s\n", file_name_out);
      exit(1);
    }

  BT_String_Init(&bt_string);
  Asm_Start();

  if (!Parse_Ma_File(file_name_in, keep_source_lines))
    {
      fprintf(stderr, "Translation aborted\n");
      exit(1);
    }

  Data_Start(initializer_fct);

  n = bt_string.nb_elem;
  if (n)
    {
      Dico_String_Start(n);

      BT_String_List(&bt_string, Dico_String);

      Dico_String_Stop(n);
    }

  if (nb_long)
    {
      Dico_Long_Start(nb_long);

      for (p = dummy_long_start.next; p; p = p->next)
	Dico_Long(p->name, p->global, p->vtype, p->value);

      Dico_Long_Stop(nb_long);
    }

  Data_Stop(initializer_fct);

  Asm_Stop();

  if (file_out != stdout)
    fclose(file_out);

  exit(0);
}




/*-------------------------------------------------------------------------*
 * DECLARE_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Declare_Initializer(char *init_fct)
{
  initializer_fct = strdup(init_fct);
}




/*-------------------------------------------------------------------------*
 * CALL_C                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C(char *fct_name, int fc, int nb_args, ArgInf arg[])
{
  unsigned i;			/* unsigned is important for the loop */
  int inc;
  int offset = 0;
  int no;

  Call_C_Start(fct_name, fc, nb_args);

  if (!call_c_reverse_args)
    i = 0, inc = 1;
  else
    i = nb_args - 1, inc = -1;

  for (; i < (unsigned) nb_args; i += inc)
    {
      switch (arg[i].type)
	{
	case INTEGER:
	  offset += Call_C_Arg_Int(offset, arg[i].t.int_val);
	  break;

	case FLOAT:
	  offset += Call_C_Arg_Double(offset, arg[i].t.dbl_val);
	  break;

	case STRING:
	  no = BT_String_Add(&bt_string, arg[i].t.str_val)->no;
	  offset += Call_C_Arg_String(offset, no);
	  break;

	case MEM:
	  offset +=
	    Call_C_Arg_Mem_L(offset, arg[i].adr_of, arg[i].t.mem.name,
			     arg[i].t.mem.index);
	  break;

	case X_REG:
	  offset += Call_C_Arg_Reg_X(offset, arg[i].adr_of, arg[i].t.index);
	  break;

	case Y_REG:
	  offset += Call_C_Arg_Reg_Y(offset, arg[i].adr_of, arg[i].t.index);
	  break;

	case FL_ARRAY:
	  offset += Call_C_Arg_Foreign_L(offset, arg[i].adr_of,
					 arg[i].t.index);
	  break;

	case FD_ARRAY:
	  offset += Call_C_Arg_Foreign_D(offset, arg[i].adr_of,
					 arg[i].t.index);
	  break;

	default:		/* for the compiler */
	  ;
	}
    }

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
  char str[32];

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
      sprintf(str, "Lwork%d", work_label++);
      Jump_If_Greater(str);
      Switch_Rec(start, mid - 1, swt);
      Label(str);
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
 * DECL_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Decl_Long(char *name, int global, VType vtype, long value)
{
  InfLong *p;

  nb_long++;

  p = (InfLong *) malloc(sizeof(InfLong));
  if (p == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for long %s\n", name);
      exit(1);
    }


  p->name = name;		/* strdup done by the parser */
  p->global = global;

  p->vtype = vtype;
  p->value = value;
  p->next = NULL;

  long_end->next = p;
  long_end = p;
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
  keep_source_lines = 0;

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "-o") || Check_Arg(i, "--output"))
	    {
	      if (++i >= argc)
		{
		  fprintf(stderr, "FILE missing after %s option\n",
			  argv[i - 1]);
		  exit(1);
		}

	      file_name_out = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--comment"))
	    {
	      keep_source_lines = 1;
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

	  fprintf(stderr, "unknown option %s - try ma2asm --help\n",
		  argv[i]);
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
#define L(msg)  fprintf(stderr,"%s\n",msg)
{
  L("Usage: ma2asm [option...] file");
  L("");
  L("Options:");
  L("  -o FILE, --output FILE      set output file name");
  L("  --comment                   include comments in the output file");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("");
  L("'-' can be given as <file> for the standard input/output");
  L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
