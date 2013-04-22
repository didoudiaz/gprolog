/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : mips_irix.c                                                     *
 * Descr.: translation file for IRIX on MIPS                               *
 * Author: Alexander Diemand, Daniel Diaz                                  *
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


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"

#define MAX_C_ARGS_IN_C_CODE       32

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[20];
char asm_reg_e[20];
char asm_reg_b[20];
char asm_reg_cp[20];

int w_label = 0;

static char dbl_arg_buffer[8192] = "\0";	/* a temp buffer for the double arguments */

char act_routine[512] = "\0";	/* remembers the actual routine we are building */

int inPrologCode = 0;	/* whether we are currently compiling a prolog code */

	  /* variables for ma_parser.c / ma2asm.c */

int can_produce_pic_code = 0;
char *comment_prefix = "#";
char *local_symb_prefix = ".L";
int strings_need_null = 1;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


/*-------------------------------------------------------------------------*
 * INLINED CODE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* all %s will be replaced with the function's name
 * all %d will be replaced with the current nb_inlines
 */
static long nb_inlines = 0;
static char *def_inlines[] = {
  /* name            code */
  0, 0				/* end of list */
};



/*-------------------------------------------------------------------------*
 * MAKE_INLINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* when it finds a function to inline it will do so immediatly and return 1
 * else it fails and returns 0
 */
static int
make_inline(char *fct_name, int nb_args)
{
  char *fp;
  int counter;

  return 0;			/* not yet */

  /* user can set an environment variable to control this */
  if (!getenv("GPROLOG_ASM_INLINE"))
    return 0;

  counter = 0;
  while (def_inlines[counter])
    {
      if (strcmp(fct_name, def_inlines[counter]) == 0)
	{
	  /* found code to inline, emit */
	  fp = def_inlines[++counter];
	  while (*fp != '\0')
	    {
	      if (*fp == '%' && *(fp + 1) == 's')
		{
		  String_Out(fct_name);
		  fp++;
		}
	      else if (*fp == '%' && *(fp + 1) == 'd')
		{
		  Int_Out(nb_inlines);
		  fp++;
		}
	      else
		{
		  Char_Out(*fp);
		}
	      fp++;
	    }
	  nb_inlines++;
	  return 1;
	}
      counter++;
    }
  return 0;
}




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef MAP_REG_BANK
  sprintf(asm_reg_bank, "%s", MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "$16");
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%s", MAP_REG_E);
#else
/* strcpy(asm_reg_e,"$21"); */
  sprintf(asm_reg_e, "%d(%s)", MAP_OFFSET_E, asm_reg_bank);
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%s", MAP_REG_B);
#else
/* sprintf(asm_reg_b,"$18"); */
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%s", MAP_REG_CP);
#else
/* sprintf(asm_reg_cp,"$20"); */
  sprintf(asm_reg_cp, "%d(%s)", MAP_OFFSET_CP, asm_reg_bank);
#endif

  Inst_Printf(".option", "pic2");	/* gcc uses this */
  Inst_Printf("#.set", "noat");
  Inst_Printf("#.set", "noreorder");	/* let the assembler reorder instructions */

  Inst_Printf("# asm_reg_bank ", asm_reg_bank);
  Inst_Printf("# asm_reg_e ", asm_reg_e);
  Inst_Printf("# asm_reg_b ", asm_reg_b);
  Inst_Printf("# asm_reg_cp ", asm_reg_cp);

  Label_Printf("\t.section\t.text");

}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
  /* we are printing the fixed doubles at the end of the file,
   * they will appear in the data section */
  if (dbl_arg_buffer[0] != '\0')
    {
      Label_Printf(".section\t.rodata");
      Label_Printf(dbl_arg_buffer);
      dbl_arg_buffer[0] = '\0';
    }
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{

  if (act_routine[0] != '\0')
    Code_Stop();		/* we first have to close the previous code */

  Inst_Printf(".text", "");
  Inst_Printf(".align", "2");
  Inst_Printf(".ent", "%s", label);
  if (global)
    Inst_Printf(".globl", "%s", label);

  Label(label);

  /* remember this label */
  strcpy(act_routine, label);

  if (prolog)
    {
      /* prolog code does not need any stack space */
      inPrologCode = 1;
      Inst_Printf(".frame", "$sp,0,$31");
      Inst_Printf(".mask", "0x00000000,0");
      Inst_Printf(".fmask", "0x00000000,0");
    }
  else
    {
      /* for c code we need to save some registers */
      inPrologCode = 0;
      /* */
      Inst_Printf(".frame", "$sp,%d,$31", MAX_C_ARGS_IN_C_CODE * 8 + 16);
      Inst_Printf(".mask", "0x10000000,-16");
      Inst_Printf(".fmask", "0x00000000,0");
      Inst_Printf("subu", "$sp,$sp,%d", MAX_C_ARGS_IN_C_CODE * 8 + 16);
      Inst_Printf("sd", "$gp,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 8);
      Inst_Printf("sd", "$31,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 0);
    }
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(void)
{
  Inst_Printf(".end", "%s", act_routine);

  act_routine[0] = '\0';
}




/*-------------------------------------------------------------------------*
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("\n%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("la", "$25,%s", label);
  Inst_Printf("j", "$25");
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("la", "%s,.Lcont%d", asm_reg_cp, w_label);	/* CP = .Lcont%d */
#else
  Inst_Printf("la", "$13,.Lcont%d", w_label);
  Inst_Printf("sw", "$13,%s", asm_reg_cp);	/* CP = .Lcont%d */
#endif
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf(".Lcont%d:", w_label++);
}




/*-------------------------------------------------------------------------*
 * PL_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call(char *label)
{
  Prep_CP();
  Pl_Jump(label);
  Here_CP();
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Inst_Printf("lw", "$25,-4(%s)", asm_reg_b);
#else
  Inst_Printf("lw", "$13,%s", asm_reg_b);
  Inst_Printf("lw", "$25,-4($13)");
#endif
  Inst_Printf("j", "$25");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
  Inst_Printf(".align", "3");
  Inst_Printf("# nop", "");	/* I don't really know why, but it helps ;-) */
#ifdef MAP_REG_CP
  Inst_Printf("move", "$25,%s", asm_reg_cp);
#else
  Inst_Printf("lw", "$25,%s", asm_reg_cp);
#endif
  Inst_Printf("j", "$25");
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("la", "$25,%s", label);
  Inst_Printf("j", "$25");
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("lw", "$24,%d(%s)", 4 * index, asm_reg_bank);	/* asm_reg_bank */
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("lw", "$24,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("lw", "$13,%s", asm_reg_e);
  Inst_Printf("lw", "$24,%d($13)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("sw", "$24,%d(%s)", 4 * index, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("sw", "$24,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("lw", "$13,%s", asm_reg_e);
  Inst_Printf("sw", "$24,%d($13)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
	     char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("li", "$4,%d", int_val);
      break;
    case 1:
      Inst_Printf("li", "$5,%d", int_val);
      break;
    case 2:
      Inst_Printf("li", "$6,%d", int_val);
      break;
    case 3:
      Inst_Printf("li", "$7,%d", int_val);
      break;
    case 4:
      Inst_Printf("li", "$8,%d", int_val);
      break;
    case 5:
      Inst_Printf("li", "$9,%d", int_val);
      break;
    case 6:
      Inst_Printf("li", "$10,%d", int_val);
      break;
    case 7:
      Inst_Printf("li", "$11,%d", int_val);
      break;
    default:
      Inst_Printf("li", "$24,%d", int_val);
      Inst_Printf("sw", "$24,%d($sp)", (offset - 8) * 8 + 4);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, double dbl_val)
{
  char buf[1024];

  sprintf(buf, "\t.align 3\n.LD%d:\n\t.double %1.20e\n", w_label++,
	  dbl_val);
  strcat(dbl_arg_buffer, buf);
  Inst_Printf("la", "$24,.LD%d", (w_label - 1));
  switch (offset)
    {
    case 0:
      Inst_Printf("l.d", "$f12,($24)");
      break;
    case 1:
      Inst_Printf("l.d", "$f13,($24)");
      break;
    case 2:
      Inst_Printf("l.d", "$f14,($24)");
      break;
    case 3:
      Inst_Printf("l.d", "$f15,($24)");
      break;
    case 4:
      Inst_Printf("l.d", "$f16,($24)");
      break;
    case 5:
      Inst_Printf("l.d", "$f17,($24)");
      break;
    case 6:
      Inst_Printf("l.d", "$f18,($24)");
      break;
    case 7:
      Inst_Printf("l.d", "$f19,($24)");
      break;
    default:
      Inst_Printf("l.d", "$f1,($24)");
      Inst_Printf("s.d", "$f1,%d($sp)", (offset - 8) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("la", "$4,%s%d", STRING_PREFIX, str_no);
      break;
    case 1:
      Inst_Printf("la", "$5,%s%d", STRING_PREFIX, str_no);
      break;
    case 2:
      Inst_Printf("la", "$6,%s%d", STRING_PREFIX, str_no);
      break;
    case 3:
      Inst_Printf("la", "$7,%s%d", STRING_PREFIX, str_no);
      break;
    case 4:
      Inst_Printf("la", "$8,%s%d", STRING_PREFIX, str_no);
      break;
    case 5:
      Inst_Printf("la", "$9,%s%d", STRING_PREFIX, str_no);
      break;
    case 6:
      Inst_Printf("la", "$10,%s%d", STRING_PREFIX, str_no);
      break;
    case 7:
      Inst_Printf("la", "$11,%s%d", STRING_PREFIX, str_no);
      break;
    default:
      Inst_Printf("la", "$24,%s%d", STRING_PREFIX, str_no);
      Inst_Printf("sw", "$24,%d($sp)", (offset - 8) * 8 + 4);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$4");
      break;
    case 1:
      sprintf(dest, "%s", "$5");
      break;
    case 2:
      sprintf(dest, "%s", "$6");
      break;
    case 3:
      sprintf(dest, "%s", "$7");
      break;
    case 4:
      sprintf(dest, "%s", "$8");
      break;
    case 5:
      sprintf(dest, "%s", "$9");
      break;
    case 6:
      sprintf(dest, "%s", "$10");
      break;
    case 7:
      sprintf(dest, "%s", "$11");
      break;
    default:
      sprintf(dest, "%s", "$24");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("la", "$25,%s", name);
      Inst_Printf("lw", "%s,%d($25)", dest, index * 4);
    }
  else
    {
      Inst_Printf("la", "%s,%s+%d", dest, name, index * 4);
    }
  if (offset > 7)
    {
      Inst_Printf("sw", "%s,%d($sp)", dest, (offset - 8) * 8 + 4);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$4");
      break;
    case 1:
      sprintf(dest, "%s", "$5");
      break;
    case 2:
      sprintf(dest, "%s", "$6");
      break;
    case 3:
      sprintf(dest, "%s", "$7");
      break;
    case 4:
      sprintf(dest, "%s", "$8");
      break;
    case 5:
      sprintf(dest, "%s", "$9");
      break;
    case 6:
      sprintf(dest, "%s", "$10");
      break;
    case 7:
      sprintf(dest, "%s", "$11");
      break;
    default:
      sprintf(dest, "%s", "$24");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("lw", "%s,%d(%s)", dest, index * 4, asm_reg_bank);
    }
  else
    {
      if (index == 0)
	{
	  Inst_Printf("move", "%s,%s", dest, asm_reg_bank);
	}
      else
	{
	  Inst_Printf("la", "%s,%d(%s)", dest, index * 4, asm_reg_bank);
	}
    }
  if (offset > 7)
    {
      Inst_Printf("sw", "%s,%d($sp)", dest, (offset - 8) * 8 + 4);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$4");
      break;
    case 1:
      sprintf(dest, "%s", "$5");
      break;
    case 2:
      sprintf(dest, "%s", "$6");
      break;
    case 3:
      sprintf(dest, "%s", "$7");
      break;
    case 4:
      sprintf(dest, "%s", "$8");
      break;
    case 5:
      sprintf(dest, "%s", "$9");
      break;
    case 6:
      sprintf(dest, "%s", "$10");
      break;
    case 7:
      sprintf(dest, "%s", "$11");
      break;
    default:
      sprintf(dest, "%s", "$24");
      break;
    }

  if (!adr_of)
    {
#ifdef MAP_REG_E
      Inst_Printf("lw", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("lw", "$12,%s", asm_reg_e);
      Inst_Printf("lw", "%s,%d($12)", dest, Y_OFFSET(index));
#endif
    }
  else
    {
#ifdef MAP_REG_E
      Inst_Printf("la", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("lw", "$12,%s", asm_reg_e);
      Inst_Printf("la", "%s,%d($12)", dest, Y_OFFSET(index));
#endif
    }
  if (offset > 7)
    {
      Inst_Printf("sw", "%s,%d($sp)", dest, (offset - 8) * 8 + 4);
    }

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$4");
      break;
    case 1:
      sprintf(dest, "%s", "$5");
      break;
    case 2:
      sprintf(dest, "%s", "$6");
      break;
    case 3:
      sprintf(dest, "%s", "$7");
      break;
    case 4:
      sprintf(dest, "%s", "$8");
      break;
    case 5:
      sprintf(dest, "%s", "$9");
      break;
    case 6:
      sprintf(dest, "%s", "$10");
      break;
    case 7:
      sprintf(dest, "%s", "$11");
      break;
    default:
      sprintf(dest, "%s", "$24");
      break;
    }

  Inst_Printf("la", "$2,pl_foreign_long");
  if (!adr_of)
    {
      Inst_Printf("lw", "%s,%d($2)", dest, index * 4);
    }
  else
    {
      Inst_Printf("la", "%s,%d($2)", dest, index * 4);
    }
  if (offset > 7)
    {
      Inst_Printf("sw", "%s,%d($sp)", dest, (offset - 8) * 8 + 4);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  char dest[8];

  if (adr_of)
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$4");
	  break;
	case 1:
	  sprintf(dest, "%s", "$5");
	  break;
	case 2:
	  sprintf(dest, "%s", "$6");
	  break;
	case 3:
	  sprintf(dest, "%s", "$7");
	  break;
	case 4:
	  sprintf(dest, "%s", "$8");
	  break;
	case 5:
	  sprintf(dest, "%s", "$9");
	  break;
	case 6:
	  sprintf(dest, "%s", "$10");
	  break;
	case 7:
	  sprintf(dest, "%s", "$11");
	  break;
	default:
	  sprintf(dest, "%s", "$24");
	  break;
	}
      Inst_Printf("la", "%s,pl_foreign_double", dest);
      Inst_Printf("addu", "%s,%s,%d", dest, dest, index * 8);
      if (offset > 7)
	{
	  Inst_Printf("sw", "%s,%d($sp)", dest, (offset - 8) * 8);
	}
      return 1;
    }
  else
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$f12");
	  break;
	case 1:
	  sprintf(dest, "%s", "$f13");
	  break;
	case 2:
	  sprintf(dest, "%s", "$f14");
	  break;
	case 3:
	  sprintf(dest, "%s", "$f15");
	  break;
	case 4:
	  sprintf(dest, "%s", "$f16");
	  break;
	case 5:
	  sprintf(dest, "%s", "$f17");
	  break;
	case 6:
	  sprintf(dest, "%s", "$f18");
	  break;
	case 7:
	  sprintf(dest, "%s", "$f19");
	  break;
	default:
	  sprintf(dest, "%s", "$f1");
	  break;
	}
      Inst_Printf("la", "$25,pl_foreign_double");
      Inst_Printf("l.d", "%s,%d($25)", dest, index * 8);
      if (offset > 7)
	{
	  Inst_Printf("s.d", "%s,%d($sp)", dest, (offset - 8) * 8);
	}
      return 1;
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int fc, int nb_args, int nb_args_in_words)
{
/*  if (!make_inline (fct_name, nb_args)) { */
  Inst_Printf("sd", "$gp,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 8);
  Inst_Printf("sd", "$31,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8);
  Inst_Printf("la", "$25,%s", fct_name);
  Inst_Printf("jal", "$25");
  Inst_Printf("ld", "$gp,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 8);
  Inst_Printf("ld", "$31,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8);
/*  } */
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args, char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * JUMP_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_Ret(void)
{
  Inst_Printf("move", "$25,$2");
  Inst_Printf("j", "$25");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("bne", "$2,$0,.Lcont%d", w_label);
  Pl_Fail();
  Label_Printf(".Lcont%d:", w_label++);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("la", "$13,%s", name);
  Inst_Printf("sw", "$2,%d($13)", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("sw", "$2,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
#ifdef MAP_REG_E
  Inst_Printf("sw", "$2,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("lw", "$13,%s", asm_reg_e);
  Inst_Printf("sw", "$2,%d($13)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("la", "$13,pl_foreign_long");
  Inst_Printf("sw", "$2,%d($13)", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("la", "$13,pl_foreign_double");
  Inst_Printf("s.d", "$f0,%d($13)", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  Inst_Printf("li", "$24,%d", int_val);
  Inst_Printf("sub", "$12,$2,$24");	/* $2 - $24 -> $12 */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beqz", "$12,%s", label);	/* $2 == 0 */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  /* this is based on the comparison we did with Cmp_Ret_And_Int */
  /* means this is more or less a Jump_If_Not_Equal ! */
  Inst_Printf("bgtz", "$12,%s", label);	/* $3 == 1 */
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ld", "$gp,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 8);
  Inst_Printf("ld", "$31,%d($sp)", MAX_C_ARGS_IN_C_CODE * 8 + 0);
  Inst_Printf("addiu", "$sp,$sp,%d", MAX_C_ARGS_IN_C_CODE * 8 + 16);
  Inst_Printf("j", "$31");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
  Label_Printf(".section\t.rodata");
  Inst_Printf(".align", "3");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{

  Label_Printf("%s%d:", STRING_PREFIX, str_no);
  Inst_Printf(".ascii", "%s", asciiz);
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_STOP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Stop(int nb_consts)
{
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_START                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Start(int nb_longs)
{
  Label_Printf(".section\t.sdata");
  Inst_Printf(".align", "3");
}




/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, PlLong value)
{
  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
      Label_Printf(".section\t.bss");
      if (!global)
	{
	  Label_Printf("%s:", name);
	  Inst_Printf(".align", "3");
	  Inst_Printf(".space", "%d", value * 4);
	  /* Inst_Printf(".popsection",""); */
	}
      else
	{
	  Inst_Printf(".comm", "%s,%d", name, value * 4);
	}
      break;

    case INITIAL_VALUE:
      Label_Printf(".section\t.rodata");
      if (global)
	{
	  Inst_Printf(".globl", "%s", name);
	  Inst_Printf(".align", "3");
	  Inst_Printf(".size", "%s,4", name);
	  Label_Printf("%s:", name);
	  Inst_Printf(".word", "%d", value);
	}
      else
	{
	  Inst_Printf(".align", "3");
	  Inst_Printf(".size", "%s,4", name);
	  Label_Printf("%s:", name);
	  Inst_Printf(".word", "%d", value);
	}
      break;
    }
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_STOP                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Stop(int nb_longs)
{
}




/*-------------------------------------------------------------------------*
 * DATA_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Start(char *initializer_fct)
{
  /* last routine has to be closed first */
  if (act_routine[0] != '\0')
    {
      Inst_Printf("j", "$31");
      Inst_Printf(".end", "%s", act_routine);

      act_routine[0] = '\0';
    }

  if (initializer_fct == NULL)
    return;

  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
  Inst_Printf(".word", "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
