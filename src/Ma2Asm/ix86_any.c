/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_any.c                                                      *
 * Descr.: translation file for Linux/Cygwin/... on intel x86              *
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
#include <string.h>

#include "../EnginePl/pl_params.h"
#include "../EnginePl/obj_chain.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"

#define MAX_C_ARGS_IN_C_CODE       32

#if (defined(M_ix86_cygwin) || defined(M_ix86_bsd)) && !defined(__FreeBSD__)

#define UN                         "_"

#else

#define UN

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[16];
char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;

#ifndef MAP_REG_E
int use_envir = 0;
#endif




	  /* variables for ma_parser.c */

int strings_need_null = 0;


	  /* variables for ma2asm.c */

int call_c_reverse_args = 0;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * SOURCE_LINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Source_Line(int line_no, char *cmt)
{
  Label_Printf("\t# %6d: %s", line_no, cmt);
}




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef MAP_REG_BANK
  sprintf(asm_reg_bank, "%%%s", MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "%ebx");
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%%%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "%esi");
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%%%s", MAP_REG_B);
#else
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%%%s", MAP_REG_CP);
#else
  sprintf(asm_reg_cp, "%d(%s)", MAP_OFFSET_CP, asm_reg_bank);
#endif

  Label_Printf(".text");

  Label("fail");
  Pl_Fail();
}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
}




/*-------------------------------------------------------------------------*
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("\n" UN "%s:", label);
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{
  Label_Printf("");
#if defined(M_ix86_linux) || defined(M_ix86_sco)
  Inst_Printf(".align", "16");
  Inst_Printf(".type", UN "%s,@function", label);
#else
  Inst_Printf(".align", "4");
#endif

  if (global)
    Inst_Printf(".globl", UN "%s", label);

  Label(label);

  if (!prolog)
    {
      Inst_Printf("pushl", "%%ebp");
      Inst_Printf("movl", "%%esp,%%ebp");
      Inst_Printf("subl", "$%d,%%esp", MAX_C_ARGS_IN_C_CODE * 4);
    }

#ifndef MAP_REG_E
  use_envir = 0;
#endif
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(void)
{
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * PL_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call(char *label)
{
  Inst_Printf("movl", "$.Lcont%d,%s", w_label, asm_reg_cp);
  Pl_Jump(label);
  Label_Printf(".Lcont%d:", w_label++);

#ifndef MAP_REG_E
  if (use_envir)
    Inst_Printf("movl", "%d(%s),%s", MAP_OFFSET_E, asm_reg_bank, asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
  Inst_Printf("movl", "%s,%%eax", asm_reg_b);
  Inst_Printf("jmp", "*-4(%%eax)");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("jmp", "*%s", asm_reg_cp);
#else
  Inst_Printf("jmp", "%s", asm_reg_cp);
#endif
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("movl", "%d(%s),%%eax", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movl", "-%d(%s),%%eax", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movl", "%%eax,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movl", "%%eax,-%d(%s)", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int nb_args)
{
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  Inst_Printf("movl", "$%ld,%d(%%esp)", int_val, offset * 4);

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, double dbl_val)
{
  int *p = (int *) &dbl_val;

  Inst_Printf("movl", "$%d,%d(%%esp)", p[0], offset * 4);
  Inst_Printf("movl", "$%d,%d(%%esp)", p[1], (offset + 1) * 4);

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  Inst_Printf("movl", "$%s%d,%d(%%esp)", STRING_PREFIX, str_no, offset * 4);

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  if (adr_of)
    Inst_Printf("movl", "$" UN "%s+%d,%d(%%esp)", name, index * 4,
		offset * 4);
  else
    {
      Inst_Printf("movl", UN "%s+%d,%%eax", name, index * 4);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
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
  if (adr_of)
    {
      if (index == 0)
	Inst_Printf("movl", "%s,%d(%%esp)", asm_reg_bank, offset * 4);
      else
	{
	  Inst_Printf("leal", "%d(%s),%%eax", index * 4, asm_reg_bank);
	  Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
	}
    }
  else
    {
      Inst_Printf("movl", "%d(%s),%%eax", index * 4, asm_reg_bank);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
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
  if (adr_of)
    {
      Inst_Printf("leal", "-%d(%s),%%eax", (index + 4) * 4, asm_reg_e);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
    }
  else
    {
      Inst_Printf("movl", "-%d(%s),%%eax", (index + 4) * 4, asm_reg_e);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
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
  if (adr_of)
    Inst_Printf("movl", "$" UN "foreign_long+%d,%d(%%esp)", index * 4,
		offset * 4);
  else
    {
      Inst_Printf("movl", UN "foreign_long+%d,%%eax", index * 4);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
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
  if (adr_of)
    {
      Inst_Printf("movl", "$" UN "foreign_double+%d,%d(%%esp)", index * 8,
		  offset * 4);
      return 1;
    }
  else
    {
      Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8);
      Inst_Printf("movl", "%%eax,%d(%%esp)", offset * 4);
      Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8 + 4);
      Inst_Printf("movl", "%%eax,%d(%%esp)", (offset + 1) * 4);
      return 2;
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args)
{
  Inst_Printf("call", UN "%s", fct_name);

#ifndef MAP_REG_E
  if (strcmp(fct_name, "Allocate") == 0)
    {
      use_envir = 1;
      Inst_Printf("movl", "%d(%s),%s", MAP_OFFSET_E, asm_reg_bank,
		  asm_reg_e);
    }
#endif
}




/*-------------------------------------------------------------------------*
 * JUMP_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_Ret(void)
{
  Inst_Printf("jmp", "*%%eax");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("testl", "%%eax,%%eax");
  Inst_Printf("je", UN "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("movl", "%%eax," UN "%s+%d", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("movl", "%%eax,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("movl", "%%eax,-%d(%s)", (index + 4) * 4, asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("movl", "%%eax," UN "foreign_long+%d", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("fstpl", UN "foreign_double+%d", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(long int_val)
{
  if (int_val == 0)
    Inst_Printf("testl", "%%eax,%%eax");
  else
    Inst_Printf("cmpl", "$%ld,%%eax", int_val);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("je", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("jg", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("leave", "");
  Inst_Printf("ret", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
#if !defined(M_ix86_bsd)
  Label_Printf(".section\t.rodata");
#endif
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{
  Label_Printf("%s%d:", STRING_PREFIX, str_no);
  Inst_Printf(".string", "%s", asciiz);
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
  Label_Printf(".data");
  Inst_Printf(".align", "4");
}




/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, long value)
{
  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
#if defined(M_ix86_linux) || defined(M_ix86_sco)
      if (!global)
	Inst_Printf(".local", UN "%s", name);
      Inst_Printf(".comm", UN "%s,%ld,4", name, value * 4);
#else
      if (!global)
	Inst_Printf(".lcomm", UN "%s,%ld", name, value * 4);
      else
	Inst_Printf(".comm", UN "%s,%ld", name, value * 4);
#endif
      break;

    case INITIAL_VALUE:
      if (global)
	Inst_Printf(".globl", UN "%s", name);
#ifdef M_ix86_cygwin
      Inst_Printf(".align", "4");
#endif
      Label_Printf(UN "%s:", name);
      Inst_Printf(".long", "%ld", value);
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
  if (initializer_fct == NULL)
    return;

  Label_Printf(".data");
  Label_Printf(UN "obj_chain_start:");

  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_1);
  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_2);
  Inst_Printf(".long", UN "obj_chain_stop");
  Inst_Printf(".long", UN "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
  if (initializer_fct == NULL)
    return;

  Label_Printf(".data");
  Label_Printf(UN "obj_chain_stop:");

  Inst_Printf(".long", UN "obj_chain_start");
}
