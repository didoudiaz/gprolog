/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_any.c                                                      *
 * Descr.: translation file for Linux/Cygwin/... on intel x86              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;

char *fc_arg_regs[] = FC_SET_OF_REGISTERS;
int inside_fc;
int fc_off_in_stack;




	  /* variables for ma_parser.c / ma2asm.c */

char *comment_prefix = "#";
char *local_symb_prefix = ".L";
int strings_need_null = 0;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static char *Off_Reg_Bank(int offset);




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK UN "reg_bank"
#elif defined(MAP_REG_BANK)
#define ASM_REG_BANK "%" MAP_REG_BANK
#else
#define ASM_REG_BANK "%ebx"
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%%%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "%edi");
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%%%s", MAP_REG_B);
#else
  strcpy(asm_reg_b, Off_Reg_Bank(MAP_OFFSET_B));
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%%%s", MAP_REG_CP);
#else
  strcpy(asm_reg_cp, Off_Reg_Bank(MAP_OFFSET_CP));
#endif

  Label_Printf(".text");

  Label("fail");
  Pl_Fail();
}




/*-------------------------------------------------------------------------*
 * OFF_REG_BANK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Off_Reg_Bank(int offset)
{
  static char str[16];

#ifdef NO_MACHINE_REG_FOR_REG_BANK
  sprintf(str, ASM_REG_BANK "+%d", offset);
#else
  sprintf(str, "%d(%s)", offset, ASM_REG_BANK);
#endif

  return str;
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
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{
  Label_Printf("");
  Inst_Printf(".align", "4");
#if defined(M_ix86_linux) || defined(M_ix86_sco)
  Inst_Printf(".type", UN "%s,@function", label);
#endif

  if (global)
    Inst_Printf(".globl", UN "%s", label);

  Label(label);

  if (!prolog)
    Inst_Printf("subl", "$%d,%%esp", MAX_C_ARGS_IN_C_CODE * 4);
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
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("");
#if 0
  Inst_Printf(".align", "4");
#endif
  Label_Printf(UN "%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Inst_Printf("movl", "%s,%s", Off_Reg_Bank(MAP_OFFSET_E), asm_reg_e);
#endif
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
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Inst_Printf("jmp", "*-4(%s)", asm_reg_b);
#else
  Inst_Printf("movl", "%s,%%eax", asm_reg_b);
  Inst_Printf("jmp", "*-4(%%eax)");
#endif
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
  Inst_Printf("movl", "%s,%%eax", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movl", "%d(%s),%%eax", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movl", "%%eax,%s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movl", "%%eax,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline)
{
#ifndef FC_USED_TO_COMPILE_CORE
  if (p_inline == NULL)		/* inlined code used a fast call */
    fc = 0;
#endif
  inside_fc = fc;
  fc_off_in_stack = 0;
}



#define BEFORE_ARG                                                          \
{                                                                           \
  char r[10], *r_aux;                                                       \
  int  r_eq_r_aux = 0;                                                      \
  int off1 = offset;                                                        \
                                                                            \
  if (inside_fc)                                                            \
    {                                                                       \
      if (offset < FC_MAX_ARGS_IN_REGS)                                     \
        {                                                                   \
  	  strcpy(r, fc_arg_regs[offset]);                                   \
          r_aux = r;                                                        \
          r_eq_r_aux = 1;                                                   \
        }                                                                   \
      else                                                                  \
        {                                                                   \
          off1 = (offset - FC_MAX_ARGS_IN_REGS + fc_off_in_stack);          \
	  sprintf(r, "%d(%%esp)", off1 * 4);                                \
          r_aux = "%esi";                                                   \
        }                                                                   \
    }                                                                       \
  else                                                                      \
    {                                                                       \
      sprintf(r, "%d(%%esp)", offset * 4);                                  \
      r_aux = "%eax";                                                       \
    }


#define BEFORE_HALF_ARG_DOUBLE                                              \
{                                                                           \
  char r[10];                                                               \
  int off1 = offset;                                                        \
                                                                            \
  if (inside_fc)                                                            \
    {                                                                       \
      if (offset < FC_MAX_ARGS_IN_REGS)                                     \
        fc_off_in_stack++;                                                  \
      else                                                                  \
        off1 = (offset - FC_MAX_ARGS_IN_REGS + fc_off_in_stack);            \
    }                                                                       \
  sprintf(r, "%d(%%esp)", off1 * 4);
  

#define AFTER_ARG                                                           \
}
    




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  BEFORE_ARG;

  Inst_Printf("movl", "$%ld,%s", int_val, r);

  AFTER_ARG;

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


  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d,%s", p[0], r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d,%s", p[1], r);

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  BEFORE_ARG;

  Inst_Printf("movl", "$%s%d,%s", STRING_PREFIX, str_no, r);

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("movl", "$" UN "%s+%d,%s", name, index * 4, r);
  else
    {
      Inst_Printf("movl", UN "%s+%d,%s", name, index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s,%s", r_aux, r);
    }

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    {
      if (!r_eq_r_aux && index == 0)
	{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
	  Inst_Printf("movl", "$%s,%s", ASM_REG_BANK, r);
#else
	  Inst_Printf("movl", "%s,%s", ASM_REG_BANK, r);
#endif
	  goto finish;
	}
      Inst_Printf("leal", "%s,%s", Off_Reg_Bank(index * 4), r_aux);
    }
  else
    Inst_Printf("movl", "%s,%s", Off_Reg_Bank(index * 4), r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);

 finish:  
  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("leal", "%d(%s),%s", Y_OFFSET(index), asm_reg_e, r_aux);
  else
    Inst_Printf("movl", "%d(%s),%s", Y_OFFSET(index), asm_reg_e, r_aux);
  
  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);
  
  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("movl", "$" UN "foreign_long+%d,%s", index * 4, r);
  else
    {
      Inst_Printf("movl", UN "foreign_long+%d,%s", index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s,%s", r_aux, r);
    }
  
  AFTER_ARG;

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
      BEFORE_ARG;

      Inst_Printf("movl", "$" UN "foreign_double+%d,%s", index * 8, r);

      AFTER_ARG;

      return 1;
    }

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8);
  Inst_Printf("movl", "%%eax,%s", r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "foreign_double+%d,%%eax", index * 8 + 4);
  Inst_Printf("movl", "%%eax,%s", r);

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int nb_args)
{
  Inst_Printf("call", UN "%s", fct_name);
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args, char **p_inline)
{
#ifndef MAP_REG_E
  if (p_inline && INL_ACCESS_INFO(p_inline))
    reload_e = 1;
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
  Inst_Printf("movl", "%%eax,%s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("movl", "%%eax,%d(%s)", Y_OFFSET(index), asm_reg_e);
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
  Inst_Printf("addl", "$%d,%%esp", MAX_C_ARGS_IN_C_CODE * 4);
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
