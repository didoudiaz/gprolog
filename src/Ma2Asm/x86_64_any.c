/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : x86_64_any.c                                                    *
 * Descr.: translation file for Linux on AMD x86-64                        *
 * Author: Gwenole Beauchesne                                              *
 *                                                                         *
 * Copyright (C) 2002 Gwenole Beauchesne                                   *
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

/* $Id: x86_64_any.c,v 1.1 2006/01/05 12:30:29 spa Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"
#define DOUBLE_PREFIX              ".LCD"

#define MAX_C_ARGS_IN_C_CODE       32

#define MAX_DOUBLES_IN_PRED        2048




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static double dbl_tbl[MAX_DOUBLES_IN_PRED];
static int nb_dbl = 0;
static int dbl_lc_no = 0;

char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;

#define MAX_GPR_ARGS 6
static int gpr_arg_no;
static const char *gpr_arg[MAX_GPR_ARGS] = {
  "%rdi", "%rsi", "%rdx",
  "%rcx", "%r8", "%r9"
};

#define MAX_FPR_ARGS 8
static int fpr_arg_no;
static const char *fpr_arg[MAX_FPR_ARGS] = {
  "%xmm0", "%xmm1", "%xmm2", "%xmm3",
  "%xmm4", "%xmm5", "%xmm6", "%xmm7"
};

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

#define LITTLE_INT(X) ((X) >= INT_MIN && (X) <= INT_MAX)


/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK "reg_bank"
#elif defined(MAP_REG_BANK)
#define ASM_REG_BANK "%" MAP_REG_BANK
#else
#define ASM_REG_BANK "%r12"
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%%%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "%rbx");
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
  int i;
  int x = dbl_lc_no - nb_dbl;

  for (i = 0; i < nb_dbl; i++)
    {
      union
      {
	double d;
	unsigned int w[2];
      } dbl = { .d = dbl_tbl[i] };

      Label_Printf("%s%d:", DOUBLE_PREFIX, x++);
      Inst_Printf(".long", "%d", dbl.w[0]);
      Inst_Printf(".long", "%d", dbl.w[1]);
    }
  nb_dbl = 0;

  Label_Printf("");
  Inst_Printf(".align", "16");
  Inst_Printf(".type", "%s,@function", label);

  if (global)
    Inst_Printf(".globl", "%s", label);

  Label(label);

  if (!prolog)
    {
      /* Save callee-saved registers. However, don't explicitly
         preserve %r12-%r15 since they are already handled as global
         -ffixed ones.  */
      Inst_Printf("pushq", "%%rbx");
      Inst_Printf("subq", "$%d,%%rsp", MAX_C_ARGS_IN_C_CODE * 8);
    }
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
  Label_Printf("%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Inst_Printf("movq", "%s,%s", Off_Reg_Bank(MAP_OFFSET_E), asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("jmp", "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  Inst_Printf("movq", "$.Lcont%d,%s", w_label, asm_reg_cp);
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
  Inst_Printf("jmp", "*-8(%s)", asm_reg_b);
#else
  Inst_Printf("movq", "%s,%%rdx", asm_reg_b);
  Inst_Printf("jmp", "*-8(%%rdx)");
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
  Inst_Printf("jmp", "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("movq", "%s,%%rdx", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movq", "%d(%s),%%rdx", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movq", "%%rdx,%s", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movq", "%%rdx,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline)
{
  gpr_arg_no = 0;
  fpr_arg_no = 0;
}

#define BEFORE_ARG						\
{								\
  char r[10], *r_aux;						\
  int r_eq_r_aux = 0;						\
								\
  if (gpr_arg_no < MAX_GPR_ARGS)				\
    {								\
      strcpy(r, gpr_arg[gpr_arg_no++]);				\
      r_aux = r;						\
      r_eq_r_aux = 1;						\
    }								\
  else								\
    {								\
      int nwords = offset - gpr_arg_no - fpr_arg_no;		\
								\
      sprintf(r, "%d(%%rsp)", nwords * 8);			\
      r_aux = "%rax";						\
    }


#define AFTER_ARG						\
}

#define BEFORE_FPR_ARG						\
{								\
  char r[10], *r_aux;						\
  int r_eq_r_aux = 0;						\
								\
  if (fpr_arg_no < MAX_FPR_ARGS)				\
    {								\
      strcpy(r, fpr_arg[fpr_arg_no++]);				\
      r_aux = r;						\
      r_eq_r_aux = 1;						\
    }								\
  else								\
    {								\
      int nwords = offset - gpr_arg_no - fpr_arg_no;		\
								\
      sprintf(r, "%d(%%rsp)", nwords * 8);			\
      r_aux = "%xmm8";						\
    }

#define AFTER_FPR_ARG						\
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  BEFORE_ARG;

  if (LITTLE_INT(int_val))
    Inst_Printf("movq", "$%ld,%s", int_val, r);
  else
    {
      Inst_Printf("movabsq", "$%ld,%s", int_val, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s,%s", r_aux, r);
    }

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
  BEFORE_FPR_ARG;

  dbl_tbl[nb_dbl++] = dbl_val;

  Inst_Printf("movsd", "%s%d(%%rip),%s", DOUBLE_PREFIX, dbl_lc_no++, r);

  AFTER_FPR_ARG;

  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  BEFORE_ARG;

  Inst_Printf("movq", "$%s%d,%s", STRING_PREFIX, str_no, r);

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
    Inst_Printf("movq", "$%s+%d,%s", name, index * 8, r);
  else
    {
      Inst_Printf("movq", "%s+%d(%%rip),%s", name, index * 8, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s,%s", r_aux, r);
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
	  Inst_Printf("movq", "$%s,%s", ASM_REG_BANK, r);
#else
	  Inst_Printf("movq", "%s,%s", ASM_REG_BANK, r);
#endif
	  goto finish;
	}
      Inst_Printf("leaq", "%s,%s", Off_Reg_Bank(index * 8), r_aux);
    }
  else
    Inst_Printf("movq", "%s,%s", Off_Reg_Bank(index * 8), r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movq", "%s,%s", r_aux, r);

finish:
  ;				/* gcc3 does not like use of label at end of compound statement */
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
    Inst_Printf("leaq", "%d(%s),%s", Y_OFFSET(index), asm_reg_e, r_aux);
  else
    Inst_Printf("movq", "%d(%s),%s", Y_OFFSET(index), asm_reg_e, r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movq", "%s,%s", r_aux, r);

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
    Inst_Printf("movq", "$foreign_long+%d, %s", index * 8, r);
  else
    {
      Inst_Printf("movq", "foreign_long+%d(%%rip),%s", index * 8, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s, %s", r_aux, r);
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
      Inst_Printf("movq", "$foreign_double+%d, %s", index * 8, r);
      AFTER_ARG;
      return 1;
    }

  BEFORE_FPR_ARG;

  Inst_Printf("movsd", "foreign_double+%d(%%rip),%s", index * 8, r_aux);
  if (!r_eq_r_aux)
    Inst_Printf("movsd", "%s, %s", r_aux, r);

  AFTER_FPR_ARG;

  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int nb_args)
{
  Inst_Printf("call", "%s", fct_name);
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
  Inst_Printf("jmp", "*%%rax");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("test", "%%rax,%%rax");
  Inst_Printf("je", "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("movq", "%%rax,%s+%d(%%rip)", name, index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* similar to Move_To_Reg_X */
  Inst_Printf("movq", "%%rax,%s", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* similar to Move_To_Reg_Y */
  Inst_Printf("movq", "%%rax,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("movq", "%%rax, foreign_long+%d(%%rip)", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("movsd", "%%xmm0, foreign_double+%d(%%rip)", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(long int_val)
{
  if (int_val == 0)
    Inst_Printf("testq", "%%rax,%%rax");
  else if (LITTLE_INT(int_val))
    Inst_Printf("cmpq", "$%ld,%%rax", int_val);
  else
    {
      /* %rdx is second integral return value. At this stage, it is
         bound to be dead since we only deal with primitive object
         types.  */
      Inst_Printf("movabsq", "$%ld,%%rdx", int_val);
      Inst_Printf("cmpq", "%%rdx,%%rax", int_val);
    }
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("je", "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("jg", "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("addq", "$%d,%%rsp", MAX_C_ARGS_IN_C_CODE * 8);
  Inst_Printf("popq", "%%rbx");
  Inst_Printf("ret", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
  Label_Printf(".section\t.rodata");
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
  Inst_Printf(".align", "16");
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
      if (!global)
	Inst_Printf(".local", "%s", name);
      Inst_Printf(".comm", "%s,%ld,8", name, value * 8);
      break;

    case INITIAL_VALUE:
      if (global)
	Inst_Printf(".globl", "%s", name);
      Label_Printf("%s:", name);
      Inst_Printf(".quad", "%ld", value);
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
  Inst_Printf(".align", "32");
  Label_Printf("obj_chain_start:");

  Inst_Printf(".quad", "%ld", OBJ_CHAIN_MAGIC_1);
  Inst_Printf(".quad", "%ld", OBJ_CHAIN_MAGIC_2);
  Inst_Printf(".quad", "obj_chain_stop");
  Inst_Printf(".quad", "%s", initializer_fct);
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
  Label_Printf("obj_chain_stop:");

  Inst_Printf(".quad", "obj_chain_start");
}
