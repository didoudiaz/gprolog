/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : x86_64_any.c                                                    *
 * Descr.: translation file for Linux on AMD x86-64                        *
 * Author: Gwenole Beauchesne, Ozaki Kiichi and Daniel Diaz                *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz and Gwenole Beauchesne              *
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
#include <string.h>
#include <limits.h>


/* For M_x86_64_linux/solaris: an important point is that C stack must be
 * aligned on 16 bytes else some problems occurs with double.
 * If this is not done and if the called function performs a movaps %xmm0,xx
 * an error will occur.
 * Just before calling a function %rsp is 16bytes aligned, %rsp = 0x...0
 * (4 low bits = 0). The callq instruction pushes the return address, so at
 * the entry of a function, %rsp is 0x...8. Gcc then adjusts (via subq)
 * %rsp to be 0x...0 before calling a function. We mimic the same modifying
 * Call_Compiled to force %rsp to be 0x...0 when arriving in a Prolog code.
 * So a Prolog code can call C functions safely.
 * When a Prolog code finishes it returns into C inside Call_Prolog_Success
 * or Call_Prolog_Fail. In both functions we re-adjust the stack (gcc thinks
 * %rsp = 0x...8 while it is 0x...0): after the gcc adjustment code we
 * force %rsp to be 0x...0.
 * For MA c_code (MA code called by a C function), we have to reserve enough
 * space in the stack to pass args to C functions. We receive %rsp = 0x...c
 * In addition we have to push 1 register (%rbx)
 * Thus 0x...8 - 8 = 0x...0 : OK ! We have to sub to %rsp the space for
 * MAX_C_ARGS_IN_C_CODE*8 (this is OK if MAX_C_ARGS_IN_C_CODE is a multiple
 * of 2). So we have to reserve: MAX_C_ARGS_IN_C_CODE * 8.
 */


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#ifdef M_x86_64_darwin

#define STRING_PREFIX              "L_.str"
#define DOUBLE_PREFIX              "LCPI"

#define UN                         "_"

#define CONT_LABEL_FMT             "Ltmp%d"

#else

#define STRING_PREFIX              ".LC"
#define DOUBLE_PREFIX              ".LCD"

#define UN

#define CONT_LABEL_FMT             ".Lcont%d"

#endif

#define MAX_C_ARGS_IN_C_CODE       32 /* must be a multiple of 2 */
#define RESERVED_STACK_SPACE       MAX_C_ARGS_IN_C_CODE * 8

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

char asm_reg_e[20];
char asm_reg_b[20];
char asm_reg_cp[20];

int w_label = 0;

#ifdef _WIN32
#define MAX_PR_ARGS 4
static int pr_arg_no;
static const char *gpr_arg[MAX_PR_ARGS] = {
  "%rcx", "%rdx", "%r8", "%r9"
};

static const char *fpr_arg[MAX_PR_ARGS] = {
  "%xmm0", "%xmm1", "%xmm2", "%xmm3"
};
#else
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
#endif
          /* variables for ma_parser.c / ma2asm.c */

int can_produce_pic_code = 1;
char *comment_prefix = "#";
#ifdef M_x86_64_darwin
char *local_symb_prefix = "L";
#else
char *local_symb_prefix = ".L";
#endif
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

/* M_x86_64_darwin needs a reg for pl_reg°bank (default is r12 see engine1.c)
 * so NO_MACHINE_REG_FOR_REG_BANK is never set (see machine.h). Else this 
 * error occurs '32-bit absolute addressing is not supported for x86-64'
 */

#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK "pl_reg_bank"
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

#ifdef M_x86_64_darwin
  pic_code = 1;			/* NB: on darwin everything is PIC code */
#elif defined(_WIN32)
  pic_code = 0;			/* NB: on MinGW nothing is needed for PIC code */
#endif

#ifdef M_x86_64_darwin
  Inst_Printf(".section", "__TEXT,__text,regular,pure_instructions");
  Inst_Printf(".align", "4, 0x90");
#else
  Label_Printf(".text");
#endif

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
  static char str[20];

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
#ifdef __ELF__
  Inst_Printf(".section", ".note.GNU-stack,\"\",@progbits");
#endif
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
      } dbl;

      dbl.d = dbl_tbl[i];

      Label_Printf("%s%d:", DOUBLE_PREFIX, x++);
      Inst_Printf(".long", "%d", dbl.w[0]);
      Inst_Printf(".long", "%d", dbl.w[1]);
    }
  nb_dbl = 0;

  Label_Printf("");
#ifdef M_x86_64_darwin
  Inst_Printf(".align", "4, 0x90");
#else
#if 1				/* old code */
  Inst_Printf(".p2align", "4,,15");
#else
  Inst_Printf(".align", "16");
#endif
#if defined(M_x86_64_linux) || defined(M_x86_64_bsd) || defined(M_x86_64_sco)
  Inst_Printf(".type", "%s,@function", label);
#endif
#endif

  if (global)
    Inst_Printf(".globl", UN "%s", label);

  Label(label);

  if (!prolog)
    {
      /* Save callee-saved registers. However, don't explicitly
         preserve %r12-%r15 since they are already handled as global
         -ffixed ones.  */
      Inst_Printf("pushq", "%%rbx");
      Inst_Printf("subq", "$%d,%%rsp", RESERVED_STACK_SPACE);
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
#ifndef M_x86_64_darwin
  if (pic_code)
    Inst_Printf("jmp", UN "%s@PLT", label);
  else
#endif
    Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  if (pic_code)
    {
      Inst_Printf("leaq", CONT_LABEL_FMT "(%%rip),%%r10", w_label);
      Inst_Printf("movq", "%%r10,%s", asm_reg_cp);
    }
  else
    {
      Inst_Printf("movq", "$" CONT_LABEL_FMT ",%s", w_label, asm_reg_cp);
    }
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf(CONT_LABEL_FMT ":", w_label++);
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
#ifndef M_x86_64_darwin
  if (pic_code)
    Inst_Printf("jmp", UN "%s@PLT", label);
  else
#endif
    Inst_Printf("jmp", UN "%s", label);
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
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
             char **p_inline)
{
#ifdef _WIN32
  pr_arg_no = 0;
#else
  gpr_arg_no = 0;
  fpr_arg_no = 0;
#endif
}

#ifdef _WIN32
#define BEFORE_ARG                                      \
{                                                       \
  char r[10], *r_aux;                                   \
  int r_eq_r_aux = 0;                                   \
                                                        \
  if (pr_arg_no < MAX_PR_ARGS)                          \
    {                                                   \
      strcpy(r, gpr_arg[pr_arg_no++]);                  \
      r_aux = r;                                        \
      r_eq_r_aux = 1;                                   \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset;                              \
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%rax";                                   \
    }

#define BEFORE_FPR_ARG                                  \
{                                                       \
  char r[10], *r_aux;                                   \
  int r_eq_r_aux = 0;                                   \
                                                        \
  if (pr_arg_no < MAX_PR_ARGS)                          \
    {                                                   \
      strcpy(r, fpr_arg[pr_arg_no++]);                  \
      r_aux = r;                                        \
      r_eq_r_aux = 1;                                   \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset;                              \
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%xmm8";                                  \
    }

#else

#define BEFORE_ARG                                      \
{                                                       \
  char r[10], *r_aux;                                   \
  int r_eq_r_aux = 0;                                   \
                                                        \
  if (gpr_arg_no < MAX_GPR_ARGS)                        \
    {                                                   \
      strcpy(r, gpr_arg[gpr_arg_no++]);                 \
      r_aux = r;                                        \
      r_eq_r_aux = 1;                                   \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset - gpr_arg_no - fpr_arg_no;    \
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%rax";                                   \
    }

#define BEFORE_FPR_ARG                                  \
{                                                       \
  char r[10], *r_aux;                                   \
  int r_eq_r_aux = 0;                                   \
                                                        \
  if (fpr_arg_no < MAX_FPR_ARGS)                        \
    {                                                   \
      strcpy(r, fpr_arg[fpr_arg_no++]);                 \
      r_aux = r;                                        \
      r_eq_r_aux = 1;                                   \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset - gpr_arg_no - fpr_arg_no;    \
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%xmm8";                                  \
    }
#endif

#define AFTER_ARG                                       \
}

#define AFTER_FPR_ARG                                   \
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  BEFORE_ARG;

  if (LITTLE_INT(int_val))
    Inst_Printf("movq", "$%" PL_FMT_d ",%s", int_val, r);
  else
    {
      Inst_Printf("movabsq", "$%" PL_FMT_d ",%s", int_val, r_aux);
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

  Inst_Printf("movsd", "%s%d(%%rip),%s", DOUBLE_PREFIX, dbl_lc_no++, r_aux);
  if (!r_eq_r_aux)
    Inst_Printf("movq", "%s,%s", r_aux, r);

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

  if (pic_code)
    {
      Inst_Printf("leaq", "%s%d(%%rip),%s", STRING_PREFIX, str_no, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s,%s", r_aux, r);
    }
  else
    {
      Inst_Printf("movq", "$%s%d,%s", STRING_PREFIX, str_no, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s,%s", r_aux, r);
    }
  

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

  if (pic_code)
    {
      Inst_Printf("movq", UN "%s@GOTPCREL(%%rip),%s", name, r_aux);
      if (adr_of)
	{
	  if (index != 0)
	    Inst_Printf("addq", "$%d,%s", index * 8, r_aux);
	}
      else
	Inst_Printf("movq", "%d(%s),%s", index * 8, r_aux, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s,%s", r_aux, r);
    }
  else
    {
      if (adr_of)
	Inst_Printf("movq", "$" "%s+%d,%s", name, index * 8, r);
      else
	{
	  Inst_Printf("movq", "%s+%d(%%rip),%s", name, index * 8, r_aux);
	  if (!r_eq_r_aux)
	    Inst_Printf("movq", "%s,%s", r_aux, r);
	}
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
  ;                             /* gcc3 does not like use of label at end of compound statement */
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

  if (pic_code)
    {
      Inst_Printf("movq", UN "pl_foreign_long@GOTPCREL(%%rip), %s", r_aux);
      if (adr_of)
	{
	  if (index != 0)
	    Inst_Printf("addq", "$%d, %s", index * 8, r_aux);
	}
      else
	Inst_Printf("movq", "%d(%s), %s", index * 8, r_aux, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movq", "%s, %s", r_aux, r);
    }
  else
    {
      if (adr_of)
	Inst_Printf("movq", "$" UN "pl_foreign_long+%d, %s", index * 8, r);
      else
	{
	  Inst_Printf("movq", UN "pl_foreign_long+%d(%%rip),%s", index * 8, r_aux);
	  if (!r_eq_r_aux)
	    Inst_Printf("movq", "%s, %s", r_aux, r);
	}
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

      if (pic_code)
	{
	  Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip), %s", r_aux);
	  if (index != 0)
	    Inst_Printf("addq", "$%d, %s", index * 8, r_aux);
	  if (!r_eq_r_aux)
	    Inst_Printf("movq", "%s, %s", r_aux, r);
	}
      else
	{
	  Inst_Printf("movq", "$" UN "pl_foreign_double+%d, %s", index * 8, r_aux);
	  if (!r_eq_r_aux)
	    Inst_Printf("movq", "%s,%s", r_aux, r);
	}
      
      AFTER_ARG;
      return 1;
    }

  BEFORE_FPR_ARG;

  if (pic_code)
    {
      Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip),%%r10");
      Inst_Printf("movsd", "%d(%%r10), %s", index * 8, r_aux);
    }
  else
    {
      Inst_Printf("movsd", UN "pl_foreign_double+%d(%%rip),%s", index * 8, r_aux);
    }
  
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
Call_C_Invoke(char *fct_name, int fc, int nb_args, int nb_args_in_words)
{
#ifndef M_x86_64_darwin
  if (pic_code)
    Inst_Printf("call", UN "%s@PLT", fct_name);
  else
#endif
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
  Inst_Printf("je", UN "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  if (pic_code)
    {
      Inst_Printf("movq", UN "%s@GOTPCREL(%%rip)," "%%r10", name);
      Inst_Printf("movq", "%%rax," "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movq", "%%rax," "%s+%d(%%rip)", name, index * 8);
    }
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{                               /* similar to Move_To_Reg_X */
  Inst_Printf("movq", "%%rax,%s", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{                               /* similar to Move_To_Reg_Y */
  Inst_Printf("movq", "%%rax,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  if (pic_code)
    {
      Inst_Printf("movq", UN "pl_foreign_long@GOTPCREL(%%rip)," "%%r10");
      Inst_Printf("movq", "%%rax," "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movq", "%%rax," UN "pl_foreign_long+%d(%%rip)", index * 8);
    }
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  if (pic_code)
    {
      Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip)," "%%r10");
      Inst_Printf("movsd", "%%xmm0," "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movsd", "%%xmm0," UN "pl_foreign_double+%d(%%rip)", index * 8);
    }
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (int_val == 0)
    Inst_Printf("testq", "%%rax,%%rax");
  else if (LITTLE_INT(int_val))
    Inst_Printf("cmpq", "$%" PL_FMT_d ",%%rax", int_val);
  else
    {
      /* %rdx is second integral return value. At this stage, it is
         bound to be dead since we only deal with primitive object
         types.  */
      Inst_Printf("movabsq", "$%" PL_FMT_d ",%%rdx", int_val);
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
  Inst_Printf("addq", "$%d,%%rsp", RESERVED_STACK_SPACE);
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
#ifdef M_x86_64_darwin
  Inst_Printf(".section", UN "_TEXT,__cstring,cstring_literals");
#else
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
#ifdef M_x86_64_darwin
  Inst_Printf(".asciz", "%s", asciiz);
#else
  Inst_Printf(".string", "%s", asciiz);
#endif
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
#ifdef M_x86_64_darwin
  Inst_Printf(".section", "__DATA,__data");
  Inst_Printf(".align", "3");
#else
  Label_Printf(".data");
  Inst_Printf(".align", "16");
#endif
}


/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, PlLong value)
{
  PlLong size_bytes;
  switch (vtype)
    {
    case NONE:
      value = 1;                /* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
      size_bytes = value * 8;
#ifdef M_x86_64_darwin
      if (!global)
        Label_Printf(".zerofill __DATA,__bss," UN "%s,%" PL_FMT_d ",3", name, size_bytes);
      else
        Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",3", name, size_bytes);
#else
#if defined(M_x86_64_linux) || defined(M_x86_64_sco) || \
    defined(M_x86_64_solaris) || defined(M_x86_64_bsd)
      if (!global)
        Inst_Printf(".local", UN "%s", name);
#else
      if (!global)
        Inst_Printf(".lcomm", UN "%s,%" PL_FMT_d, name, size_bytes);
      else
#endif
#if 1				/* work for all */
      Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",8", name, size_bytes);
#else  /* this does not work under MinGW - not used for the moment */
      if (value < 4)
	Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",8", name, size_bytes);
      else
	Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",32", name, size_bytes);
#endif
#endif
      break;

    case INITIAL_VALUE:
      if (global)
        Inst_Printf(".globl", UN "%s", name);
#if !(defined(M_x86_64_darwin) || defined(_WIN32))
      Inst_Printf(".size", UN "%s,8", name);
#endif
      Label_Printf(UN "%s:", name);
      Inst_Printf(".quad", "%" PL_FMT_d, value);
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
#ifdef M_x86_64_darwin
  Label_Printf("\n\n.subsections_via_symbols");
#endif
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

#ifdef _MSC_VER
  Inst_Printf(".section", ".GPLC$m");
#elif defined(__CYGWIN__) || defined(_WIN32)
  Inst_Printf(".section", ".ctors,\"aw\"");
#elif defined(M_x86_64_darwin)
  Inst_Printf(".section", "__DATA,__mod_init_func,mod_init_funcs");
#else
  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
#endif
#ifdef M_x86_64_darwin
  Inst_Printf(".align", "3");
#else
  Inst_Printf(".align", "8");
#endif
  Inst_Printf(".quad", UN "%s", initializer_fct);
}


/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
