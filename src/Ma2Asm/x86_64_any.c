/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : x86_64_any.c                                                    *
 * Descr.: translation file for AMD x86-64                                 *
 * Author: Gwenole Beauchesne, Ozaki Kiichi and Daniel Diaz                *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz and Gwenole Beauchesne              *
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



/* Supported arch: x86_64 (64 bits) on Linux, BSD, Darwin (MacOS)
 *                 Solaris, MinGW, Cygwin, Windows
 */




/* x86_64/darwin needs a reg for pl_reg_bank (default is r12 see engine1.c)
 * so NO_MACHINE_REG_FOR_REG_BANK is never set (see machine.h). Else this 
 * error occurs '32-bit absolute addressing is not supported for x86-64'
 * On x86_64/linux, NO_MACHINE_REG_FOR_REG_BANK should work but can be 
 * easily undone in machine.h if problem occurs without register for pl_reg_bank
 */

#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK "pl_reg_bank(%rip)"
#elif defined(MAP_REG_BANK)
#define ASM_REG_BANK "%" MAP_REG_BANK
#else
#define ASM_REG_BANK "%r12"
#endif




/* Windows64 : allows for LARGEADDRESSAWARE (LAA)
 * if yes, needs RIP-related addressing
 * if no, pass option /LARGEADDRESSAWARE:NO to cl (in top_comp.c)
 */
#if 1 
#define LARGE_ADDRESS_AWARE 1
#endif


/* For x86_64 (Linux or Solaris): the C stack must be aligned on 16-bytes 
 * else some problems occur with double.
 * If this is not done and if the called function performs a movaps %xmm0,xx
 * an error will occur.
 * Just before calling a function %rsp is 16-bytes aligned, %rsp = 0x...0
 * (4 low bits = 0). The callq instruction pushes the return address, so at
 * the entry of a function, %rsp is 0x...8. GCC then adjusts (via subq)
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
 *
 * Mac OS X Yosemite (14.0.0) using clang: similar problem when using an 
 * xmmN regs and movdqa instruction (for integ operations on longs = 64bits). 
 * It appeared in the chkma utility. Due to a global variable misaligned.
 * defined initially with  .comm _ma_array,40000,3
 * problem was fixed using .comm _ma_array,40000,4
 */


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#ifdef M_darwin

#define UN                         "_"

#else

#define UN

#endif

#define MAX_C_ARGS_IN_C_CODE       32 /* must be a multiple of 2 */
#define RESERVED_STACK_SPACE       MAX_C_ARGS_IN_C_CODE * 8




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_e[32];
char asm_reg_b[32];
char asm_reg_cp[32];

int arg_reg_no;
int arg_dbl_reg_no;		/* unused on Microsoft ABI, same as arg_dbl_reg_no */


#if defined(__CYGWIN__) && !defined(_WIN32)
#define _WIN32 /* ensure Microsoft ABI on cygwin */
#endif

#ifdef _WIN32 /* Microsoft ABI */

#define MAX_ARGS_IN_REGS 4
char *arg_reg_name[MAX_ARGS_IN_REGS] = { "%rcx", "%rdx", "%r8", "%r9" };

#define MAX_ARGS_DOUBLE_IN_REGS 4
char *arg_dbl_reg_name[MAX_ARGS_DOUBLE_IN_REGS] = { "%xmm0", "%xmm1", "%xmm2", "%xmm3" };

#else /* System V AMD64 ABI */

#define MAX_ARGS_IN_REGS 6

char *arg_reg_name[MAX_ARGS_IN_REGS] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

#define MAX_ARGS_DOUBLE_IN_REGS 8
char *arg_dbl_reg_name[MAX_ARGS_DOUBLE_IN_REGS] = { "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7" };

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static char *Off_Reg_Bank(int offset);

#define LITTLE_INT(X) ((X) >= INT_MIN && (X) <= INT_MAX)




/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = FALSE;
  mi.can_produce_pic_code = TRUE;
  mi.comment_prefix = "#";

#ifdef M_darwin
  mi.local_symb_prefix = "L";
  mi.string_symb_prefix = "L_.str";
  mi.double_symb_prefix =  "LCPI";
#else
  mi.local_symb_prefix = ".L";
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = ".LCD";
#endif

  mi.strings_need_null = FALSE;
  mi.call_c_reverse_args = FALSE;
}




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
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

#if defined(M_darwin) || defined(M_bsd)
  pic_code = TRUE;  		/* NB: on darwin and BSD everything is PIC code */
#elif defined(M_linux) && __GNUC__ >= 6 /* gcc >= 6 needs PIC for linux */
  pic_code = FALSE;
#elif defined(_WIN32)
  pic_code = FALSE;  		/* NB: on MinGW nothing is needed for PIC code */
#endif

#ifdef M_darwin
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
  static char str[32];

#ifdef NO_MACHINE_REG_FOR_REG_BANK
  sprintf(str, "%d+%s", offset, ASM_REG_BANK);
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
Code_Start(CodeInf *c)
{
  Label_Printf("%s", "");
#ifdef M_darwin
  Inst_Printf(".align", "4, 0x90");
#else
#if 1				/* old code */
  Inst_Printf(".p2align", "4,,15");
#else
  Inst_Printf(".align", "16");
#endif
#if defined(M_linux) || defined(M_bsd) || defined(M_sco)
  Inst_Printf(".type", "%s,@function", c->name);
#endif
#endif

  if (c->global)
    Inst_Printf(".globl", UN "%s", c->name);

  Label(c->name);

  if (!c->prolog)
    {
      /* Save callee-saved registers. However, don't explicitly
         preserve %r12-%r15 since they are already handled as global
         -ffixed ones.  */
      Inst_Printf("pushq", "%%rbx");
      Inst_Printf("subq", "$%d, %%rsp", RESERVED_STACK_SPACE);
    }
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(CodeInf *c)
{
}




/*-------------------------------------------------------------------------*
 * LABEL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label(char *label)
{
  Label_Printf("%s", "");
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
  Inst_Printf("movq", "%s, %s", Off_Reg_Bank(MAP_OFFSET_E), asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
#ifndef M_darwin
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
  if (LARGE_ADDRESS_AWARE || pic_code)
    {
      Inst_Printf("leaq", "%s(%%rip), %%r10", Label_Cont_New());
      Inst_Printf("movq", "%%r10, %s", asm_reg_cp);
    }
  else
    {
      Inst_Printf("movq", "$%s, %s", Label_Cont_New(), asm_reg_cp);
    }
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf("%s:", Label_Cont_Get());
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
  Inst_Printf("movq", "%s, %%rdx", asm_reg_b);
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
#ifndef M_darwin
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
  Inst_Printf("movq", "%s, %%rdx", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movq", "%d(%s), %%rdx", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movq", "%%rdx, %s", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movq", "%%rdx, %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  arg_reg_no = 0;
  arg_dbl_reg_no = 0;
}

#ifdef __GNUC__                 /* ignore r_aux/r_eq_r_aux not always used */
#pragma GCC diagnostic push
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wunknown-warning-option"
#endif
#pragma GCC diagnostic ignored "-Wformat-overflow"
#endif


#ifdef _WIN32
#define BEFORE_ARG                                      \
{                                                       \
  char r[32], *r_aux;                                   \
  Bool r_eq_r_aux = FALSE;                              \
                                                        \
  if (arg_reg_no < MAX_ARGS_IN_REGS)                    \
    {                                                   \
      strcpy(r, arg_reg_name[arg_reg_no++]);            \
      r_aux = r;                                        \
      r_eq_r_aux = TRUE;                                \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset;                              \
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%rax";                                   \
    }

#define BEFORE_ARG_DOUBLE                               \
{                                                       \
  char r[32], *r_aux;                                   \
  Bool r_eq_r_aux = FALSE;                              \
                                                        \
  if (arg_reg_no < MAX_ARGS_DOUBLE_IN_REGS)             \
    {                                                   \
      strcpy(r, arg_dbl_reg_name[arg_reg_no++]);        \
      r_aux = r;                                        \
      r_eq_r_aux = TRUE;                                \
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
  char r[32], *r_aux;                                   \
  Bool r_eq_r_aux = FALSE;                              \
                                                        \
  if (arg_reg_no < MAX_ARGS_IN_REGS)                    \
    {                                                   \
      strcpy(r, arg_reg_name[arg_reg_no++]);            \
      r_aux = r;                                        \
      r_eq_r_aux = TRUE;                                \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset - arg_reg_no - arg_dbl_reg_no;\
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%rax";                                   \
    }


#define BEFORE_ARG_DOUBLE                               \
{                                                       \
  char r[32], *r_aux;                                   \
  Bool r_eq_r_aux = FALSE;                              \
                                                        \
  if (arg_dbl_reg_no < MAX_ARGS_DOUBLE_IN_REGS)         \
    {                                                   \
      strcpy(r, arg_dbl_reg_name[arg_dbl_reg_no++]);    \
      r_aux = r;                                        \
      r_eq_r_aux = TRUE;                                \
    }                                                   \
  else                                                  \
    {                                                   \
      int nwords = offset - arg_reg_no - arg_dbl_reg_no;\
                                                        \
      sprintf(r, "%d(%%rsp)", nwords * 8);              \
      r_aux = "%xmm8";                                  \
    }
#endif


#define AFTER_ARG                                       \
  if (!r_eq_r_aux)					\
    Inst_Printf("movq", "%s, %s", r_aux, r);            \
}

#define AFTER_ARG_DOUBLE			        \
  if (!r_eq_r_aux)                                      \
    Inst_Printf("movq", "%s, %s", r_aux, r);            \
}


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  BEFORE_ARG;

  if (LITTLE_INT(int_val))
    {
      Inst_Printf("movq", "$%" PL_FMT_d ", %s", int_val, r); /* optim */
      goto finish;
    }
  else
    Inst_Printf("movabsq", "$%" PL_FMT_d ", %s", int_val, r_aux);

  AFTER_ARG;

 finish:
  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, DoubleInf *d)
{
  BEFORE_ARG_DOUBLE;

  Inst_Printf("movsd", "%s(%%rip), %s", d->symb, r_aux);

  AFTER_ARG_DOUBLE;

  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, StringInf *s)
{
  BEFORE_ARG;

  if (LARGE_ADDRESS_AWARE || pic_code)
    Inst_Printf("leaq", "%s(%%rip), %s", s->symb, r_aux);
  else
    Inst_Printf("movq", "$%s, %s", s->symb, r_aux);
  

  AFTER_ARG;

  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, Bool adr_of, char *name, int index)
{
  BEFORE_ARG;

  if (pic_code)
    {
      Inst_Printf("movq", UN "%s@GOTPCREL(%%rip), %s", name, r_aux);
      if (adr_of)
	{
	  if (index != 0)
	    Inst_Printf("addq", "$%d, %s", index * 8, r_aux);
	}
      else
	Inst_Printf("movq", "%d(%s), %s", index * 8, r_aux, r_aux);
    }
  else
    {
      if (adr_of)
	{
	  if (LARGE_ADDRESS_AWARE)
	    {
	      Inst_Printf("leaq", "%s+%d(%%rip), %s", name, index * 8, r_aux);
 	    }
	  else
	    Inst_Printf("movq", "$" "%s+%d, %s", name, index * 8, r);
	}
      else
	{
	  Inst_Printf("movq", "%s+%d(%%rip), %s", name, index * 8, r_aux);
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
Call_C_Arg_Reg_X(int offset, Bool adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    {
#ifndef NO_MACHINE_REG_FOR_REG_BANK
      if (!r_eq_r_aux && index == 0)
        {
          Inst_Printf("movq", "%s, %s", ASM_REG_BANK, r); /* optim */
	  goto finish;
        }
#endif
      Inst_Printf("leaq", "%s, %s", Off_Reg_Bank(index * 8), r_aux);
    }
  else
    Inst_Printf("movq", "%s, %s", Off_Reg_Bank(index * 8), r_aux);

  AFTER_ARG;

finish:
  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, Bool adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    Inst_Printf("leaq", "%d(%s), %s", Y_OFFSET(index), asm_reg_e, r_aux);
  else
    Inst_Printf("movq", "%d(%s), %s", Y_OFFSET(index), asm_reg_e, r_aux);


  AFTER_ARG;

  return 1;
}


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, Bool adr_of, int index)
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
    }
  else
    {
      if (adr_of)
	{
	  if (LARGE_ADDRESS_AWARE)
	    Inst_Printf("leaq", UN "pl_foreign_long+%d(%%rip), %s", index * 8, r_aux);
	  else
	    Inst_Printf("movq", "$" UN "pl_foreign_long+%d, %s", index * 8, r_aux);
	}
      else
	{
	  Inst_Printf("movq", UN "pl_foreign_long+%d(%%rip), %s", index * 8, r_aux);
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
Call_C_Arg_Foreign_D(int offset, Bool adr_of, int index)
{
  if (adr_of)
    {
      BEFORE_ARG;

      if (pic_code)
	{
	  Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip), %s", r_aux);
	  if (index != 0)
	    Inst_Printf("addq", "$%d, %s", index * 8, r_aux);
	}
      else
	{
	  if (LARGE_ADDRESS_AWARE)
	    Inst_Printf("leaq", UN "pl_foreign_double+%d(%%rip), %s", index * 8, r_aux);
	  else
	    Inst_Printf("movq", "$" UN "pl_foreign_double+%d, %s", index * 8, r_aux);
	}
      
      AFTER_ARG;
      return 1;
    }

  BEFORE_ARG_DOUBLE;

  if (pic_code)
    {
      Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip), %%r10");
      Inst_Printf("movsd", "%d(%%r10), %s", index * 8, r_aux);
    }
  else
    {
      Inst_Printf("movsd", UN "pl_foreign_double+%d(%%rip), %s", index * 8, r_aux);
    }
  
  AFTER_ARG_DOUBLE;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
#ifndef M_darwin
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
Call_C_Stop(char *fct_name, int nb_args)
{
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
  Inst_Printf("test", "%%rax, %%rax");
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
      Inst_Printf("movq", UN "%s@GOTPCREL(%%rip), " "%%r10", name);
      Inst_Printf("movq", "%%rax, " "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movq", "%%rax, " "%s+%d(%%rip)", name, index * 8);
    }
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{                               /* similar to Move_To_Reg_X */
  Inst_Printf("movq", "%%rax, %s", Off_Reg_Bank(index * 8));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{                               /* similar to Move_To_Reg_Y */
  Inst_Printf("movq", "%%rax, %d(%s)", Y_OFFSET(index), asm_reg_e);
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
      Inst_Printf("movq", UN "pl_foreign_long@GOTPCREL(%%rip), " "%%r10");
      Inst_Printf("movq", "%%rax, " "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movq", "%%rax, " UN "pl_foreign_long+%d(%%rip)", index * 8);
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
      Inst_Printf("movq", UN "pl_foreign_double@GOTPCREL(%%rip), " "%%r10");
      Inst_Printf("movsd", "%%xmm0, " "%d(%%r10)", index * 8);
    }
  else
    {
      Inst_Printf("movsd", "%%xmm0, " UN "pl_foreign_double+%d(%%rip)", index * 8);
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
    Inst_Printf("testq", "%%rax, %%rax");
  else if (LITTLE_INT(int_val))
    Inst_Printf("cmpq", "$%" PL_FMT_d ", %%rax", int_val);
  else
    {
      /* %rdx is second integral return value. At this stage, it is bound
       * to be dead since we only deal with primitive object types. */
      Inst_Printf("movabsq", "$%" PL_FMT_d ", %%rdx", int_val);
      Inst_Printf("cmpq", "%%rdx, %%rax");
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
  Inst_Printf("addq", "$%d, %%rsp", RESERVED_STACK_SPACE);
  Inst_Printf("popq", "%%rbx");
  Inst_Printf("ret", "%s", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
#ifdef M_darwin
  Inst_Printf(".section", "__TEXT,__cstring,cstring_literals");
#elif !defined(_WIN32)
  Inst_Printf(".section", ".rodata.str1.1,\"aMS\",@progbits,1");
#else
  Inst_Printf(".section", ".rodata");  
#endif
}


/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(StringInf *s)
{
  Label_Printf("%s:", s->symb);
#ifdef M_darwin
  Inst_Printf(".asciz", "%s", s->str);
#else
  Inst_Printf(".string", "%s", s->str);
#endif
}


/*-------------------------------------------------------------------------*
 * DICO_STRING_STOP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Stop(int nb)
{
}




/*-------------------------------------------------------------------------*
 * DICO_DOUBLE_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double_Start(int nb)
{
#ifdef M_darwin
  Inst_Printf(".section", "__TEXT,__literal8,8byte_literals");
#elif !defined(_WIN32)
  Inst_Printf(".section", ".rodata.cst8,\"aM\",@progbits,8");
#else
  Inst_Printf(".section", ".rodata");  
#endif
  Inst_Printf(".align", "8");
}


/*-------------------------------------------------------------------------*
 * DICO_DOUBLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double(DoubleInf *d)
{
  Label_Printf("%s:", d->symb);
#if 1
  Inst_Printf(".quad", "%" FMT64_d, d->v.i64);
#else
  Inst_Printf(".long", "%d", d->v.i32[0]);
  Inst_Printf(".long", "%d", d->v.i32[1]);
#endif
}


/*-------------------------------------------------------------------------*
 * DICO_DOUBLE_STOP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double_Stop(int nb)
{
}




/*-------------------------------------------------------------------------*
 * DICO_LONG_START                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Start(int nb)
{
#ifdef M_darwin
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
Dico_Long(LongInf *l)
{
  PlLong size_bytes;
  switch (l->vtype)
    {
    case NONE:		/* in case ARRAY_SIZE since its value = 1 (see parser) */
    case ARRAY_SIZE:
      size_bytes = l->value * 8;
#ifdef M_darwin
      if (!l->global)
        Label_Printf(".zerofill __DATA,__bss," UN "%s,%" PL_FMT_d ",4", l->name, size_bytes);
      else
        Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",4", l->name, size_bytes);
#else
#if defined(M_linux) || defined(M_sco) || defined(M_solaris) || defined(M_bsd)
      if (!l->global)
        Inst_Printf(".local", UN "%s", l->name);
#else
      if (!l->global)
        Inst_Printf(".lcomm", UN "%s,%" PL_FMT_d, l->name, size_bytes);
      else
#endif
#if 1				/* work for all */
      Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",8", l->name, size_bytes);
#else  /* this does not work under MinGW - not used for the moment */
      if (l->value < 4)
	Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",8", l->name, size_bytes);
      else
	Inst_Printf(".comm", UN "%s,%" PL_FMT_d ",32", l->name, size_bytes);
#endif
#endif
      break;

    case INITIAL_VALUE:
      if (l->global)
        Inst_Printf(".globl", UN "%s", l->name);
#ifdef M_darwin
      Inst_Printf(".align", "3");
#else
      Inst_Printf(".align", "8");
#endif
#if !(defined(M_darwin) || defined(_WIN32))
      Inst_Printf(".size", UN "%s,8", l->name);
#endif
      Label_Printf(UN "%s:", l->name);
      Inst_Printf(".quad", "%" PL_FMT_d, l->value);
      break;
    }
}


/*-------------------------------------------------------------------------*
 * DICO_LONG_STOP                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long_Stop(int nb)
{
#ifdef M_darwin
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
#elif defined(M_darwin)
  Inst_Printf(".section", "__DATA,__mod_init_func,mod_init_funcs");
#else
  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
#endif
#ifdef M_darwin
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
