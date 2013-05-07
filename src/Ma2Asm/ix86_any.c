/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_any.c                                                      *
 * Descr.: translation file for Linux/Cygwin/mingw32/... on intel x86      *
 * Author: Daniel Diaz                                                     *
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* For M_ix86_darwin: an important point is the C stack must be aligned
 * on 16 bytes. It is possible to use gcc option -mstackrealign but
 * it produces bigger and slower code (and uses %ecx as register).
 * If this is not done and if the called function performs a movdqa
 * an error will occur (generally Bus Error).
 * Just before calling a function %esp is 16bytes aligned, %esp = 0x...0
 * (4 low bits = 0). The call instruction pushes the return address, so at
 * the entry of a function, %esp is 0x...c. Gcc then adjusts (via subl)
 * %esp to be 0x...0 before calling a function. We mimic the same modifying
 * Call_Compiled to force %esp to be 0x...0 when arriving in a Prolog code.
 * So a Prolog code can call C functions safely.
 * When a Prolog code finishes it returns into C inside Call_Prolog_Success
 * or Call_Prolog_Fail. In both functions we re-adjust the stack (gcc thinks
 * %esp = 0x...c while it is 0x...0): after the gcc adjustment code we
 * force %esp to be 0x...0.
 * For MA c_code (MA code called by a C function), we have to reserve enough
 * space in the stack to pass args to C functions. We receive %esp = 0x...c
 * In addition we have to push 2 registers (%ebp = PB_REG and %esi)
 * Thus 0x...c - 4 - 4 = 0x...4. We have to sub 4 to %esp and the space for
 * MAX_C_ARGS_IN_C_CODE*4 (this is OK if MAX_C_ARGS_IN_C_CODE is a multiple
 * of 4). So we have to reserve: 4 + MAX_C_ARGS_IN_C_CODE * 4.
 */


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"

#define MAX_C_ARGS_IN_C_CODE       32 /* must be a multiple of 4 for darwin */
#define RESERVED_STACK_SPACE       MAX_C_ARGS_IN_C_CODE * 4 + 4

#if defined(__CYGWIN__) || defined (_WIN32) || defined(M_ix86_darwin)

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

extern int pic_code;


char asm_reg_e[20];
char asm_reg_b[20];
char asm_reg_cp[20];

int w_label = 0;

char *fc_arg_regs[] = FC_SET_OF_REGISTERS;
int stack_offset = 0;		/* offset wrt esp to store the next argument in the stack */
int fc_reg_no = 0;		/* index  wrt fc_arg_reg to store the next arg in a FC reg */
int eax_used_as_fc_reg = 0;	/* is eax already containing an arg (FC) ? */




	  /* variables for ma_parser.c / ma2asm.c */
int can_produce_pic_code = 1;
#ifndef M_solaris
char *comment_prefix = "#";
#else
char *comment_prefix = "/";
#endif

#ifdef M_ix86_darwin
char *local_symb_prefix = "L";
#else
char *local_symb_prefix = ".L";
#endif
#ifdef M_ix86_darwin
int strings_need_null = 1;
#else
int strings_need_null = 0;
#endif

int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static char *Off_Reg_Bank(int offset);



#ifdef M_ix86_darwin

#define DARWIN_PB_REG "%ebp"	/* PIC BASE (PB) customization */
int load_pb_reg = 0;
int pb_label_no = 0;
char pb_label[32];

int needs_pre_pass = 1;		/* overwritte var of ma_parser.c */

#include "../Wam2Ma/bt_string.h"
BTString bt_stub;
BTString bt_non_lazy;

void Emit_Non_Lazy(int str_no, char *name);
void Emit_Stub(int str_no, char *name);

#endif




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK UN "pl_reg_bank"
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

  Inst_Printf(".text", "");

  Label("fail");
  Pl_Fail();

#ifdef M_ix86_darwin
  BT_String_Init(&bt_stub);
  BT_String_Init(&bt_non_lazy);
#endif
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

#ifdef M_ix86_darwin
  if (bt_non_lazy.nb_elem)
    {
      Inst_Printf(".section __IMPORT,__pointers,non_lazy_symbol_pointers", "");
      BT_String_List(&bt_non_lazy, Emit_Non_Lazy);
    }

  if (bt_stub.nb_elem)
    {
      Inst_Printf(".section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5", "");

      BT_String_List(&bt_stub, Emit_Stub);
    }


  Inst_Printf(".subsections_via_symbols", "");
  Inst_Printf(".section", "__TEXT,__textcoal_nt,coalesced,pure_instructions");
  Label_Printf(".weak_definition\t___i686.get_pc_thunk.%s", DARWIN_PB_REG + 2);
  Label_Printf(".private_extern ___i686.get_pc_thunk.%s", DARWIN_PB_REG + 2);
  Label_Printf("___i686.get_pc_thunk.%s:", DARWIN_PB_REG + 2);
  Inst_Printf("movl", "(%%esp), %s", DARWIN_PB_REG);
  Inst_Printf("ret", "");
#endif
}

#ifdef M_ix86_darwin
void
Emit_Non_Lazy(int str_no, char *name)
{
  Label_Printf("L_%s$non_lazy_ptr:", name);
  Label_Printf("\t.indirect_symbol _%s", name);
  Inst_Printf(".long", "0");
}

void
Emit_Stub(int str_no, char *name)
{
  Label_Printf("L_%s$stub:", name);
  Label_Printf(".indirect_symbol _%s", name);
  Inst_Printf("hlt ; hlt ; hlt ; hlt ; hlt", "");
}



void
Load_PB_Reg(void)
{
  if (!load_pb_reg)
    return;
  int i;
  Inst_Printf("call", "___i686.get_pc_thunk.%s", DARWIN_PB_REG + 2);
  i = sprintf(pb_label, "\"L%011d$pb\"", ++pb_label_no);
  Label_Printf("%s:", pb_label);
  sprintf(pb_label + i, "(%s)", DARWIN_PB_REG);
  load_pb_reg = 0;
}
#endif



/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{
#ifdef M_solaris
  Inst_Printf(".align", "4");
#elif defined(M_ix86_darwin)
#else
  Inst_Printf(".p2align", "4,,15");
#endif
#if defined(M_ix86_linux) || defined(M_ix86_bsd) || defined(M_ix86_sco)
  Inst_Printf(".type", UN "%s,@function", label);
#endif

  if (global)
    Label_Printf(".globl " UN "%s", label);

  Label(label);

  if (!prolog)
    {
#ifdef M_ix86_darwin
      Inst_Printf("pushl", "%s", DARWIN_PB_REG);
#endif
      Inst_Printf("pushl", "%%esi"); /* used as r_aux when %eax is a FC reg */
      Inst_Printf("subl", "$%d,%%esp", RESERVED_STACK_SPACE);
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
#if 0
  Inst_Printf(".align", "4");
#endif
  Label_Printf(UN "%s:", label);
#ifdef M_ix86_darwin
  load_pb_reg = 1;
#endif
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
#ifdef M_ix86_darwin
  if (!Is_Code_Defined(label))
    {
      BT_String_Add(&bt_stub, strdup(label));
      Inst_Printf("jmp", "L_%s$stub", label);
    }
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
#ifdef M_ix86_darwin
  Load_PB_Reg();
  Inst_Printf("leal", "Lcont%d-%s,%%eax", w_label, pb_label);
  Inst_Printf("movl", "%%eax,%s", asm_reg_cp);
#else
  Inst_Printf("movl", "$.Lcont%d,%s", w_label, asm_reg_cp);
#endif
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf("%scont%d:", local_symb_prefix, w_label++);
#ifdef M_ix86_darwin
  load_pb_reg = 1;
#endif
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
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
	     char **p_inline)
{
#ifndef FC_USED_TO_COMPILE_CORE
  if (p_inline == NULL)		/* inlined code used a fast call */
    fc = 0;
#endif
  stack_offset = 0;
  if (fc)
    fc_reg_no = 0;
  else
    fc_reg_no = FC_MAX_ARGS_IN_REGS; /* so no more regs left to use */
  eax_used_as_fc_reg = 0;
}




#define BEFORE_ARG					\
{							\
  char r[10], *r_aux;					\
  int  r_eq_r_aux = 0;					\
							\
  if (fc_reg_no < FC_MAX_ARGS_IN_REGS)			\
    {							\
      strcpy(r, fc_arg_regs[fc_reg_no++]);		\
      if (strcmp("%eax", r) == 0)			\
	eax_used_as_fc_reg = 1;				\
      r_aux = r;					\
      r_eq_r_aux = 1;					\
    }							\
  else							\
    {							\
      sprintf(r, "%d(%%esp)", stack_offset * 4);	\
      stack_offset++;					\
      r_aux = (eax_used_as_fc_reg) ? "%esi" : "%eax";	\
    }


/* In GCC 3, the 3 first args are passed via registers if they are
 * ints (recall: 1 double = 2 ints). So if the 2 first args are
 * double (4 ints) nothing is passed in registers.
 * In GCC 4, the 3 first int args are passed in register whatever
 * the previous arg types.
 */

#if __GNUC__ >= 4 || defined(_MSC_VER)
#define SKIP_FC_REG
#else
#define SKIP_FC_REG   fc_reg_no++
#endif

#define BEFORE_HALF_ARG_DOUBLE			\
{						\
  char r[10], *r_aux;				\
						\
  if (fc_reg_no < FC_MAX_ARGS_IN_REGS)		\
    SKIP_FC_REG;				\
  sprintf(r, "%d(%%esp)", stack_offset * 4);	\
  stack_offset++;				\
  r_aux = (eax_used_as_fc_reg) ? "%esi" : "%eax";


#define AFTER_ARG				\
}





/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
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

#ifdef M_ix86_darwin
  Load_PB_Reg();
  Inst_Printf("leal", "%s%d-%s,%s", STRING_PREFIX, str_no, pb_label, r_aux);
  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);
#else
  Inst_Printf("movl", "$%s%d,%s", STRING_PREFIX, str_no, r);
#endif

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
#ifdef M_ix86_darwin
  int global, value;
  VType vtype;
  int is_a_long;
#endif

  BEFORE_ARG;

#ifdef M_ix86_darwin

  Load_PB_Reg();
  is_a_long = Get_Long_Infos(name, &global, &vtype, &value);

  if ((is_a_long && global && vtype != INITIAL_VALUE) ||
      (!is_a_long && !Is_Code_Defined(name))) /* external code */
    {
      BT_String_Add(&bt_non_lazy, name); /* strdup done by parser */
      Inst_Printf("movl", "L_%s$non_lazy_ptr-%s,%s", name, pb_label, r_aux);
      if (adr_of)
	{
	  if (index > 0)
	    Inst_Printf("addl", "$%d,%s", index * 4, r_aux);
	}
      else
	Inst_Printf("movl", "%d(%s),%s", index * 4, r_aux, r_aux);
    }
  else
    {
      if (adr_of)
	Inst_Printf("leal", "%d+_%s-%s,%s", index * 4, name, pb_label, r_aux);
      else
	Inst_Printf("movl", "%d+_%s-%s,%s", index * 4, name, pb_label, r_aux);
    }

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s,%s", r_aux, r);

#else /* !M_ix86_darwin */

  if (adr_of)
    Inst_Printf("movl", "$" UN "%s+%d,%s", name, index * 4, r);
  else
    {
      Inst_Printf("movl", UN "%s+%d,%s", name, index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s,%s", r_aux, r);
    }

#endif

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
  ;    /* gcc3 does not like use of label at end of compound statement */
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
  return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_long", index);
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  if (adr_of)
    return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_double", index * 2);

  BEFORE_HALF_ARG_DOUBLE;

#ifdef M_ix86_darwin

  Load_PB_Reg();
  Inst_Printf("movl", "L_pl_foreign_double$non_lazy_ptr-%s,%s", pb_label, r_aux);
  Inst_Printf("movsd", "%d(%s),%%xmm0", index * 8, r_aux);
  Inst_Printf("movsd", "%%xmm0,%s", r);
  stack_offset++;

#else /* !M_ix86_darwin */

  Inst_Printf("movl", UN "pl_foreign_double+%d,%s", index * 8, r_aux);
  Inst_Printf("movl", "%s,%s", r_aux, r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "pl_foreign_double+%d,%%eax", index * 8 + 4);
  Inst_Printf("movl", "%%eax,%s", r);

#endif

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int fc, int nb_args, int nb_args_in_words)
{
#if defined(_MSC_VER) && FC_MAX_ARGS_IN_REGS > 0
  if (fc)
    {
      /* under MSVC: __fastcall implies decorated names @fct_name@nb_args_in_word
       * It also implies the callee pops the args then we have to readjust the stack.
       * I suppose this removes the benefit of passing args in stack:
       * by default it is switched off (see file arch_dep.h)
       */
      Inst_Printf("call", "@%s@%d", fct_name, nb_args_in_words * sizeof(int));
      if (stack_offset > 0)
	Inst_Printf("subl", "$%d,%%esp", stack_offset * 4);
      return;
    }
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
  Inst_Printf("movl", "%%eax," UN "pl_foreign_long+%d", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("fstpl", UN "pl_foreign_double+%d", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
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
  Inst_Printf("addl", "$%d,%%esp", RESERVED_STACK_SPACE);
  Inst_Printf("popl", "%%esi");
#ifdef M_ix86_darwin
  Inst_Printf("popl", "%s", DARWIN_PB_REG);
#endif
  Inst_Printf("ret", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
#if defined( __CYGWIN__) || defined (_WIN32)
  Inst_Printf(".section", ".rdata,\"dr\"");
#elif defined(M_solaris)
  Inst_Printf(".section", ".rodata");
#elif defined(M_ix86_darwin)
  Inst_Printf(".cstring", "");
#else
  Inst_Printf(".section", ".rodata.str1.1,\"aMS\",@progbits,1");
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
#ifdef M_ix86_darwin
  Inst_Printf(".ascii", "%s", asciiz);
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
  Inst_Printf(".data", "");
#ifdef M_ix86_darwin
  Inst_Printf(".align", "2");
#else
  Inst_Printf(".align", "4");
#endif
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
#if defined(M_ix86_linux) || defined(M_ix86_sco) || \
    defined(M_ix86_solaris) || defined(M_ix86_bsd)
      if (!global)
	Inst_Printf(".local", UN "%s", name);
      Inst_Printf(".comm", UN "%s,%ld,4", name, value * 4);
#else
      if (!global)
#ifdef M_ix86_darwin
	Inst_Printf(".lcomm", UN "%s,%ld,2", name, value * 4);
#else
	Inst_Printf(".lcomm", UN "%s,%ld", name, value * 4);
#endif
      else
	Inst_Printf(".comm", UN "%s,%ld", name, value * 4);
#endif
      break;

    case INITIAL_VALUE:
      if (global)
	Label_Printf(".globl " UN "%s", name);
#ifdef M_ix86_darwin
      Inst_Printf(".align", "2");
#else
      Inst_Printf(".align", "4");
#if !defined(__CYGWIN__) && !defined(_WIN32)
      Inst_Printf(".type", UN "%s,@object", name);
      Inst_Printf(".size", UN "%s,4", name);
#endif
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

#if defined(M_ix86_darwin)
  Inst_Printf(".mod_init_func", "");
  Inst_Printf(".align", "2");
  Inst_Printf(".long", UN "%s", initializer_fct);

#else

#ifdef _MSC_VER
  Inst_Printf(".section", ".GPLC$m");
#elif defined( __CYGWIN__) || defined (_WIN32)
  Inst_Printf(".section", ".ctors,\"aw\"");
#else
  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
#endif
  Inst_Printf(".align", "4");
  Inst_Printf(".long", UN "%s", initializer_fct);

#endif /* M_ix86_darwin */
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
