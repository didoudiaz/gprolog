/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_any.c                                                      *
 * Descr.: translation file for intel ix86                                 *
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
#include <string.h>


/* Supported arch: ix86 (32 bits) on Linux, BSD
 *                 Solaris, SCO, MinGW, Cygwin, Windows, Darwin (MacOS)
 *
 * ARM A32 instruction set                                                                                      
 * https://developer.arm.com/documentation/dui0801/k/A32-and-T32-Instructions?lang=en
 */




/* If no register is mapped for pl_reg_bank, %ebx is used (see engine1.c)
 */

#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define ASM_REG_BANK UN "pl_reg_bank"
#elif defined(MAP_REG_BANK)
#define ASM_REG_BANK "%" MAP_REG_BANK
#else
#define ASM_REG_BANK "%ebx"
#endif




/* For ix86/darwin: an important point is the C stack must be aligned
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

#define MAX_C_ARGS_IN_C_CODE       32 /* must be a multiple of 4 for darwin */
#define RESERVED_STACK_SPACE       MAX_C_ARGS_IN_C_CODE * 4 + 4

#if defined(__CYGWIN__) || defined (_WIN32) || defined(M_darwin)

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

char asm_reg_e[32];
char asm_reg_b[32];
char asm_reg_cp[32];

char *fc_arg_regs[] = FC_SET_OF_REGISTERS;
int stack_offset = 0;			/* offset wrt esp to store the next argument in the stack */
int fc_reg_no = 0;			/* index  wrt fc_arg_reg to store the next arg in a FC reg */
Bool eax_used_as_fc_reg = FALSE;	/* is eax already containing an arg (FC) ? */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static char *Off_Reg_Bank(int offset);



#ifdef M_darwin

#define DARWIN_PB_REG "%ebp"	/* PIC BASE (PB) customization */
#define DARWIN_PB_REG_SHORT (&DARWIN_PB_REG[2]) /* only bp from %ebp */

Bool load_pb_reg = FALSE;
int pb_label_no = 0;
char pb_label[32];

#include "../Wam2Ma/bt_string.h"
BTString bt_stub;
BTString bt_non_lazy;

void Emit_Non_Lazy(int str_no, char *name, void *unused_info);
void Emit_Stub(int str_no, char *name, void *unused_info);

#endif




/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.can_produce_pic_code = TRUE;
  mi.needs_pre_pass = TRUE;

#ifndef M_solaris
  mi.comment_prefix = "#";
#else
  mi.comment_prefix = "/";
#endif

#ifdef M_darwin
  mi.local_symb_prefix = "L";
  mi.string_symb_prefix = "L.str.";
  mi.double_symb_prefix =  "LCPI"; /* not used */
  mi.strings_need_null = TRUE;
#else
  mi.local_symb_prefix = ".L";
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = ".LCD"; /* not used */
  mi.strings_need_null = FALSE;
#endif

  mi.call_c_reverse_args = FALSE;

#ifdef M_darwin
  BT_String_Init(&bt_stub);
  BT_String_Init(&bt_non_lazy);
#endif
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

  Inst_Printf(".text", "%s", "");

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

#ifdef M_darwin
  if (bt_non_lazy.nb_elem)
    {
      Inst_Printf(".section __IMPORT,__pointers,non_lazy_symbol_pointers", "%s", "");
      BT_String_List(&bt_non_lazy, Emit_Non_Lazy);
    }

  if (bt_stub.nb_elem)
    {
      Inst_Printf(".section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5", "%s", "");

      BT_String_List(&bt_stub, Emit_Stub);
    }


  Inst_Printf(".subsections_via_symbols", "%s", "");
  Inst_Printf(".section", "__TEXT,__textcoal_nt,coalesced,pure_instructions");
  Label_Printf(".weak_definition\t___i686.get_pc_thunk.%s", DARWIN_PB_REG_SHORT);
  Label_Printf(".private_extern ___i686.get_pc_thunk.%s", DARWIN_PB_REG_SHORT);
  Label_Printf("___i686.get_pc_thunk.%s:", DARWIN_PB_REG_SHORT);
  Inst_Printf("movl", "(%%esp), %s", DARWIN_PB_REG);
  Inst_Printf("ret", "%s", "");
#endif
}

#ifdef M_darwin
void
Emit_Non_Lazy(int str_no, char *name, void *unused_info)
{
  Label_Printf("L_%s$non_lazy_ptr:", name);
  Label_Printf("\t.indirect_symbol _%s", name);
  Inst_Printf(".long", "0");
}

void
Emit_Stub(int str_no, char *name, void *unused_info)
{
  Label_Printf("L_%s$stub:", name);
  Label_Printf(".indirect_symbol _%s", name);
  Inst_Printf("hlt ; hlt ; hlt ; hlt ; hlt", "%s", "");
}



void
Load_PB_Reg(void)
{
  if (!load_pb_reg)
    return;
  int i;
  Inst_Printf("call", "___i686.get_pc_thunk.%s", DARWIN_PB_REG_SHORT);
  i = sprintf(pb_label, "\"L%011d$pb\"", ++pb_label_no);
  Label_Printf("%s:", pb_label);
  sprintf(pb_label + i, "(%s)", DARWIN_PB_REG);
  load_pb_reg = FALSE;
}
#endif




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(CodeInf *c)
{
#ifdef M_solaris
  Inst_Printf(".align", "4");
#elif defined(M_darwin)
#else
  Inst_Printf(".p2align", "4,,15");
#endif
#if defined(M_linux) || defined(M_bsd) || defined(M_sco)
  Inst_Printf(".type", UN "%s,@function", c->name);
#endif

  if (c->global)
    Label_Printf(".globl " UN "%s", c->name);

  Label(c->name);

  if (!c->prolog)
    {
#ifdef M_darwin
      Inst_Printf("pushl", "%s", DARWIN_PB_REG);
#endif
      Inst_Printf("pushl", "%%esi"); /* used as r_aux when %eax is a FC reg */
      Inst_Printf("subl", "$%d, %%esp", RESERVED_STACK_SPACE);
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
#if 0
  Inst_Printf(".align", "4");
#endif
  Label_Printf(UN "%s:", label);
#ifdef M_darwin
  load_pb_reg = TRUE;
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
  Inst_Printf("movl", "%s, %s", Off_Reg_Bank(MAP_OFFSET_E), asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
#ifdef M_darwin
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
#ifdef M_darwin
  Load_PB_Reg();
  Inst_Printf("leal", "%s-%s, %%eax", Label_Cont_New(), pb_label);
  Inst_Printf("movl", "%%eax, %s", asm_reg_cp);
#else
  Inst_Printf("movl", "$%s, %s", Label_Cont_New(), asm_reg_cp);
#endif
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Label_Printf("%s:", Label_Cont_Get());
#ifdef M_darwin
  load_pb_reg = TRUE;
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
  Inst_Printf("movl", "%s, %%eax", asm_reg_b);
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
  Inst_Printf("movl", "%s, %%eax", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("movl", "%d(%s), %%eax", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("movl", "%%eax, %s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("movl", "%%eax, %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void 
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
#ifndef FC_USED_TO_COMPILE_CORE
  fc = FALSE;
#endif
  stack_offset = 0;
  if (fc)
    fc_reg_no = 0;
  else
    fc_reg_no = FC_MAX_ARGS_IN_REGS; /* so no more regs left to use */
  eax_used_as_fc_reg = FALSE;
}


#ifdef __GNUC__			/* ignore r_aux/r_eq_r_aux not always used */
#pragma GCC diagnostic push
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wunknown-warning-option"
#endif
#pragma GCC diagnostic ignored "-Wformat-overflow"
#endif


#define BEFORE_ARG					\
{							\
  char r[10], *r_aux;					\
  Bool r_eq_r_aux = FALSE;				\
							\
  if (fc_reg_no < FC_MAX_ARGS_IN_REGS)			\
    {							\
      strcpy(r, fc_arg_regs[fc_reg_no++]);		\
      if (strcmp("%eax", r) == 0)			\
	eax_used_as_fc_reg = TRUE;			\
      r_aux = r;					\
      r_eq_r_aux = TRUE;				\
    }							\
  else							\
    {							\
      sprintf(r, "%d(%%esp)", stack_offset * 4);	\
      stack_offset++;					\
      r_aux = (eax_used_as_fc_reg) ? "%esi" : "%eax";	\
    }

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif


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

  Inst_Printf("movl", "$%ld, %s", int_val, r);

  AFTER_ARG;

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, DoubleInf *d)
{
  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d, %s", d->v.i32[0], r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", "$%d, %s", d->v.i32[1], r);

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, StringInf *s)
{
  BEFORE_ARG;

#ifdef M_darwin
  Load_PB_Reg();
  Inst_Printf("leal", "%s-%s, %s", s->symb, pb_label, r_aux);
  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s, %s", r_aux, r);
#else
  Inst_Printf("movl", "$%s, %s", s->symb, r);
#endif

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
#ifdef M_darwin
  LongInf *l = Get_Long_Infos(name);
#endif

  BEFORE_ARG;

#ifdef M_darwin

  Load_PB_Reg();

  if ((l && l->global && l->vtype != INITIAL_VALUE) ||
      (!l && !Is_Code_Defined(name))) /* external code */
    {
      BT_String_Add(&bt_non_lazy, name); /* strdup done by parser */
      Inst_Printf("movl", "L_%s$non_lazy_ptr-%s, %s", name, pb_label, r_aux);
      if (adr_of)
	{
	  if (index > 0)
	    Inst_Printf("addl", "$%d, %s", index * 4, r_aux);
	}
      else
	Inst_Printf("movl", "%d(%s), %s", index * 4, r_aux, r_aux);
    }
  else
    {
      if (adr_of)
	Inst_Printf("leal", "%d+_%s-%s, %s", index * 4, name, pb_label, r_aux);
      else
	Inst_Printf("movl", "%d+_%s-%s, %s", index * 4, name, pb_label, r_aux);
    }

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s, %s", r_aux, r);

#else /* !M_darwin */

  if (adr_of)
    Inst_Printf("movl", "$" UN "%s+%d, %s", name, index * 4, r);
  else
    {
      Inst_Printf("movl", UN "%s+%d, %s", name, index * 4, r_aux);
      if (!r_eq_r_aux)
	Inst_Printf("movl", "%s, %s", r_aux, r);
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
Call_C_Arg_Reg_X(int offset, Bool adr_of, int index)
{
  BEFORE_ARG;

  if (adr_of)
    {
      if (!r_eq_r_aux && index == 0)
	{
#ifdef NO_MACHINE_REG_FOR_REG_BANK
	  Inst_Printf("movl", "$%s, %s", ASM_REG_BANK, r);
#else
	  Inst_Printf("movl", "%s, %s", ASM_REG_BANK, r);
#endif
	  goto finish;
	}
      Inst_Printf("leal", "%s, %s", Off_Reg_Bank(index * 4), r_aux);
    }
  else
    Inst_Printf("movl", "%s, %s", Off_Reg_Bank(index * 4), r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s, %s", r_aux, r);

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
    Inst_Printf("leal", "%d(%s), %s", Y_OFFSET(index), asm_reg_e, r_aux);
  else
    Inst_Printf("movl", "%d(%s), %s", Y_OFFSET(index), asm_reg_e, r_aux);

  if (!r_eq_r_aux)
    Inst_Printf("movl", "%s, %s", r_aux, r);

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
  return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_long", index);
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, Bool adr_of, int index)
{
  if (adr_of)
    return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_double", index * 2);

  BEFORE_HALF_ARG_DOUBLE;

#ifdef M_darwin

  Load_PB_Reg();
  Inst_Printf("movl", "L_pl_foreign_double$non_lazy_ptr-%s, %s", pb_label, r_aux);
  Inst_Printf("movsd", "%d(%s), %%xmm0", index * 8, r_aux);
  Inst_Printf("movsd", "%%xmm0, %s", r);
  stack_offset++;

#else /* !M_darwin */

  Inst_Printf("movl", UN "pl_foreign_double+%d, %s", index * 8, r_aux);
  Inst_Printf("movl", "%s, %s", r_aux, r);

  AFTER_ARG;

  offset++;

  BEFORE_HALF_ARG_DOUBLE;

  Inst_Printf("movl", UN "pl_foreign_double+%d, %%eax", index * 8 + 4);
  Inst_Printf("movl", "%%eax, %s", r);

#endif

  AFTER_ARG;

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
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
	Inst_Printf("subl", "$%d, %%esp", stack_offset * 4);
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
  Inst_Printf("jmp", "*%%eax");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("testl", "%%eax, %%eax");
  Inst_Printf("je", UN "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("movl", "%%eax, " UN "%s+%d", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("movl", "%%eax, %s", Off_Reg_Bank(index * 4));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("movl", "%%eax, %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("movl", "%%eax, " UN "pl_foreign_long+%d", index * 4);
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
    Inst_Printf("testl", "%%eax, %%eax");
  else
    Inst_Printf("cmpl", "$%ld, %%eax", int_val);
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
  Inst_Printf("addl", "$%d, %%esp", RESERVED_STACK_SPACE);
  Inst_Printf("popl", "%%esi");
#ifdef M_darwin
  Inst_Printf("popl", "%s", DARWIN_PB_REG);
#endif
  Inst_Printf("ret", "%s", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
#if defined( __CYGWIN__) || defined (_WIN32)
  Inst_Printf(".section", ".rdata,\"dr\"");
#elif defined(M_solaris)
  Inst_Printf(".section", ".rodata");
#elif defined(M_darwin)
  Inst_Printf(".cstring", "%s", "");
#else
  Inst_Printf(".section", ".rodata.str1.1,\"aMS\",@progbits,1");
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
  Inst_Printf(".ascii", "%s", s->str);
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
}


/*-------------------------------------------------------------------------*
 * DICO_DOUBLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double(DoubleInf *d)
{
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
  Inst_Printf(".data", "%s", "");
#ifdef M_darwin
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
Dico_Long(LongInf *l)
{
  switch (l->vtype)
    {
    case NONE:		/* in case ARRAY_SIZE since its value = 1 (see parser) */
    case ARRAY_SIZE:
#if defined(M_linux) || defined(M_sco) || \
    defined(M_solaris) || defined(M_bsd)
      if (!l->global)
	Inst_Printf(".local", UN "%s", l->name);
      Inst_Printf(".comm", UN "%s,%ld,4", l->name, l->value * 4);
#else
      if (!l->global)
#ifdef M_darwin
	Inst_Printf(".lcomm", UN "%s,%ld,2", l->name, l->value * 4);
#else
	Inst_Printf(".lcomm", UN "%s,%ld", l->name, l->value * 4);
#endif
      else
	Inst_Printf(".comm", UN "%s,%ld", l->name, l->value * 4);
#endif
      break;

    case INITIAL_VALUE:
      if (l->global)
	Label_Printf(".globl " UN "%s", l->name);
#ifdef M_darwin
      Inst_Printf(".align", "2");
#else
      Inst_Printf(".align", "4");
#if !defined(__CYGWIN__) && !defined(_WIN32)
      Inst_Printf(".type", UN "%s,@object", l->name);
      Inst_Printf(".size", UN "%s,4", l->name);
#endif
#endif
      Label_Printf(UN "%s:", l->name);
      Inst_Printf(".long", "%ld", l->value);
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

#ifdef M_darwin
  Inst_Printf(".mod_init_func", "%s", "");
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

#endif /* M_darwin */
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
