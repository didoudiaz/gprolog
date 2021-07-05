/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ppc32_any.c                                                     *
 * Descr.: translation file PowerPC 32 bits                                *
 * Author: Daniel Diaz and Lindsey Spratt                                  *
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
#include <string.h>


/* Supported arch: powerpc 32 bits on Linux, Darwin (MacOS)
 */


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_C_ARGS_IN_C_CODE       32

#define MAX_DOUBLES_IN_PRED        2048


#if defined(M_linux) || defined(M_bsd)

#define UN
#define R(reg)                     #reg
#define F(reg)                     #reg
#define CR(reg)                    #reg
#define HI(adr)                    #adr "@ha"
#define HI_UN(adr)                 UN #adr "@ha"
#define LO(adr)                    #adr "@l"
#define LO_UN(adr)                 UN #adr "@l"

#else

#define UN                         "_"
#define R(reg)                     "r" #reg
#define F(reg)                     "f" #reg
#define CR(reg)                    "cr" #reg
#define HI(adr)                    "ha16(" #adr ")"
#define HI_UN(adr)                 "ha16(" UN #adr ")"
#define LO(adr)                    "lo16(" #adr ")"
#define LO_UN(adr)                 "lo16(" UN #adr ")"

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[30];		/* enough big sizes to avoid compiler warnings */
char asm_reg_e[40];
char asm_reg_b[40];
char asm_reg_cp[40];

int dbl_reg_no;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define LITTLE_INT(int_val)     ((unsigned) ((int_val) + 32768) < 65536)

#define IHI(x)                  ((unsigned) ((unsigned long) x >> 16))
#define ILO(x)                  ((unsigned) ((unsigned long) x & 0xFFFF))




/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = FALSE;
  mi.can_produce_pic_code = FALSE;
  mi.comment_prefix = "#";
  mi.local_symb_prefix = ".L";	/* TODO check local symbol on darwin */
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = ".LCD";
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
#ifdef MAP_REG_BANK
  sprintf(asm_reg_bank, R(%s), MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, R(15));
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, R(%s), MAP_REG_E);
#else
  strcpy(asm_reg_e, R(16));
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, R(%s), MAP_REG_B);
#else
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, R(%s), MAP_REG_CP);
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
  Inst_Printf(".align", "2");
#if defined(M_linux) || defined(M_bsd)
  Inst_Printf(".type", "%s,@function", c->name);
#endif

  if (c->global)
    Inst_Printf(".globl", UN "%s", c->name);

  Label(c->name);

  if (!c->prolog)
    {
      Inst_Printf("mr", R(12) ", " R(1));
      Inst_Printf("addi", R(1) ", " R(1) ", -%d", MAX_C_ARGS_IN_C_CODE * 4);
      Inst_Printf("stw", R(12) ", 0(" R(1) ")");
      Inst_Printf("mflr", R(0));
      Inst_Printf("stw", R(0) ", %d(" R(1) ")", (MAX_C_ARGS_IN_C_CODE + 1) * 4);
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
  Label_Printf("\n" UN "%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Inst_Printf("lwz", "%s, %d(%s)", asm_reg_e, MAP_OFFSET_E, asm_reg_bank);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("b", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("addis", R(9) ", 0, " HI(%s), Label_Cont_New());
  Inst_Printf("addi", R(9) ", " R(9) ", " LO(%s), Label_Cont_Get());
  Inst_Printf("stw", R(9) ", %s", asm_reg_cp);
#else
  Inst_Printf("addis", "%s, 0, " HI(%s), asm_reg_cp, Label_Cont_New());
  Inst_Printf("addi", "%s, %s, " LO(%s), asm_reg_cp, asm_reg_cp, Label_Cont_Get());
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
  Inst_Printf("lwz", R(9) ", -4(%s)", asm_reg_b);
#else
  Inst_Printf("lwz", R(9) ", %s", asm_reg_b);
  Inst_Printf("lwz", R(9) ", -4(" R(9) ")");
#endif

  Inst_Printf("mtctr", R(9));
  Inst_Printf("bctr", "%s", "");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("lwz", R(0) ", %s", asm_reg_cp);
  Inst_Printf("mtctr", R(0));
#else
  Inst_Printf("mtctr", "%s", asm_reg_cp);
#endif

  Inst_Printf("bctr", "%s", "");
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("b", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("lwz", R(0) ", %d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("lwz", R(0) ", %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("stw", R(0) ", %d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("stw", R(0) ", %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  dbl_reg_no = 0;
}




#if defined(M_linux) || defined(M_bsd)

#define STACK_OFFSET(offset)   offset * 4 - 24
#define DBL_RET_WORDS          0

#else

#define STACK_OFFSET(offset)   offset * 4 + 24
#define DBL_RET_WORDS          2

#endif

#define MAX_ARGS_IN_REGS 8

#define BEFORE_ARG				\
{						\
  char r[32];					\
  if (offset < MAX_ARGS_IN_REGS)		\
    sprintf(r, R(%d), offset + 3);		\
  else						\
    strcpy(r, R(11));




#define AFTER_ARG							\
  if (offset >= MAX_ARGS_IN_REGS)					\
    Inst_Printf("stw", "%s, %d(" R(1) ")", r, STACK_OFFSET(offset));	\
}


#if defined(M_linux) || defined(M_bsd)

#define AFTER_ARG_DBL						\
}

#else

#define AFTER_ARG_DBL						\
  if (offset >= MAX_ARGS_IN_REGS)				\
    Inst_Printf("stfd", F(%d) ", %d(" R(1) ")", dbl_reg_no,	\
		offset * 4 + 24);				\
}

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
    Inst_Printf("li", "%s, %ld", r, int_val);
  else
    {
      Inst_Printf("lis", "%s, %#x", r, IHI(int_val));
      Inst_Printf("ori", "%s, %s, %#x", r, r, ILO(int_val));
    }

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
  BEFORE_ARG;

  Inst_Printf("addis", "%s, 0, " HI(%s), r, d->symb);
  Inst_Printf("lfd", F(%d) ", " LO(%s) "(%s)", ++dbl_reg_no, d->symb, r);

  AFTER_ARG_DBL;

  return DBL_RET_WORDS;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, StringInf *s)
{
  BEFORE_ARG;

  Inst_Printf("addis", "%s, 0, " HI(%s), r, s->symb);
  Inst_Printf("addi", "%s, %s, " LO(%s), r, r, s->symb);

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

  Inst_Printf("addis", "%s, 0, " HI_UN(%s+%d), r, name, index * 4);

  if (adr_of)
    Inst_Printf("addi", "%s, %s, " LO_UN(%s+%d), r, r, name, index * 4);
  else
    Inst_Printf("lwz", "%s, " LO_UN(%s+%d) "(%s)", r, name, index * 4, r);

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
    Inst_Printf("addi", "%s, %s, %d", r, asm_reg_bank, index * 4);
  else
    Inst_Printf("lwz", "%s, %d(%s)", r, index * 4, asm_reg_bank);

  AFTER_ARG;

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
    Inst_Printf("addi", "%s, %s, %d", r, asm_reg_e, Y_OFFSET(index));
  else
    Inst_Printf("lwz", "%s, %d(%s)", r, Y_OFFSET(index), asm_reg_e);

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

  Inst_Printf("addis", "%s, 0, " HI_UN(pl_foreign_long+%d), r, index * 4);

  if (adr_of)
    Inst_Printf("addi", "%s, %s, " LO_UN(pl_foreign_long+%d), r, r, index * 4);
  else
    Inst_Printf("lwz", "%s, " LO_UN(pl_foreign_long+%d) "(%s)", r, index * 4, r);

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
  BEFORE_ARG;

  Inst_Printf("addis", "%s, 0, " HI_UN(pl_foreign_double+%d), r,
#if defined(M_linux) || defined(M_bsd)
	      index * 4
#else
	      index * 8
#endif
	      );

  if (adr_of)
    {
      Inst_Printf("addi", "%s, %s, " LO_UN(pl_foreign_double+%d), r, r, index * 8);
      if (offset >= MAX_ARGS_IN_REGS)
	Inst_Printf("stw", "%s, %d(" R(1) ")", r, offset * 4 + 24);

      return 1;
    }

  Inst_Printf("lfd", F(%d) ", " LO_UN(pl_foreign_double+%d) "(%s)", ++dbl_reg_no, index * 8, r);

  AFTER_ARG_DBL;

  return DBL_RET_WORDS;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
#if 0	/* only useful to call varargs functions - not the case here */
  if (dbl_reg_no == 0)
    Inst_Printf("crxor", "6, 6, 6");
  else
    Inst_Printf("creqv", "6, 6, 6");
#endif

  Inst_Printf("bl", UN "%s", fct_name);
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
  Inst_Printf("mtctr", R(3));
  Inst_Printf("bctr", "%s", "");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("cmpwi", CR(1) ", " R(3) ", 0");
  Inst_Printf("bne", CR(1) ", %s", Label_Cont_New());
  Inst_Printf("b", UN "fail");
  Label_Printf("%s:", Label_Cont_Get());
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("addis", R(4) ", 0, " HI_UN(%s+%d), name, index * 4);
  Inst_Printf("stw", R(3) ", " LO_UN(%s+%d) "(" R(4) ")", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* similar to Move_To_Reg_X */
  Inst_Printf("stw", R(3) ", %d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* similar to Move_To_Reg_Y */
  Inst_Printf("stw", R(3) ", %d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("addis", R(4) ", 0, " HI_UN(pl_foreign_long+%d), index * 4);
  Inst_Printf("stw", R(3) ", " LO_UN(pl_foreign_long+%d) "(" R(4) ")", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("addis", R(4) ", 0, " HI_UN(pl_foreign_double+%d), index * 8);
  Inst_Printf("stfd", F(1) ", " LO_UN(pl_foreign_double+%d) "(" R(4) ")", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (LITTLE_INT(int_val))
    Inst_Printf("cmpwi", CR(1) ", " R(3) ", %ld", int_val);
  else
    {
      Inst_Printf("lis", R(0) ", %#x", IHI(int_val));
      Inst_Printf("ori", R(0) ", " R(0) ", %#x", ILO(int_val));
      Inst_Printf("cmpw", CR(1) ", " R(3) ", " R(0));
    }
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", CR(1) ", " UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("bgt", CR(1) ", " UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("lwz", R(0) ", %d(" R(1) ")", (MAX_C_ARGS_IN_C_CODE + 1) * 4);
  Inst_Printf("mtlr", R(0));
  Inst_Printf("addi", R(1) ", " R(1) ", %d", MAX_C_ARGS_IN_C_CODE * 4);
  Inst_Printf("blr", "%s", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
#if defined(M_linux) || defined(M_bsd)
  Inst_Printf(".section", ".rodata");
#else
  Label_Printf(".cstring");
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
#if defined(M_linux) || defined(M_bsd)
  Inst_Printf(".string", "%s", s->str);
#else
  Inst_Printf(".asciz", "%s", s->str);
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
#if defined(M_linux) || defined(M_bsd)
  Inst_Printf(".section", ".rodata");
#else
  /* TODO */
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
  Inst_Printf(".double", "%.17g", d->v.dbl);
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
  Label_Printf(".data");
  Inst_Printf(".align", "4");
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
#if defined(M_linux) || defined(M_bsd)
      if (!l->global)
	Inst_Printf(".local", UN "%s", l->name);
      Inst_Printf(".comm", UN "%s,%ld,4", l->name, l->value * 4);
#else
      if (!l->global)
	Inst_Printf(".lcomm", UN "%s,%ld,4", l->name, l->value * 4);
      else
	Inst_Printf(".comm", UN "%s,%ld", l->name, l->value * 4);
#endif
      break;

    case INITIAL_VALUE:
      if (l->global)
	Inst_Printf(".globl", UN "%s", l->name);
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

#ifdef M_linux
  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
  Inst_Printf(".align", "2");
  Inst_Printf(".long", UN "%s", initializer_fct);
#else
  Label_Printf(".data");
  Label_Printf(".mod_init_func");
  Inst_Printf(".align", "2");
  Inst_Printf(".long", UN "%s", initializer_fct);
#endif
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

#if 0
  Label_Printf(".data");
  Label_Printf(UN "obj_chain_stop:");

  Inst_Printf(".long", UN "obj_chain_start");
#endif
}
