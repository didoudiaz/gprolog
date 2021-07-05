/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : sparc32_any.c                                                   *
 * Descr.: translation file for sparc 32 bits                              *
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
#include <string.h>
#include <stdarg.h>


/* Supported arch: sparc64 on Solaris, SunOS, BSD
 */




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#ifdef M_sunos

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

char asm_reg_bank[20];		/* enough big sizes to avoid compiler warnings */
char asm_reg_e[32];
char asm_reg_b[32];
char asm_reg_cp[32];

char *delay_op;
char delay_operands[1024];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Delay_Printf(char *op, char *operands, ...);



#define LITTLE_INT(int_val)     ((unsigned) ((int_val) + 4096) < 8192)




/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = FALSE;
  mi.can_produce_pic_code = FALSE;
  mi.comment_prefix = "!";	/* NB: # does not work on Solaris 9 */
  mi.local_symb_prefix = "L";
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = ".LCD"; /* not used */
  mi.strings_need_null = TRUE;
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
  sprintf(asm_reg_bank, "%%%s", MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "%l0");
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%%%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "%l1");
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%%%s", MAP_REG_B);
#else
  sprintf(asm_reg_b, "[%s+%d]", asm_reg_bank, MAP_OFFSET_B);
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%%%s", MAP_REG_CP);
#else
  sprintf(asm_reg_cp, "[%s+%d]", asm_reg_bank, MAP_OFFSET_CP);
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
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(CodeInf *c)
{
  Label_Printf("%s", "");
  Inst_Printf(".align", "4");
#if defined(M_solaris) || defined(M_bsd)
  Inst_Printf(".type", UN "%s,#function", c->name);
#endif
  Inst_Printf(".proc", "020");

  if (c->global)
    Inst_Printf(".global", UN "%s", c->name);

  Label(c->name);

  if (!c->prolog)
    Inst_Printf("save", "%%sp, -104, %%sp");
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
  Inst_Printf("ld", "[%s+%d], %s", asm_reg_bank, MAP_OFFSET_E, asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("call", UN "%s", label);
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  Inst_Printf("sethi", "%%hi(%s-8), %%g1", Label_Cont_New());
  Inst_Printf("or", "%%g1, %%lo(%s-8), %%g1", Label_Cont_Get());
  Inst_Printf("st", "%%g1, %s", asm_reg_cp);
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
  Inst_Printf("call", UN "%s, 0", label);
#ifdef MAP_REG_CP
  Inst_Printf("mov", "%%o7, %s", asm_reg_cp);	/* delay slot */
#else
  Inst_Printf("st", "%%o7, %s", asm_reg_cp);	/* delay slot */
#endif
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Inst_Printf("ld", "[%s-4], %%o0", asm_reg_b);
#else
  Inst_Printf("ld", "%s, %%o0", asm_reg_b);
  Inst_Printf("ld", "[%%o0-4], %%o0");
#endif

  Inst_Printf("call", "%%o0");
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("jmp", "%s+8", asm_reg_cp);
#else
  Inst_Printf("ld", "%s, %%o0", asm_reg_cp);
  Inst_Printf("jmp", "%%o0+8");
#endif
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("ba", UN "%s", label);
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("ld", "[%s+%d], %%o0", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("ld", "[%s%+d], %%o0", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("st", "%%o0, [%s+%d]", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("st", "%%o0, [%s%+d]", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  delay_op = NULL;
}



#define MAX_ARGS_IN_REGS 6

#define BEFORE_ARG				\
{						\
  char r[32];					\
						\
  if (offset < MAX_ARGS_IN_REGS)		\
    sprintf(r, "%%o%d", offset);		\
  else						\
    strcpy(r, "%l7");


#define AFTER_ARG					\
  if (offset >= MAX_ARGS_IN_REGS)			\
    Delay_Printf("st", "%s, [%%sp+%d]", r,		\
                 92 + (offset - MAX_ARGS_IN_REGS) * 4);	\
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
    Delay_Printf("mov", "%ld, %s", int_val, r);
  else
    {
      Delay_Printf("sethi", "%%hi(%ld), %s", int_val, r);
      Delay_Printf("or", "%s, %%lo(%ld), %s", r, int_val, r);
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

  Delay_Printf("sethi", "%%hi(%d), %s", d->v.i32[0], r);
  Delay_Printf("or", "%s, %%lo(%d), %s", r, d->v.i32[0], r);

  AFTER_ARG;

  offset++;

  BEFORE_ARG;

  Delay_Printf("sethi", "%%hi(%d), %s", d->v.i32[1], r);
  Delay_Printf("or", "%s, %%lo(%d), %s", r, d->v.i32[1], r);

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

  Delay_Printf("sethi", "%%hi(%s), %s", s->symb, r);
  Delay_Printf("or", "%s, %%lo(%s), %s", r, s->symb, r);

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

  Delay_Printf("sethi", "%%hi(" UN "%s+%d), %s", name, index * 4, r);
  if (adr_of)
    Delay_Printf("or", "%s, %%lo(" UN "%s+%d), %s", r, name, index * 4, r);
  else
    Delay_Printf("ld", "[%s+%%lo(" UN "%s+%d)], %s", r, name, index * 4, r);

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
    Delay_Printf("add", "%s, %d, %s", asm_reg_bank, index * 4, r);
  else
    Delay_Printf("ld", "[%s+%d], %s", asm_reg_bank, index * 4, r);

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
    Delay_Printf("add", "%s, %+d, %s", asm_reg_e, Y_OFFSET(index), r);
  else
    Delay_Printf("ld", "[%s%+d], %s", asm_reg_e, Y_OFFSET(index), r);

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

  if (adr_of)
    Delay_Printf("add", "%%l2, %d, %s", index * 4, r);
  else
    Delay_Printf("ld", "[%%l2+%d], %s", index * 4, r);

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

      Delay_Printf("add", "%%l3, %d, %s", index * 8, r);

      AFTER_ARG;

      return 1;
    }

  BEFORE_ARG;

  Delay_Printf("ld", "[%%l3+%d], %s", index * 8, r);

  AFTER_ARG;

  offset++;

  BEFORE_ARG;

  Delay_Printf("ld", "[%%l3+%d], %s", index * 8 + 4, r);

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
  Inst_Printf("call", UN "%s", fct_name);
  if (delay_op)
    Inst_Out(delay_op, delay_operands);
  else
    Inst_Printf("nop", "%s", "");	/* delay slot */
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
  Inst_Printf("jmp", "%%o0");
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("cmp", "%%o0, 0");

#if 0
  Inst_Printf("be", UN "fail");
  Inst_Printf("nop", "%s", "");	/* delay slot */
#else

  Inst_Printf("be", UN "%s+4", "fail");
#ifdef MAP_REG_B
  Inst_Printf("ld", "[%s-4], %%o0", asm_reg_b);
#else
  Inst_Printf("ld", "%s, %%o0", asm_reg_b);
#endif

#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("sethi", "%%hi(" UN "%s+%d), %%o1", name, index * 4);
  Inst_Printf("st", "%%o0, [%%o1+%%lo(" UN "%s+%d)]", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("st", "%%o0, [%s+%d]", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("st", "%%o0, [%s%+d]", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("st", "%%o0, [%%l2+%d]", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("std", "%%f0, [%%l3+%d]", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (LITTLE_INT(int_val))
    Inst_Printf("cmp", "%%o0, %ld", int_val);
  else
    {
      Inst_Printf("sethi", "%%hi(%ld), %%o1", int_val);
      Inst_Printf("or", "%%o1, %%lo(%ld), %%o1", int_val);
      Inst_Printf("cmp", "%%o0, %%o1");
    }
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("be", UN "%s", label);
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("bg", UN "%s", label);
  Inst_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ret", "%s", "");
  Inst_Printf("restore", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
  Inst_Printf(".section", "\".rodata\"");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(StringInf *s)
{
  Inst_Printf(".align", "8");
  Label_Printf("%s:", s->symb);
  Inst_Printf(".asciz", "%s", s->str);
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
#ifdef M_sunos
  Label_Printf(".data");
#else
  Inst_Printf(".section", "\".data\"");
#endif
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
#ifdef M_sunos
      if (!l->global)
	Inst_Printf(".reserve", UN "%s,%ld,\"bss\",4", l->name, l->value * 4);
      else
	Inst_Printf(".common", UN "%s,%ld,\"bss\"", l->name, l->value * 4);
#else
      if (!l->global)
	Inst_Printf(".local", UN "%s", l->name);
      Inst_Printf(".common", UN "%s,%ld,4", l->name, l->value * 4);
#endif
      break;

    case INITIAL_VALUE:
#if defined(M_solaris) || defined(M_bsd)
      Inst_Printf(".type", UN "%s,#object", l->name);
      Inst_Printf(".size", UN "%s,4", l->name);
#endif
      if (l->global)
	Inst_Printf(".global", UN "%s", l->name);
      Label_Printf(UN "%s:", l->name);
#ifdef M_sunos
      Inst_Printf(".word", "%ld", l->value);
#else
      Inst_Printf(".uaword", "%ld", l->value);
#endif
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

  Inst_Printf(".section", "\".ctors\",#alloc,#write");
  Inst_Printf(".align", "4");
  Inst_Printf(".long", UN "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}




/*-------------------------------------------------------------------------*
 * DELAY_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Delay_Printf(char *op, char *operands, ...)
{
  va_list arg_ptr;

  if (delay_op)
    Inst_Out(delay_op, delay_operands);

  va_start(arg_ptr, operands);

  delay_op = op;
  vsprintf(delay_operands, operands, arg_ptr);
}
