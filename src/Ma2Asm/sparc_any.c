/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : sparc_any.c                                                     *
 * Descr.: translation file for SunOs/Solaris on sparc                     *
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
#include <string.h>
#include <stdarg.h>




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              ".LC"

#ifdef M_sparc_sunos

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

char asm_reg_bank[20];
char asm_reg_e[20];
char asm_reg_b[20];
char asm_reg_cp[20];

int w_label = 0;

char *delay_op;
char delay_operands[1024];




	  /* variables for ma_parser.c / ma2asm.c */
int can_produce_pic_code = 0;
#if 0
char *comment_prefix = "#";  /* does not work on solaris 9 */
#else
char *comment_prefix = "!";
#endif
char *local_symb_prefix = "L";
int strings_need_null = 1;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Delay_Printf(char *op, char *operands, ...);



#define LITTLE_INT(int_val)     ((unsigned) ((int_val)+4096) < 8192)




/*-------------------------------------------------------------------------*
 * SOURCE_LINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Source_Line(int line_no, char *cmt)
{
  Label_Printf("\t! %6d: %s", line_no, cmt);
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
Code_Start(char *label, int prolog, int global)
{
  Label_Printf("");
  Inst_Printf(".align", "4");
#if defined(M_sparc_solaris) || defined(M_sparc_bsd)
  Inst_Printf(".type", UN "%s,#function", label);
#endif
  Inst_Printf(".proc", "020");

  if (global)
    Inst_Printf(".global", UN "%s", label);

  Label(label);

  if (!prolog)
    Inst_Printf("save", "%%sp,-104,%%sp");
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
  Inst_Printf("ld", "[%s+%d],%s", asm_reg_bank, MAP_OFFSET_E, asm_reg_e);
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
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  Inst_Printf("sethi", "%%hi(.Lcont%d-8),%%g1", w_label);
  Inst_Printf("or", "%%g1,%%lo(.Lcont%d-8),%%g1", w_label);
  Inst_Printf("st", "%%g1,%s", asm_reg_cp);
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
  Inst_Printf("call", UN "%s,0", label);
#ifdef MAP_REG_CP
  Inst_Printf("mov", "%%o7,%s", asm_reg_cp);	/* delay slot */
#else
  Inst_Printf("st", "%%o7,%s", asm_reg_cp);	/* delay slot */
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
  Inst_Printf("ld", "[%s-4],%%o0", asm_reg_b);
#else
  Inst_Printf("ld", "%s,%%o0", asm_reg_b);
  Inst_Printf("ld", "[%%o0-4],%%o0");
#endif

  Inst_Printf("call", "%%o0");
  Inst_Printf("nop", "");	/* delay slot */
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
  Inst_Printf("ld", "%s,%%o0", asm_reg_cp);
  Inst_Printf("jmp", "%%o0+8");
#endif
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("ba", UN "%s", label);
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("ld", "[%s+%d],%%o0", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("ld", "[%s%+d],%%o0", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("st", "%%o0,[%s+%d]", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("st", "%%o0,[%s%+d]", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
	     char **p_inline)
{
  delay_op = NULL;
}



#define MAX_ARGS_IN_REGS 6

#define BEFORE_ARG				\
{						\
  char r[4];					\
						\
  if (offset < MAX_ARGS_IN_REGS)		\
    sprintf(r, "%%o%d", offset);		\
  else						\
    strcpy(r, "%l7");


#define AFTER_ARG					\
  if (offset >= MAX_ARGS_IN_REGS)			\
    Delay_Printf("st","%s,[%%sp+%d]", r,		\
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
    Delay_Printf("mov", "%ld,%s", int_val, r);
  else
    {
      Delay_Printf("sethi", "%%hi(%ld),%s", int_val, r);
      Delay_Printf("or", "%s,%%lo(%ld),%s", r, int_val, r);
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
  int *p = (int *) &dbl_val;

  BEFORE_ARG;

  Delay_Printf("sethi", "%%hi(%d),%s", p[0], r);
  Delay_Printf("or", "%s,%%lo(%d),%s", r, p[0], r);

  AFTER_ARG;

  offset++;

  BEFORE_ARG;

  Delay_Printf("sethi", "%%hi(%d),%s", p[1], r);
  Delay_Printf("or", "%s,%%lo(%d),%s", r, p[1], r);

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

  Delay_Printf("sethi", "%%hi(%s%d),%s", STRING_PREFIX, str_no, r);
  Delay_Printf("or", "%s,%%lo(%s%d),%s", r, STRING_PREFIX, str_no, r);

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

  Delay_Printf("sethi", "%%hi(" UN "%s+%d),%s", name, index * 4, r);
  if (adr_of)
    Delay_Printf("or", "%s,%%lo(" UN "%s+%d),%s", r, name, index * 4, r);
  else
    Delay_Printf("ld", "[%s+%%lo(" UN "%s+%d)],%s", r, name, index * 4, r);

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
    Delay_Printf("add", "%s,%d,%s", asm_reg_bank, index * 4, r);
  else
    Delay_Printf("ld", "[%s+%d],%s", asm_reg_bank, index * 4, r);

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
    Delay_Printf("add", "%s,%+d,%s", asm_reg_e, Y_OFFSET(index), r);
  else
    Delay_Printf("ld", "[%s%+d],%s", asm_reg_e, Y_OFFSET(index), r);

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
    Delay_Printf("add", "%%l2,%d,%s", index * 4, r);
  else
    Delay_Printf("ld", "[%%l2+%d],%s", index * 4, r);

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

      Delay_Printf("add", "%%l3,%d,%s", index * 8, r);

      AFTER_ARG;

      return 1;
    }

  BEFORE_ARG;

  Delay_Printf("ld", "[%%l3+%d],%s", index * 8, r);

  AFTER_ARG;

  offset++;

  BEFORE_ARG;

  Delay_Printf("ld", "[%%l3+%d],%s", index * 8 + 4, r);

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
  Inst_Printf("call", UN "%s", fct_name);
  if (delay_op)
    Inst_Out(delay_op, delay_operands);
  else
    Inst_Printf("nop", "");	/* delay slot */
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
  Inst_Printf("jmp", "%%o0");
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("cmp", "%%o0,0");

#if 0
  Inst_Printf("be", UN "fail");
  Inst_Printf("nop", "");	/* delay slot */
#else

  Inst_Printf("be", UN "%s+4", "fail");
#ifdef MAP_REG_B
  Inst_Printf("ld", "[%s-4],%%o0", asm_reg_b);
#else
  Inst_Printf("ld", "%s,%%o0", asm_reg_b);
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
  Inst_Printf("sethi", "%%hi(" UN "%s+%d),%%o1", name, index * 4);
  Inst_Printf("st", "%%o0,[%%o1+%%lo(" UN "%s+%d)]", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("st", "%%o0,[%s+%d]", asm_reg_bank, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("st", "%%o0,[%s%+d]", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("st", "%%o0,[%%l2+%d]", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("std", "%%f0,[%%l3+%d]", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (LITTLE_INT(int_val))
    Inst_Printf("cmp", "%%o0,%ld", int_val);
  else
    {
      Inst_Printf("sethi", "%%hi(%ld),%%o1", int_val);
      Inst_Printf("or", "%%o1,%%lo(%ld),%%o1", int_val);
      Inst_Printf("cmp", "%%o0,%%o1");
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
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("bg", UN "%s", label);
  Inst_Printf("nop", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ret", "");
  Inst_Printf("restore", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
  Inst_Printf(".section", "\".rodata\"");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{
  Inst_Printf(".align", "8");
  Label_Printf("%s%d:", STRING_PREFIX, str_no);
  Inst_Printf(".asciz", "%s", asciiz);
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
#ifdef M_sparc_sunos
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
Dico_Long(char *name, int global, VType vtype, PlLong value)
{

  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
#ifdef M_sparc_sunos
      if (!global)
	Inst_Printf(".reserve", UN "%s,%ld,\"bss\",4", name, value * 4);
      else
	Inst_Printf(".common", UN "%s,%ld,\"bss\"", name, value * 4);
#else
      if (!global)
	Inst_Printf(".local", UN "%s", name);
      Inst_Printf(".common", UN "%s,%ld,4", name, value * 4);
#endif
      break;

    case INITIAL_VALUE:
#if defined(M_sparc_solaris) || defined(M_sparc_bsd)
      Inst_Printf(".type", UN "%s,#object", name);
      Inst_Printf(".size", UN "%s,4", name);
#endif
      if (global)
	Inst_Printf(".global", UN "%s", name);
      Label_Printf(UN "%s:", name);
#ifdef M_sparc_sunos
      Inst_Printf(".word", "%ld", value);
#else
      Inst_Printf(".uaword", "%ld", value);
#endif
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
