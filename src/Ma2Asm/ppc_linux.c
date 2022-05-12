/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ppc_linux.c                                                     *
 * Descr.: translation file for Linux on PowerPC                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2022 Daniel Diaz                                     *
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

#include <stdio.h>
#include <string.h>




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

double dbl_tbl[MAX_DOUBLES_IN_PRED];
int nb_dbl = 0;
int dbl_lc_no = 0;
int dbl_reg_no;

char asm_reg_bank[16];
char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;

	  /* variables for ma_parser.c / ma2asm.c */

char *comment_prefix = "#";
char *local_symb_prefix = ".L";
int strings_need_null = 0;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define LITTLE_INT(int_val)     ((unsigned) ((int_val)+32768) < 65536)




/*-------------------------------------------------------------------------*
 * ASM_START                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Start(void)
{
#ifdef MAP_REG_BANK
  sprintf(asm_reg_bank, "%s", MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "15");
#endif

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%s", MAP_REG_E);
#else
  strcpy(asm_reg_e, "16");
#endif

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%s", MAP_REG_B);
#else
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%s", MAP_REG_CP);
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
      Label_Printf("%s%d:", DOUBLE_PREFIX, x++);
      Inst_Printf(".double", "0d%1.20e", dbl_tbl[i]);
    }

  nb_dbl = 0;

  Label_Printf("");
  Inst_Printf(".align", "2");
  Inst_Printf(".type", "%s,@function", label);

  if (global)
    Inst_Printf(".globl", "%s", label);

  Label(label);

  if (!prolog)
    {
      Inst_Printf("mr", "12,1");
      Inst_Printf("addi", "1,1,-%d", MAX_C_ARGS_IN_C_CODE * 4);
      Inst_Printf("stw", "12,0(1)");
      Inst_Printf("mflr", "0");
      Inst_Printf("stw", "0,%d(1)", (MAX_C_ARGS_IN_C_CODE + 1) * 4);
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
  Label_Printf("\n%s:", label);
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Inst_Printf("lwz", "%s,%d(%s)", asm_reg_e, MAP_OFFSET_E, asm_reg_bank);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("b", "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("addis", "9,0,.Lcont%d@ha", w_label);
  Inst_Printf("addi", "9,9,.Lcont%d@l", w_label);
  Inst_Printf("stw", "9,%s", asm_reg_cp);
#else
  Inst_Printf("addis", "%s,0,.Lcont%d@ha", asm_reg_cp, w_label);
  Inst_Printf("addi", "%s,%s,.Lcont%d@l", asm_reg_cp, asm_reg_cp, w_label);
#endif
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
  Inst_Printf("lwz", "9,-4(%s)", asm_reg_b);
#else
  Inst_Printf("lwz", "9,%s", asm_reg_b);
  Inst_Printf("lwz", "9,-4(9)");
#endif

  Inst_Printf("mtctr", "9");
  Inst_Printf("bctr", "");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("lwz", "0,%s", asm_reg_cp);
  Inst_Printf("mtctr", "0", asm_reg_b);
#else
  Inst_Printf("mtctr", "%s", asm_reg_cp);
#endif

  Inst_Printf("bctr", "");
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("b", "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("lwz", "0,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("lwz", "0,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("stw", "0,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("stw", "0,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline)
{
  dbl_reg_no = 0;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  if (LITTLE_INT(int_val))
    Inst_Printf("li", "%d,%ld", offset + 3, int_val);
  else
    {
      Inst_Printf("lis", "%ld,%#x", offset + 3, int_val >> 16);
      Inst_Printf("ori", "%d,%ld,%#x", offset + 3, offset + 3,
		  int_val & 0xFFFF);
    }

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, double dbl_val)
{
  dbl_tbl[nb_dbl++] = dbl_val;

  Inst_Printf("addis", "%d,0,%s%d@ha", offset + 3, DOUBLE_PREFIX,
	      dbl_lc_no);
  Inst_Printf("lfd", "%d,%s%d@l(%d)", ++dbl_reg_no, DOUBLE_PREFIX,
	      dbl_lc_no++, offset + 3);

  return 0;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  Inst_Printf("addis", "%d,0,%s%d@ha", offset + 3, STRING_PREFIX, str_no);
  Inst_Printf("addi", "%d,%d,%s%d@l", offset + 3, offset + 3, STRING_PREFIX,
	      str_no);

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  Inst_Printf("addis", "%d,0,%s+%d@ha", offset + 3, name, index * 4);

  if (adr_of)
    Inst_Printf("addi", "%d,%d,%s+%d@l", offset + 3, offset + 3, name,
		index * 4);
  else
    Inst_Printf("lwz", "%d,%s+%d@l(%d)", offset + 3, name, index * 4,
		offset + 3);

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
    Inst_Printf("addi", "%d,%s,%d", offset + 3, asm_reg_bank, index * 4);
  else
    Inst_Printf("lwz", "%d,%d(%s)", offset + 3, index * 4, asm_reg_bank);

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
    Inst_Printf("addi", "%d,%s,%d", offset + 3, asm_reg_e, Y_OFFSET(index));
  else
    Inst_Printf("lwz", "%d,%d(%s)", offset + 3, Y_OFFSET(index), asm_reg_e);

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  Inst_Printf("addis", "%d,0,foreign_long+%d@ha", offset + 3, index * 4);

  if (adr_of)
    Inst_Printf("addi", "%d,%d,foreign_long+%d@l", offset + 3, offset + 3,
		index * 4);
  else
    Inst_Printf("lwz", "%d,foreign_long+%d@l(%d)", offset + 3, index * 4,
		offset + 3);

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  Inst_Printf("addis", "%d,0,foreign_double+%d@ha", offset + 3, index * 4);

  if (adr_of)
    {
      Inst_Printf("addi", "%d,%d,foreign_double+%d@l", offset + 3,
		  offset + 3, index * 8);
      return 1;
    }

  Inst_Printf("lfd", "%d,foreign_double+%d@l(%d)", ++dbl_reg_no, index * 8,
	      offset + 3);

  return 0;

}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int nb_args)
{
#if 0	/* only useful to call varargs functions - not the case here */
  if (dbl_reg_no == 0)
    Inst_Printf("crxor", "6,6,6");
  else
    Inst_Printf("creqv", "6,6,6");
#endif

  Inst_Printf("bl", "%s", fct_name);
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
  Inst_Printf("mtctr", "3");
  Inst_Printf("bctr", "");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("cmpwi", "1,3,0");
  Inst_Printf("bne", "1,.Lcont%d", w_label);
  Inst_Printf("b", "fail");
  Label_Printf(".Lcont%d:", w_label++);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("addis", "4,0,%s+%d@ha", name, index * 4);
  Inst_Printf("stw", "3,%s+%d@l(4)", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* similar to Move_To_Reg_X */
  Inst_Printf("stw", "3,%d(%s)", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* similar to Move_To_Reg_Y */
  Inst_Printf("stw", "3,%d(%s)", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("addis", "4,0,foreign_long+%d@ha", index * 4);
  Inst_Printf("stw", "3,foreign_long+%d@l(4)", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("addis", "4,0,foreign_double+%d@ha", index * 8);
  Inst_Printf("stfd", "1,foreign_double+%d@l(4)", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(long int_val)
{
  if (LITTLE_INT(int_val))
    Inst_Printf("cmpwi", "1,3,%ld", int_val);
  else
    {
      Inst_Printf("lis", "0,%#lx", int_val >> 16);
      Inst_Printf("ori", "0,0,%#lx", int_val & 0xFFFF);
      Inst_Printf("cmpw", "1,3,0");
    }
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", "1,%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("bgt", "1,%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("lwz", "0,%d(1)", (MAX_C_ARGS_IN_C_CODE + 1) * 4);
  Inst_Printf("mtlr", "0");
  Inst_Printf("addi", "1,1,%d", MAX_C_ARGS_IN_C_CODE * 4);
  Inst_Printf("blr", "");
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
      if (!global)
	Inst_Printf(".local", "%s", name);
      Inst_Printf(".comm", "%s,%ld,4", name, value * 4);
      break;

    case INITIAL_VALUE:
      if (global)
	Inst_Printf(".globl", "%s", name);
      Label_Printf("%s:", name);
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
  Label_Printf("obj_chain_start:");

  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_1);
  Inst_Printf(".long", "%d", OBJ_CHAIN_MAGIC_2);
  Inst_Printf(".long", "obj_chain_stop");
  Inst_Printf(".long", "%s", initializer_fct);
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

  Inst_Printf(".long", "obj_chain_start");
}
