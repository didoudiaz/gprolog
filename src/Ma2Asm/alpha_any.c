/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : alpha_any.c                                                     *
 * Descr.: translation file for alpha-based architectures                  *
 * Author: Alexander Diemand, Daniel Diaz                                  *
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


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>


/* Supported arch: alpha (64 bits) on Linux, OSF1
 */




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              "$LC"

#define MAX_C_ARGS_IN_C_CODE       32




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[10];
char asm_reg_e[10];
char asm_reg_b[10];
char asm_reg_cp[10];

int dbl_label = 0;

char dbl_arg_buffer[1024 * 1024] = "\0";	/* a temp buffer for the double arguments */

char act_routine[512] = "\0";	/* remembers the actual routine we are building */

	  /* variables for ma_parser.c / ma2asm.c */

int can_produce_pic_code = 0;
char *comment_prefix = "#";
char *local_symb_prefix = "$";
int strings_need_null = 1;
int call_c_reverse_args = 0;

char *inline_asm_data[] = { NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


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
  strcpy(asm_reg_bank, "$9");
#endif
  Inst_Printf("# asm_reg_bank ", asm_reg_bank);

#ifdef MAP_REG_E
  sprintf(asm_reg_e, "%s", MAP_REG_E);
#else
  sprintf(asm_reg_e, "%d(%s)", MAP_OFFSET_E, asm_reg_bank);
#endif
  Inst_Printf("# REG_E ", asm_reg_e);

#ifdef MAP_REG_B
  sprintf(asm_reg_b, "%s", MAP_REG_B);
#else
  sprintf(asm_reg_b, "%d(%s)", MAP_OFFSET_B, asm_reg_bank);
#endif
  Inst_Printf("# REG_B ", asm_reg_b);

#ifdef MAP_REG_CP
  sprintf(asm_reg_cp, "%s", MAP_REG_CP);
#else
  sprintf(asm_reg_cp, "%d(%s)", MAP_OFFSET_CP, asm_reg_bank);
#endif
  Inst_Printf("# REG_CP ", asm_reg_cp);

  Inst_Printf(".set", "noat");
  Inst_Printf(".set", "noreorder");

  Label_Printf(".text");

}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
  /* we are printing the fixed doubles at the end of the file,
   * they will appear in the data section */
  if (dbl_arg_buffer[0] != '\0')
    {
#ifdef M_alpha_linux
      Label_Printf(".section\t.rodata");
#else
      Label_Printf(".rdata");
#endif
      Label_Printf(dbl_arg_buffer);
      dbl_arg_buffer[0] = '\0';
    }
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{

  if (act_routine[0] != '\0')
    Code_Stop();		/* we first have to close the previous code */

  Inst_Printf(".align", "5");
  if (global)
    Inst_Printf(".globl", "%s", label);
  Inst_Printf(".ent", "%s", label);

  Label(label);

  /* remember this label */
  strcpy(act_routine, label);

  if (prolog)
    {
      /* prolog code does not need any stack space */
      Inst_Printf(".frame", "$30,0,$26,0");
      Inst_Printf(".mask", "0x4000000,0");
      Inst_Printf("ldgp", "$gp,0($27)");
      Inst_Printf(".prologue", "1");
    }
  else
    {
      /* for C code we need to save some registers */
      Inst_Printf(".frame", "$30,32,$26,0");
      Inst_Printf(".mask", "0x4008000,-32");
      Inst_Printf("ldgp", "$gp,0($27)");
      Inst_Printf("subq", "$30,32,$30");
      Inst_Printf("stq", "$26,0($30)");
      Inst_Printf("stq", "$15,8($30)");
      Inst_Printf(".prologue", "1");
    }
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(void)
{
  Inst_Printf(".end", "%s", act_routine);

  act_routine[0] = '\0';
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
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
#ifdef M_alpha_linux		/* also works for OSF but 'as' warns */
  Inst_Printf("jmp", "$31,%s", label);	/* about macro using $at */
#else
  Inst_Printf("lda", "$27,%s", label);
  Inst_Printf("jmp", "$31,($27),%s", label);
#endif
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("lda", "%s,%s", asm_reg_cp, Label_Cont_New());	/* CP = $Lcont%d */
#else
  Inst_Printf("lda", "$4,%s", Label_Cont_New());	/* CP = $Lcont%d */
  Inst_Printf("stq", "$4,%s", asm_reg_cp);
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

  Inst_Printf("ldgp","$gp,0($27)");
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
  Inst_Printf("ldq", "$27,-8(%s)", asm_reg_b);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_b);
  Inst_Printf("ldq", "$27,-8($4)");
#endif

  Inst_Printf("jmp", "$31,($27),0");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("mov", "%s,$27", asm_reg_cp);	/* make a copy of it in $27 */
#else
  Inst_Printf("ldq", "$27,%s", asm_reg_cp);
#endif
  Inst_Printf("jmp", "$31,($27),0");	/* jump to CP */
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("lda", "$3,%s", label);
  Inst_Printf("jmp", "$31,($3),%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("ldq", "$1,%d(%s)", 8 * index, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("ldq", "$1,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("ldq", "$1,%d($4)", Y_OFFSET(index));
#endif

}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("stq", "$1,%d(%s)", 8 * index, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Inst_Printf("stq", "$1,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("stq", "$1,%d($4)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, int nb_args_in_words,
	     char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("lda", "$16,%ld", int_val);
      break;
    case 1:
      Inst_Printf("lda", "$17,%ld", int_val);
      break;
    case 2:
      Inst_Printf("lda", "$18,%ld", int_val);
      break;
    case 3:
      Inst_Printf("lda", "$19,%ld", int_val);
      break;
    case 4:
      Inst_Printf("lda", "$20,%ld", int_val);
      break;
    case 5:
      Inst_Printf("lda", "$21,%ld", int_val);
      break;
    default:
      Inst_Printf("lda", "$1,%ld", int_val);
      Inst_Printf("stq", "$1,%d($30)", (offset - 6) * 8);
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
  char buf[1024];

  sprintf(buf, "\t.align 3\n$LD%d:\n\t.t_floating %1.20e\n", dbl_label, dbl_val);
  strcat(dbl_arg_buffer, buf);
  Inst_Printf("lda", "$1,$LD%d", dbl_label++);
  switch (offset)
    {
    case 0:
      Inst_Printf("ldt", "$f16,0($1)");
      break;
    case 1:
      Inst_Printf("ldt", "$f17,0($1)");
      break;
    case 2:
      Inst_Printf("ldt", "$f18,0($1)");
      break;
    case 3:
      Inst_Printf("ldt", "$f19,0($1)");
      break;
    case 4:
      Inst_Printf("ldt", "$f20,0($1)");
      break;
    case 5:
      Inst_Printf("ldt", "$f21,0($1)");
      break;
    default:
      Inst_Printf("ldt", "$f1,0($1)");
      Inst_Printf("stt", "$f1,%d($30)", (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  switch (offset)
    {
    case 0:
      Inst_Printf("lda", "$16,%s%d", STRING_PREFIX, str_no);
      break;
    case 1:
      Inst_Printf("lda", "$17,%s%d", STRING_PREFIX, str_no);
      break;
    case 2:
      Inst_Printf("lda", "$18,%s%d", STRING_PREFIX, str_no);
      break;
    case 3:
      Inst_Printf("lda", "$19,%s%d", STRING_PREFIX, str_no);
      break;
    case 4:
      Inst_Printf("lda", "$20,%s%d", STRING_PREFIX, str_no);
      break;
    case 5:
      Inst_Printf("lda", "$21,%s%d", STRING_PREFIX, str_no);
      break;
    default:
      Inst_Printf("lda", "$1,%s%d", STRING_PREFIX, str_no);
      Inst_Printf("stq", "$1,%d($30)", (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("lda", "$2,%s", name);
      Inst_Printf("ldq", "%s,%d($2)", dest, index * 8);
    }
  else
    {
      Inst_Printf("lda", "%s,%s+%d", dest, name, index * 8);
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
      Inst_Printf("ldq", "%s,%d(%s)", dest, index * 8, asm_reg_bank);
    }
  else
    {
      if (index == 0)
	{
	  Inst_Printf("mov", "%s,%s", asm_reg_bank, dest);
	}
      else
	{
	  Inst_Printf("lda", "%s,%d(%s)", dest, index * 8, asm_reg_bank);
	}
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  if (!adr_of)
    {
#ifdef MAP_REG_E
      Inst_Printf("ldq", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("ldq", "$4,%s", asm_reg_e);
      Inst_Printf("ldq", "%s,%d($4)", dest, Y_OFFSET(index));
#endif
    }
  else
    {
#ifdef MAP_REG_E
      Inst_Printf("lda", "%s,%d(%s)", dest, Y_OFFSET(index), asm_reg_e);
#else
      Inst_Printf("ldq", "$4,%s", asm_reg_e);
      Inst_Printf("lda", "%s,%d($4)", dest, Y_OFFSET(index));
#endif
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, int adr_of, int index)
{
  char dest[8];

  switch (offset)
    {
    case 0:
      sprintf(dest, "%s", "$16");
      break;
    case 1:
      sprintf(dest, "%s", "$17");
      break;
    case 2:
      sprintf(dest, "%s", "$18");
      break;
    case 3:
      sprintf(dest, "%s", "$19");
      break;
    case 4:
      sprintf(dest, "%s", "$20");
      break;
    case 5:
      sprintf(dest, "%s", "$21");
      break;
    default:
      sprintf(dest, "%s", "$1");
      break;
    }

  Inst_Printf("lda", "$2,pl_foreign_long");
  if (!adr_of)
    {
      Inst_Printf("ldq", "%s,%d($2)", dest, index * 8);
    }
  else
    {
      Inst_Printf("lda", "%s,%d($2)", dest, index * 8);
    }
  if (offset > 5)
    {
      Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, int adr_of, int index)
{
  char dest[8];

  if (adr_of)
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$16");
	  break;
	case 1:
	  sprintf(dest, "%s", "$17");
	  break;
	case 2:
	  sprintf(dest, "%s", "$18");
	  break;
	case 3:
	  sprintf(dest, "%s", "$19");
	  break;
	case 4:
	  sprintf(dest, "%s", "$20");
	  break;
	case 5:
	  sprintf(dest, "%s", "$21");
	  break;
	default:
	  sprintf(dest, "%s", "$1");
	  break;
	}
      Inst_Printf("lda", "%s,pl_foreign_double+%d", dest, index * 8);
      if (offset > 5)
	{
	  Inst_Printf("stq", "%s,%d($30)", dest, (offset - 6) * 8);
	}
      return 1;
    }
  else
    {
      switch (offset)
	{
	case 0:
	  sprintf(dest, "%s", "$f16");
	  break;
	case 1:
	  sprintf(dest, "%s", "$f17");
	  break;
	case 2:
	  sprintf(dest, "%s", "$f18");
	  break;
	case 3:
	  sprintf(dest, "%s", "$f19");
	  break;
	case 4:
	  sprintf(dest, "%s", "$f20");
	  break;
	case 5:
	  sprintf(dest, "%s", "$f21");
	  break;
	default:
	  sprintf(dest, "%s", "$f1");
	  break;
	}
      Inst_Printf("lda", "$1,pl_foreign_double+%d", index * 8);
      Inst_Printf("ldt", "%s,0($1)", dest);
      if (offset > 5)
	{
	  Inst_Printf("stt", "%s,%d($30)", dest, (offset - 6) * 8);
	}
      return 1;
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int fc, int nb_args, int nb_args_in_words)
{
  Inst_Printf("jsr", "$26,%s", fct_name);
  Inst_Printf("ldgp", "$gp,0($26)");
}




/*-------------------------------------------------------------------------*
 * CALL_C_STOP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Stop(char *fct_name, int nb_args, char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * JUMP_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_Ret(void)
{
  Inst_Printf("mov", "$0,$27");
  Inst_Printf("jmp", "$31,($27),0");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("bne", "$0,%s", Label_Cont_New());
  Pl_Fail();
  Label_Printf("%s:", Label_Cont_Get());
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  Inst_Printf("lda", "$1,%s", name);
  if (index * 8 > 1 << 15)
    {
      Inst_Printf("lda", "$2,%d", index * 8);
      Inst_Printf("addq", "$1,$2,$1");
      index = 0;
    }
  Inst_Printf("stq", "$0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("stq", "$0,%d(%s)", index * 8, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
#ifdef MAP_REG_E
  Inst_Printf("stq", "$0,%d(%s)", Y_OFFSET(index), asm_reg_e);
#else
  Inst_Printf("ldq", "$4,%s", asm_reg_e);
  Inst_Printf("stq", "$0,%d($4)", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Inst_Printf("lda", "$1,pl_foreign_long");
  Inst_Printf("stq", "$0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("lda", "$1,pl_foreign_double");
  Inst_Printf("stt", "$f0,%d($1)", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  Inst_Printf("lda", "$1,%ld", int_val);
  Inst_Printf("subq", "$0,$1,$1");
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", "$1,%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  /* this is based on the comparison we did with Cmp_Ret_And_Int */
  /* means this is more or less a Jump_If_Not_Equal ! */
  Inst_Printf("bgt", "$1,%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ldq", "$26,0($30)");
  Inst_Printf("addq", "$30,32,$30");
  Inst_Printf("ret", "$31,($26),1");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
#ifdef M_alpha_linux
  Label_Printf(".section\t.rodata");
#else
  Label_Printf(".rdata");
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
  Inst_Printf(".ascii", "%s", asciiz);
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
#ifdef M_alpha_linux
  Label_Printf(".section\t.sdata,\"aw\"");
#else
  Label_Printf(".data");
#endif
  Inst_Printf(".align", "3");
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
      Inst_Printf(".align", "3");
#ifdef M_alpha_linux
      Label_Printf(".section\t.bss");
#endif
      if (!global)
	{
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,%ld", name, value * 8);
	  Inst_Printf(".align", "3");
	  Label_Printf("%s:", name);
	  Inst_Printf(".zero", "%ld", value * 8);
#else
	  Inst_Printf(".lcomm", "%s,%ld", name, value * 8);
#endif
	}
      else
	{
#ifdef M_alpha_linux
	  Inst_Printf(".comm", "%s,%ld,8", name, value * 8);
#else
	  Inst_Printf(".comm", "%s,%ld", name, value * 8);
#endif
	}
      break;

    case INITIAL_VALUE:
#ifdef M_alpha_linux
      Label_Printf(".section\t.sdata,\"aw\"");
#endif
      if (global)
	{
	  Inst_Printf(".globl", "%s", name);
	  Inst_Printf(".align", "3");
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,8", name);
#endif
	  Label_Printf("%s:", name);
	  Inst_Printf(".quad", "%ld", value);
	}
      else
	{
	  Inst_Printf(".align", "3");
#ifdef M_alpha_linux
	  Inst_Printf(".type", "%s,@object", name);
	  Inst_Printf(".size", "%s,8", name);
#endif
	  Label_Printf("%s:", name);
	  Inst_Printf(".quad", "%ld", value);
	}
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
				/* last routine has to be closed first */
  if (act_routine[0] != '\0')
    {
      Inst_Printf("ret", "$31,($26),1");
      Inst_Printf(".end", "%s", act_routine);

      act_routine[0] = '\0';
    }

  if (initializer_fct == NULL)
    return;

  Inst_Printf(".section", ".ctors,\"aw\"");
  Inst_Printf(".quad", "%s", initializer_fct);
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
}
