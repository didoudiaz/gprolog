/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ix86_win32.c                                                    *
 * Descr.: translation file for Win32 on intel x86 (for nasm)              *
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
#include <ctype.h>

#include "../Wam2Ma/bt_string.h"
			       /* bt_string.c already included by ma2asm.c */



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STRING_PREFIX              "LS_"

#define MAX_C_ARGS_IN_C_CODE       32

#if defined(M_ix86_win32)

#define UN                         "_"

#else

#define UN

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

enum
{
  SECTION_DATA,
  SECTION_BSS,
  SECTION_INIT,
  SECTION_NONE
};




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

BTString bt_symbol;
int section = SECTION_NONE;

char asm_reg_bank[16];
char asm_reg_e[16];
char asm_reg_b[16];
char asm_reg_cp[16];

int w_label = 0;


	  /* variables for ma_parser.c / ma2asm.c */

char *comment_prefix = ";";
char *local_symb_prefix = "L";
int strings_need_null = 0;
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
  strcpy(asm_reg_bank, MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "ebx");
#endif

#ifdef MAP_REG_E
  strcpy(asm_reg_e, MAP_REG_E);
#else
  strcpy(asm_reg_e, "edi");
#endif

#ifdef MAP_REG_B
  strcpy(asm_reg_b, MAP_REG_B);
#else
  sprintf(asm_reg_b, "[%s+%d]", asm_reg_bank, MAP_OFFSET_B);
#endif

#ifdef MAP_REG_CP
  strcpy(asm_reg_cp, MAP_REG_CP);
#else
  sprintf(asm_reg_cp, "[%s+%d]", asm_reg_bank, MAP_OFFSET_CP);
#endif

  BT_String_Init(&bt_symbol);

  Label_Printf("SECTION .text");

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
  void External_Symbol(int str_no, char *asciiz);

  Label_Printf("");
  BT_String_List(&bt_symbol, External_Symbol);
}




/*-------------------------------------------------------------------------*
 * EXTERNAL_SYMBOL                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
External_Symbol(int str_no, char *asciiz)
{
  if (str_no >= 0)
    Inst_Printf("EXTERN", UN "%s", asciiz);
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(char *label, int prolog, int global)
{
  BT_String_Add(&bt_symbol, strdup(label))->no = -1;

  Label_Printf("");

  Inst_Printf("ALIGN", "16");

  if (global)
    Inst_Printf("GLOBAL", UN "%s", label);

  Label(label);

  if (!prolog)
    Inst_Printf("sub", "esp,%d", MAX_C_ARGS_IN_C_CODE * 4);
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
  BT_String_Add(&bt_symbol, strdup(label))->no = -1;
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
  Inst_Printf("mov", "%s,[%d+%s]", asm_reg_e, MAP_OFFSET_E, asm_reg_bank);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  BT_String_Add(&bt_symbol, strdup(label));
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  Inst_Printf("mov", "dword %s,.Lcont%d", asm_reg_cp, w_label);
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
  BT_String_Add(&bt_symbol, strdup(label));

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
  Inst_Printf("mov", "eax,%s", asm_reg_b);
  Inst_Printf("jmp", "[eax-4]");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifndef MAP_REG_CP
  Inst_Printf("jmp", "%s", asm_reg_cp);
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
  BT_String_Add(&bt_symbol, strdup(label));
  Inst_Printf("jmp", UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Inst_Printf("mov", "eax,[%d+%s]", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Inst_Printf("mov", "eax,[%d+%s]", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Inst_Printf("mov", "[%d+%s],eax", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Inst_Printf("mov", "[%d+%s],eax", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline)
{
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, long int_val)
{
  Inst_Printf("mov", "[esp+%d],dword %ld", offset * 4, int_val);

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

  Inst_Printf("mov", "[esp+%d],dword %d", offset * 4, p[0]);
  Inst_Printf("mov", "[esp+%d],dword %d", (offset + 1) * 4, p[1]);

  return 2;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, int str_no)
{
  Inst_Printf("mov", "[esp+%d],dword %s%d", offset * 4, STRING_PREFIX,
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
  BT_String_Add(&bt_symbol, strdup(name));

  if (adr_of)
    Inst_Printf("mov", "[esp+%d],dword " UN "%s+%d", offset * 4, name,
		index * 4);
  else
    {
      Inst_Printf("mov", "eax,[" UN "%s+%d]", name, index * 4);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
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
  if (adr_of)
    {
      if (index == 0)
	Inst_Printf("mov", "[esp+%d],%s", offset * 4, asm_reg_bank);
      else
	{
	  Inst_Printf("lea", "eax,[%d+%s]", index * 4, asm_reg_bank);
	  Inst_Printf("mov", "[esp+%d],eax", offset * 4);
	}
    }
  else
    {
      Inst_Printf("mov", "eax,[%d+%s]", index * 4, asm_reg_bank);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
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
  if (adr_of)
    {
      Inst_Printf("lea", "eax,[%d+%s]", Y_OFFSET(index), asm_reg_e);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
    }
  else
    {
      Inst_Printf("mov", "eax,[%d+%s]", Y_OFFSET(index), asm_reg_e);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
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
  BT_String_Add(&bt_symbol, "foreign_long");

  if (adr_of)
    Inst_Printf("mov", "[esp+%d],dword " UN "foreign_long+%d", offset * 4,
		index * 4);
  else
    {
      Inst_Printf("mov", "eax,[" UN "foreign_long+%d]", index * 4);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
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
  BT_String_Add(&bt_symbol, "foreign_double");

  if (adr_of)
    {
      Inst_Printf("mov", "[esp+%d],dword " UN "foreign_double+%d",
		  offset * 4, index * 8);
      return 1;
    }
  else
    {
      Inst_Printf("mov", "eax,[" UN "foreign_double+%d]", index * 8);
      Inst_Printf("mov", "[esp+%d],eax", offset * 4);
      Inst_Printf("mov", "eax,[" UN "foreign_double+%d]", index * 8 + 4);
      Inst_Printf("mov", "[esp+%d],eax", (offset + 1) * 4);
      return 2;
    }
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, int nb_args)
{
  BT_String_Add(&bt_symbol, strdup(fct_name));

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
  Inst_Printf("jmp", "eax");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("test", "eax,eax");
  Inst_Printf("je", "near " UN "fail");
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  BT_String_Add(&bt_symbol, strdup(name));
  Inst_Printf("mov", "[" UN "%s+%d],eax", name, index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Inst_Printf("mov", "[%d+%s],eax", index * 4, asm_reg_bank);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Inst_Printf("mov", "[%d+%s],eax", Y_OFFSET(index), asm_reg_e);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  BT_String_Add(&bt_symbol, "foreign_long");

  Inst_Printf("mov", "[" UN "foreign_long+%d],eax", index * 4);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  BT_String_Add(&bt_symbol, "foreign_double");

  Inst_Printf("fstp", "qword [" UN "foreign_double+%d]", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(long int_val)
{
  if (int_val == 0)
    Inst_Printf("test", "eax,eax");
  else
    Inst_Printf("cmp", "eax,%ld", int_val);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("je", "near " UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("jg", "near " UN "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("add", "esp,%d", MAX_C_ARGS_IN_C_CODE * 4);
  Inst_Printf("ret", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb_consts)
{
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(int str_no, char *asciiz)
{
  extern FILE *file_out;
  char c;
  char escape_symbol[] = "abfnrtv";
  char escape_char[] = "\a\b\f\n\r\t\v";
  char *p;
  enum
  { NO_PREVIOUS, INSIDE_QUOTE, OUTSIDE_QUOTE }
  prev;

  if (section != SECTION_DATA)
    {
      Label_Printf("\nSECTION .data");
      section = SECTION_DATA;
    }

  fprintf(file_out, "%s%d:\tDB ", STRING_PREFIX, str_no);
  prev = NO_PREVIOUS;
  do
    {
      c = *++asciiz;		/* at first time skips leading " */
      if (c == '"')		/* trailing " becomes 0 */
	c = 0;

      if (c != '\\')
	goto one_char;

      c = *++asciiz;

      if ((p = (char *) strchr(escape_symbol, c)))	/* \a \b \f \n \r \t \v */
	{
	  c = escape_char[p - escape_symbol];
	  goto one_char;
	}

      if (isdigit(c))
	{
	  c =
	    (asciiz[0] - '0') * 8 * 8 + (asciiz[1] - '0') * 8 + asciiz[2] -
	    '0';
	  asciiz += 2;
	  goto one_char;
	}

    one_char:
      if (isprint(c) && c != '\'')
	{
	  if (prev != INSIDE_QUOTE)
	    {
	      if (prev != NO_PREVIOUS)
		fputc(',', file_out);
	      fputc('\'', file_out);
	      prev = INSIDE_QUOTE;
	    }
	  fputc(c, file_out);
	}
      else
	{
	  if (prev == INSIDE_QUOTE)
	    fputc('\'', file_out);
	  if (prev != NO_PREVIOUS)
	    fputc(',', file_out);
	  fprintf(file_out, "%d", (int) (unsigned char) c);
	  prev = OUTSIDE_QUOTE;
	}
    }
  while (c);

  fputc('\n', file_out);
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
}




/*-------------------------------------------------------------------------*
 * DICO_LONG                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Long(char *name, int global, VType vtype, long value)
{
  BT_String_Add(&bt_symbol, strdup(name))->no = -1;

  switch (vtype)
    {
    case NONE:
      value = 1;		/* then in case ARRAY_SIZE */
    case ARRAY_SIZE:
      if (section != SECTION_BSS)
	{
	  Label_Printf("\nSECTION .bss");
	  section = SECTION_BSS;
	}
      if (global)
	Inst_Printf("GLOBAL", UN "%s", name);

      Label_Printf(UN "%s:", name);
      Inst_Printf("RESD", "%ld", value);
      break;

    case INITIAL_VALUE:
      if (section != SECTION_DATA)
	{
	  Label_Printf("\nSECTION .data");
	  section = SECTION_DATA;
	}

      if (global)
	Inst_Printf("GLOBAL", UN "%s", name);
#ifdef M_ix86_cygwin
      Inst_Printf(".align", "4");
#endif
      Label_Printf(UN "%s:", name);
      Inst_Printf("DD", "%ld", value);
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

#ifndef _MSC_VER
  Label_Printf("\nSECTION .data");
  section = SECTION_DATA;

  Inst_Printf("ALIGN", "4");
  Label_Printf(UN "obj_chain_start:");

  Inst_Printf("DD", "%d", OBJ_CHAIN_MAGIC_1);
  Inst_Printf("DD", "%d", OBJ_CHAIN_MAGIC_2);
  Inst_Printf("DD", UN "obj_chain_stop");
  Inst_Printf("DD", UN "%s", initializer_fct);
#else
  Label_Printf("\nSECTION .INIT$m data align=4");
  section = SECTION_INIT;

  Inst_Printf("DD", UN "%s", initializer_fct);
#endif
}




/*-------------------------------------------------------------------------*
 * DATA_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Data_Stop(char *initializer_fct)
{
#ifndef _MSC_VER
  if (initializer_fct == NULL)
    return;

  if (section != SECTION_DATA)
    {
      Label_Printf("\nSECTION .data");
      section = SECTION_DATA;
    }

  Inst_Printf("ALIGN", "4");
  Label_Printf(UN "obj_chain_stop:");

  Inst_Printf("DD", UN "obj_chain_start");
#endif
}
