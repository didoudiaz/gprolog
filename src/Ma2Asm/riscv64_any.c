/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : riscv64_any.c                                                   *
 * Descr.: translation file RISC-V                                         *
 * Author: Jasper Taylor, Alexander Diemand, Daniel Diaz                   *
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

/* Supported arch: RISC-V 64 bits on GNU/Linux
 *
 *  Integer Register Convention
 *  Register ABI name Meaning                                Saver
 *     x0     zero    Hard-wired zero                        n/a
 *     x1      ra     Return address                         Caller
 *     x2      sp     Stack pointer                          Callee
 *     x3      gp     Global pointer                         n/a
 *     x4      tp     Thread pointer                         n/a
 *   x5-x7    t0-t2   Temporary registers                    Caller
 *     x8     s0/fp   Callee-saved register / Frame pointer  Callee
 *     x9      s1     Callee-saved register                  Callee
 *  x10-x11  a0-a1    Function arguments / Return value      Caller
 *  x12-x17  a2-a7    Function arguments                     Caller
 *  x18-x27  s2-s11   Callee-saved registers                 Callee
 *  x28-x31  t3-t6    Temporary registers                    Caller
 *
 *
 *  Floating-point Register Convention
 *  Register ABI name Meaning                                Saver
 *   f0-f7   ft0-ft7  Temporary registers                    Caller
 *   f8-f9   fs0-fs1  Callee-saved registers                 Callee
 *  f10-f11  fa0-fa1  Function arguments / return value      Caller
 *  f12-f17  fa2-fa7  Function arguments                     Caller
 *  f18-f27  fs2-fs11 Callee-saved registers                 Callee
 *  f28-f31  ft8-ft11 Temporary registers                    Caller
 */




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define BPW                        8
#define MAX_ARGS_IN_REGS           8
#define MAX_ARGS_DOUBLE_IN_REGS    8

#define MAX_C_ARGS_IN_C_CODE       32

#define SCRATCH "t1"
#define XFER    "t2"

/* NB: +8 to save x30, Round_Up_Pow2 since sp must be 16-bytes aligned */
#define RESERVED_STACK_SPACE       Round_Up_Pow2((MAX_C_ARGS_IN_C_CODE - MAX_AR\
GS_IN_REGS) * BPW + 8, 16)


/* Round up x to p where x is positive and p is a power of 2 */
#define Round_Up_Pow2(x, p)  (((x) + ((p) - 1)) & (-(p)))



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_bank[20];		/* enough big sizes to avoid compiler warnings */

int dbl_args_so_far;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/





/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = FALSE;
  mi.can_produce_pic_code = FALSE;
  mi.comment_prefix = "#";
  mi.local_symb_prefix = ".L";
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = ".LD";
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
  strcpy(asm_reg_bank, MAP_REG_BANK);
#else
  strcpy(asm_reg_bank, "t4");
#endif
  
  Inst_Printf("#.set", "noat");
  Inst_Printf("#.set", "noreorder");	/* let the assembler reorder instructions */

  Inst_Printf(".section", ".text");
}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
}



void Load_Reg_Bank() {
  #ifndef MAP_REG_BANK
  Inst_Printf("lla", "%s, pl_reg_bank", asm_reg_bank);
  #endif
}

int Get_Mash(int val) {
    int mash = val & 0xfff;
    if (mash & 0x800) mash -= 0x1000;
    return mash;
}

void Offset_Load(char* reg,  char* base, int off) {
  int mash = Get_Mash(off);
  if (off != mash) {
    Inst_Printf("li", "%s, %d", reg, off-mash);
    Inst_Printf("add", "%s, %s, %s", reg, reg, base);	/* asm_reg_bank */
    Inst_Printf("ld", "%s, %d(%s)", reg, mash, reg);	/* asm_reg_bank */
  } else {
    Inst_Printf("ld", "%s, %d(%s)", reg, off, base);	/* asm_reg_bank */
  }
}

void Offset_LdAddr(char* reg,  char* base, int off) {
  int mash = Get_Mash(off);
  if (off != mash) {
    Inst_Printf("li", "%s, %d", reg, off-mash);
    Inst_Printf("add", "%s, %s, %s", reg, reg, base);	/* asm_reg_bank */
    Inst_Printf("la", "%s, %d(%s)", reg, mash, reg);	/* asm_reg_bank */
  } else {
    Inst_Printf("addi", "%s, %s, %d", reg, base, off);	/* asm_reg_bank */
  }
}

void Offset_Store(char* reg,  char* base, int off) {
  int mash = Get_Mash(off);
  if (off != mash) {
    Inst_Printf("li", "%s, %d", SCRATCH, off-mash);
    Inst_Printf("add", "%s, %s, %s", SCRATCH, SCRATCH, base);	/* asm_reg_bank */
    Inst_Printf("sd", "%s, %d(%s)", reg, mash, SCRATCH);	/* asm_reg_bank */
  } else {
    Inst_Printf("sd", "%s, %d(%s)", reg, off, base);	/* asm_reg_bank */
  }
}

/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(CodeInf *c)
{
  Inst_Printf(".text", "%s", "");
  Inst_Printf(".align", "3");
  Inst_Printf(".type", "%s, @function", c->name);
  if (c->global)
    Inst_Printf(".globl", "%s", c->name);

  Label(c->name);

  if (c->prolog)
    {
      /* prolog code does not need any stack space */
    }
  else
    {
      /* for c code we need to save some registers */
      Inst_Printf("sd", "ra, -8(sp)");
      Inst_Printf("addi", "sp, sp, -%d", RESERVED_STACK_SPACE);
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
  Inst_Printf("tail", "%s", label);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("lla", "%s, %s", MAP_REG_CP, Label_Cont_New());
#else
  Inst_Printf("lla", XFER ", %s", Label_Cont_New());
  Load_Reg_Bank();
  Offset_Store(XFER, asm_reg_bank, MAP_OFFSET_CP);
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
  Inst_Printf("ld", "ra, -8(%s)", MAP_REG_B);
#else
  Load_Reg_Bank();
  Offset_Load(SCRATCH, asm_reg_bank, MAP_OFFSET_B);
  Inst_Printf("ld", "ra, -8(%s)", SCRATCH);
#endif
  Inst_Printf("jr", "ra");
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
  Inst_Printf(".align", "3");
  Inst_Printf("# nop", "%s", "");	/* I don't really know why, but it helps ;-) */
#ifdef MAP_REG_CP
  Inst_Printf("move", "ra, %s", MAP_REG_CP);
#else
  Load_Reg_Bank();
  Offset_Load("ra", asm_reg_bank, MAP_OFFSET_CP);
#endif
  Inst_Printf("jr", "ra");
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("j", "%s", label);
}

/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Load_Reg_Bank();
  Offset_Load(XFER, asm_reg_bank, BPW*index);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Offset_Load(XFER, MAP_REG_E, Y_OFFSET(index));
#else
  Load_Reg_Bank();
  Offset_Load(SCRATCH, asm_reg_bank, MAP_OFFSET_E);
  Offset_Load(XFER, SCRATCH, Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Load_Reg_Bank();
  Offset_Store(XFER, asm_reg_bank, BPW*index);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
#ifdef MAP_REG_E
  Offset_Store(XFER, MAP_REG_E, Y_OFFSET(index));
#else
  Load_Reg_Bank();
  Offset_Load("t3", asm_reg_bank, MAP_OFFSET_E);
  Offset_Store(XFER, "t3", Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  dbl_args_so_far = 0;
}

#define AFTER_ARG_DOUBLE if (dbl_args_so_far < MAX_ARGS_DOUBLE_IN_REGS) ++dbl_args_so_far;


/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  int mash = Get_Mash(int_val);
  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS) {
    if (int_val == mash)
      Inst_Printf("li", "a%d, %ld", offset, int_val);
    else {
      Inst_Printf("li", "a%d, %ld", offset, int_val-mash);
      Inst_Printf("addi", "a%d, a%d, %d", offset, offset, mash);
    }
  } else {
    if (int_val == mash)
      Inst_Printf("li", XFER ", %ld", int_val);
    else {
      Inst_Printf("li", XFER ", %ld", int_val-mash);
      Inst_Printf("addi", XFER ", " XFER ", %d", mash);
    }
    Inst_Printf("sd", XFER ", %d(sp)", (offset - MAX_ARGS_IN_REGS) * 8);
  }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_DOUBLE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Double(int offset, DoubleInf *d)
{
  if (dbl_args_so_far < MAX_ARGS_DOUBLE_IN_REGS)
    Inst_Printf("fld", "fa%d, %s, " XFER, dbl_args_so_far, d->symb);
  else {
    offset = offset - MAX_ARGS_DOUBLE_IN_REGS;
    // JAT: once the fa regs are full, floats mix with other args --
    // offset should reflect this! Maybe just save args and do all at once?
    if (offset < MAX_ARGS_IN_REGS)
      Inst_Printf("ld", "a%d, %s", offset, d->symb);
    else {
      Inst_Printf("fld", "f" SCRATCH ", %s, " XFER, d->symb);
      Inst_Printf("fsd", "f" SCRATCH ", %d(sp)", (offset - MAX_ARGS_IN_REGS) * 8);
    }
  }
  AFTER_ARG_DOUBLE
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, StringInf *s)
{
  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS)
    Inst_Printf("lla", "a%d, %s", offset, s->symb);
  else {
    Inst_Printf("lla", XFER ", %s", s->symb);
    Inst_Printf("sd", XFER ", %d(sp)", (offset - MAX_ARGS_IN_REGS) * 8);
  }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_MEM_L                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Mem_L(int offset, Bool adr_of, char *name, int index)
{
  char dest[20];

  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS)
    sprintf(dest, "a%d", offset);
  else
    strcpy(dest, XFER);

  if (!adr_of)
    {
      Inst_Printf("lla", XFER ", %s", name);
      Offset_Load(dest, XFER, index * BPW);
    }
  else
    {
      Inst_Printf("lla", "%s, %s+%d", dest, name, index * BPW);
    }
  if (offset >= MAX_ARGS_IN_REGS)
    {
      Offset_Store(dest, XFER, (offset - MAX_ARGS_IN_REGS) * BPW);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_X                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_X(int offset, Bool adr_of, int index)
{
  char dest[20];

  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS)
    sprintf(dest, "a%d", offset);
  else
    strcpy(dest, XFER);

  Load_Reg_Bank();
  if (!adr_of)
    {
      Offset_Load(dest, asm_reg_bank, index * BPW);
    }
  else
    {
      if (index == 0)
	{
	  Inst_Printf("move", "%s, %s", dest, asm_reg_bank);
	}
      else
	{
	  Offset_LdAddr(dest, asm_reg_bank, index * BPW);
	}
    }
  if (offset >= MAX_ARGS_IN_REGS)
    {
      Offset_Store(dest, "sp", (offset - MAX_ARGS_IN_REGS) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Reg_Y(int offset, Bool adr_of, int index)
{
  char dest[20];

  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS)
    sprintf(dest, "a%d", offset);
  else
    strcpy(dest, XFER);

  if (!adr_of)
    {
#ifdef MAP_REG_E
      Offset_Load(dest, MAP_REG_E, Y_OFFSET(index));
#else
      Load_Reg_Bank();
      Offset_Load(SCRATCH, asm_reg_bank, MAP_OFFSET_E);
      Offset_Load(dest, SCRATCH, Y_OFFSET(index));
#endif
    }
  else
    {
#ifdef MAP_REG_E
      Offset_LdAddr(dest, MAP_REG_E, Y_OFFSET(index));
#else
      Load_Reg_Bank();
      Offset_Load(SCRATCH, asm_reg_bank, MAP_OFFSET_E);
      Offset_LdAddr(dest, SCRATCH, Y_OFFSET(index));
#endif
    }
  if (offset > MAX_ARGS_IN_REGS)
    {
      Offset_Store(dest, "sp", (offset - MAX_ARGS_IN_REGS) * 8);
    }

  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_L                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_L(int offset, Bool adr_of, int index)
{
  char dest[20];

  offset -= dbl_args_so_far;
  if (offset < MAX_ARGS_IN_REGS)
    sprintf(dest, "a%d", offset);
  else
    strcpy(dest, XFER);

  Inst_Printf("lla", SCRATCH ", pl_foreign_long");
  if (!adr_of)
    {
      Offset_Load(dest, SCRATCH, index * BPW);
    }
  else
    {
      Offset_LdAddr(dest, SCRATCH, index * BPW);
    }
  if (offset > MAX_ARGS_IN_REGS)
    {
      Offset_Store(dest, "sp", (offset - MAX_ARGS_IN_REGS) * 8);
    }
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_FOREIGN_D                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Foreign_D(int offset, Bool adr_of, int index)
{
  char dest[20];

  if (adr_of)
    return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_double", index);

  if (dbl_args_so_far < MAX_ARGS_DOUBLE_IN_REGS)
    sprintf(dest, "fa%d", dbl_args_so_far);
  else {
    offset -= MAX_ARGS_DOUBLE_IN_REGS;
    if (offset < MAX_ARGS_IN_REGS)
      sprintf(dest, "a%d", offset);
    else
      strcpy(dest, XFER);
  }
  Inst_Printf("lla", SCRATCH ", pl_foreign_double");
  Inst_Printf("fld", "%s, %d(" SCRATCH ")", dest, index * 8);
  if (offset >= MAX_ARGS_IN_REGS)
    Inst_Printf("sd", "%s, %d(sp)", dest, (offset - MAX_ARGS_IN_REGS)*8);
  AFTER_ARG_DOUBLE
  return 1;
}




/*-------------------------------------------------------------------------*
 * CALL_C_INVOKE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Invoke(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  Inst_Printf("call", "%s@plt", fct_name);
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
  Inst_Printf("jr", "a0");
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("bne", "a0, x0, %s", Label_Cont_New());
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
  Inst_Printf("lla", XFER ", %s", name);
  Offset_Store("a0", XFER, BPW * index);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Load_Reg_Bank();
  Offset_Store("a0", asm_reg_bank, index * BPW);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
#ifdef MAP_REG_E
  Offset_Store("a0", MAP_REG_E, Y_OFFSET(index));
#else
  Load_Reg_Bank();
  Offset_Load(XFER, asm_reg_bank, MAP_OFFSET_E);
  Offset_Store("a0", XFER, Y_OFFSET(index));
#endif
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Move_Ret_To_Mem_L("pl_foreign_long", index);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Inst_Printf("lla", SCRATCH ", pl_foreign_double");
  Inst_Printf("fsd", "fa0, %d(" SCRATCH ")", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  int mash = Get_Mash(int_val);
  if (mash == int_val)
    Inst_Printf("li", SCRATCH ", %d", mash);
  else {
    Inst_Printf("li", SCRATCH ", %ld", int_val-mash);
    Inst_Printf("addi", SCRATCH ", " SCRATCH ", %d", mash);
  }    
  //  Inst_Printf("li", "$24, %ld", int_val);
  //  Inst_Printf("sub", "$12, $2, $24");	/* $2 - $24 -> $12 */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", SCRATCH ", a0, %s", label);	/* $2 == 0 */
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
  Inst_Printf("blt", SCRATCH ", a0, %s", label);	/* $3 == 1 */
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("addi", "sp, sp, %d", RESERVED_STACK_SPACE);
  Inst_Printf("ld", "ra, -8(sp)");
  Inst_Printf("ret", "%s", "");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
  Inst_Printf(".section", ".rodata");
  Inst_Printf(".align", "3");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(StringInf *s)
{

  Label_Printf("%s:", s->symb);
  Inst_Printf(".ascii", "%s", s->str);
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
  Inst_Printf(".section", ".rodata");
}


/*-------------------------------------------------------------------------*
 * DICO_DOUBLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double(DoubleInf *d)
{
  Inst_Printf(".align 3", "%s", "");
  Label_Printf("%s:", d->symb);
  Inst_Printf(".double", "%1.17g", d->v.dbl);
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
  Inst_Printf(".section", ".sdata");
  Inst_Printf(".align", "3");
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
      Inst_Printf(".section", ".bss");
      if (!l->global)
	{
	  Label_Printf("%s:", l->name);
	  Inst_Printf(".align", "3");
	  Inst_Printf(".space", "%ld", l->value * BPW);
	  /* Inst_Printf(".popsection", "%s", ""); */
	}
      else
	{
	  Inst_Printf(".comm", "%s,%ld", l->name, l->value * BPW);
	}
      break;

    case INITIAL_VALUE:
      Inst_Printf(".section", ".rodata");
      if (l->global)
	{
	  Inst_Printf(".globl", "%s", l->name);
	  Inst_Printf(".align", "3");
	  Inst_Printf(".size", "%s,%d", l->name, BPW);
	  Label_Printf("%s:", l->name);
	  Inst_Printf(".word", "%ld", l->value);
	}
      else
	{
	  Inst_Printf(".align", "3");
	  Inst_Printf(".size", "%s,%d", l->name, BPW);
	  Label_Printf("%s:", l->name);
	  Inst_Printf(".word", "%ld", l->value);
	}
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

  Inst_Printf(".section", ".ctors,\"aw\",@progbits");
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
