/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : arm64_any.c                                                     *
 * Descr.: translation file arm 64 bits (aarch64)                          *
 * Author: Jasper Taylor and Daniel Diaz                                   *
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


/* Supported arch: arm 64 bits (aarch64) (e.g. armv8) 
 *                 on Linux, Darwin (MacOS)
 *
 * AArch64 Instruction Set Architecture
 * https://developer.arm.com/documentation/102374/0101
 *
 * ARM A64 instruction set
 * https://developer.arm.com/documentation/dui0801/k/A64-General-Instructions?lang=en
 */


/* pl_reg_bank is normally the first mapped register. If it is not mapped 
 * it is because:
 *
 * 1) no registers are used (either none available or --disable-regs).
 *    In that case it is loaded in a callee-save register by engine1.c
 *    (see ASM_REG_BANK)
 *
 * 2) or because NO_MACHINE_REG_FOR_REG_BANK is defined (debug only ?).
 *    In that case Load_Reg_Bank loads it in a callee-save register. 
 *    But this register must not be already used (mapped), 
 *    so we here check no registers are used at all !
 */

#if defined(NO_MACHINE_REG_FOR_REG_BANK) && NB_USED_MACHINE_REGS > 0
#error NO_MACHINE_REG_FOR_REG_BANK can only be defined if no registers are used at all (use --disable_regs)
#endif

#ifdef MAP_REG_BANK
#define ASM_REG_BANK MAP_REG_BANK
#else
#define ASM_REG_BANK "x20"	/* see engine1.c. If NO_MACHINE_REG_FOR_REG_BANK see Load_Reg_Bank */
#endif


#ifdef NO_MACHINE_REG_FOR_REG_BANK
#define Load_Reg_Bank()   Load_Address(ASM_REG_BANK, "pl_reg_bank")
#else
#define Load_Reg_Bank()
#endif




/* To load an immediate (constant or label) use the pseudo-instruction:
 *    ldr, =immediate or address
 * the assembler replaces by a sequence of mov and movk
 *
 * However, as can also place the constant in a literal pool and generates a 
 * pc-relative ldr instruction that reads the constant from the literal pool.
 * For this reson, the pool must not be too far else we obtain an error, e.g.:
 *    Error: pc-relative load offset out of range
 * We can inform the assembler to force the emission of the pool with 
 *    .ltorg directive
 * We try to emit it ASAP (e.g. after a branching). We also use a mechanisme
 * to force the pool emission when a maximum number of call_c is reached.
 */
#if 1
#define USE_LDR_PSEUDO_OP_AND_POOL
#define MAX_CALL_C_BEFORE_EMIT_POOL 500
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#ifdef M_darwin
#define ASM_DOUBLE_DIRECTIV_PREFIX ""
#else
#define ASM_DOUBLE_DIRECTIV_PREFIX "0d"
#endif


#define BPW                        8
#define MAX_ARGS_IN_REGS           8
#define MAX_ARGS_DOUBLE_IN_REGS    8

#define MAX_C_ARGS_IN_C_CODE       32

/* NB: +8 to save x30, Round_Up_Pow2 since sp must be 16-bytes aligned */
#define RESERVED_STACK_SPACE       Round_Up_Pow2((MAX_C_ARGS_IN_C_CODE - MAX_ARGS_IN_REGS) * BPW + 8, 16)


/* Round up x to p where x is positive and p is a power of 2 */
#define Round_Up_Pow2(x, p)  (((x) + ((p) - 1)) & (-(p)))




/* arm64/linux and arm64/darwin act differently on local vs global symbols.
 * We use a pre_pass to discover the defined symbols in the MA file.
 *
 * Common points:
 *    A global symbol is visible from everywhere (has a .global asm directive).
 *    and is referenced via the GOT (Global Offset Table).
 *    Seems always PIC code 
 *    (on linux use .LANCHORn labels to optimize loadings in the same region ?)
 *
 *    A local symbol is only visible in the source.
 *    Some instructions only work with a local symbol, e.g. bgt label (label must be local)
 *    Due to RISC approach, an address is loaded (PC-relative) with 2 instructions 
 *          loading the page (page size = 4KB) with adrp (p stands for 'page')
 *          then add the page offset (i.e. 12 LSB).  Syntax:
 *
 *               arm64/linux                           arm64/darwin
 *            adrp  x0, myaddr                      adrp  x0, myaddr@PAGE
 *            add   x0, x0, :lo12:myaddr            add   x0, x0, myaddr@PAGEOFF
 *
 *          For a global symbol, the loading is similar but with an indirection via the GOT.
 *          (hence the ldr instead of 
 *
 *               arm64/linux                           arm64/darwin
 *            adrp  x0, :got:myaddr                 adrp  x0, _myaddr@GOTPAGE
 *            ldr   x0, [x0, :got_lo12:myaddr]      ldr   x0, [x0, _myaddr@GOTPAGEOFF]
 *
 * Specific to arm64/linux:
 *    symbols do not need a prefix _
 *    A code local symbol MUST NOT be addressed via the GOT.
 *    Local symbols are addressed using adrp and :loc_12: (page (4KB) and page offset)
 *    External symbols are indirectly addressed via GOT (using @GOTPAGE and @GOTPAGEOFF)
 *
 * Specific to arm64/darwin:
 *    An external symbol always starts with _   
 *    (local labels DO NOT begin with _ but a code local symbol can begin with _)
 *    Local symbols are addressed using @PAGE and @PAGEOFF
 *    External symbols are indirectly addressed via GOT (using @GOTPAGE and @GOTPAGEOFF)
 *    NB: a code local symbol can be addressed via the GOT (we do not use this however).
 *
 * All _FMT macros use %s%s for the underscore + symbol 
 * (even if we know there is no _ under arm64/linux) 
 */

#ifdef M_darwin

#define UN_EXT                     "_"

#define PAGE_FMT                   "%s%s@PAGE"
#define PAGEOFF_FMT                "%s%s@PAGEOFF"

#define GOT_PAGE_FMT               "%s%s@GOTPAGE"
#define GOT_PAGEOFF_FMT            "%s%s@GOTPAGEOFF"

#else

#define UN_EXT                     ""

#define PAGE_FMT                   "%s%s"
#define PAGEOFF_FMT                ":lo12:%s%s"

#define GOT_PAGE_FMT               ":got:%s%s"
#define GOT_PAGEOFF_FMT            ":got_lo12:%s%s"

#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char asm_reg_e[32];

int arg_reg_no;
int arg_dbl_reg_no;

#ifdef USE_LDR_PSEUDO_OP_AND_POOL
LabelGen lg_pool;
#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = TRUE;
  mi.can_produce_pic_code = TRUE;
  mi.comment_prefix = "#";

#ifdef M_darwin
  mi.local_symb_prefix = "L";
  mi.string_symb_prefix = "L.str.";
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
  strcpy(asm_reg_e, MAP_REG_E);
#else
  strcpy(asm_reg_e, "x25");
#endif

#ifdef USE_LDR_PSEUDO_OP_AND_POOL
  Label_Gen_Init(&lg_pool, "pool");
#endif

  Label_Printf(".text");
#if 0				/* see Fail_Ret() */
  Label("fail");
  Pl_Fail();
#endif
}




/*-------------------------------------------------------------------------*
 * ASM_STOP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Asm_Stop(void)
{
#ifdef __ELF__
  Inst_Printf(".section", ".note.GNU-stack,\"\"");
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
  Inst_Printf(".p2align", "2");
#else
  Inst_Printf(".p2align", "3,,7");
  Inst_Printf(".type", UN_EXT "%s, %%function", c->name);
#endif
  if (c->global)
    Inst_Printf(".global", UN_EXT "%s", c->name);

  Label_Printf("%s", "");
  Label_Printf(UN_EXT "%s:", c->name);

  if (!c->prolog)
    {
      Inst_Printf("sub", "sp, sp, #%d", RESERVED_STACK_SPACE);
      Inst_Printf("str", "x30, [sp]"); /* save lr (x30) */
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
Label(char *label)		/* only used for a local label */
{
  Label_Printf("%s", "");
  Label_Printf("%s:", label);
}




/*-------------------------------------------------------------------------*
 * EMIT_POOL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Pool(Bool after_call_c)
{
#ifdef USE_LDR_PSEUDO_OP_AND_POOL
  static int call_c_since_emit_pool = 0;

  if (after_call_c && ++call_c_since_emit_pool < MAX_CALL_C_BEFORE_EMIT_POOL)
    return;
  
  if (after_call_c)		/* after a call to a C fct needs a branch over the pool */
    {
      Inst_Printf("b", "%s", Label_Gen_New(&lg_pool));
      Inst_Printf(".ltorg", "%s", "");
      Label_Printf("%s:", Label_Gen_Get(&lg_pool));
    }
  else
    {
      Inst_Printf(".ltorg", "%s", "");
    }
  call_c_since_emit_pool = 0;
#endif
}




/*-------------------------------------------------------------------------*
 * NEAREST_IMMEDIATE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* Only certain values can be used for immediate addition, break into
 * separate steps consisting of these values (its good enough for gcc...)
 */
int
Nearest_Immediate(int target)
{
  int close = (target >= 0) ? target : -target;
  int exp = 0;

  while (close > 255)
    {
      exp += 2;
      close = close >> 2;
    }
  close = close << exp;

  return (target >= 0) ? close : -close;
}




/*-------------------------------------------------------------------------*
 * INCREMENT_REG                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Increment_Reg(char *r, int int_val)
{
  /* Could also be something like
   *    ldr x10, =int_val
   *    add r, r, x10
   * but sometimes 1 instruction is possible
   */
  char *op = "add";
  int slice, shift = 0;

  if (int_val < 0)
    {
      int_val = -int_val;
      op = "sub";
    }


  while (int_val)
    {
      slice = int_val & 0xfff;
      if (slice)
	Inst_Printf(op, "%s, %s, #%d, LSL %d", r, r, slice, shift);
      int_val >>= 12;
      shift += 12;
    }
}




/*-------------------------------------------------------------------------*
 * LOAD_IMMEDIATE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
/* To see what is produced by gcc for an immediate constant use:
 * i=4095; echo "long foo() {return $i;}" | gcc -O2 -S -o- -xc - 
 */
void
Load_Immediate(char *r, PlULong int_val)
{
#ifdef USE_LDR_PSEUDO_OP_AND_POOL
  Inst_Printf("ldr", "%s, =%" PL_FMT_d, r, int_val);
#else
  int slice, shift = 0;
  Bool wipe = TRUE;

  if (comment)
    Inst_Printf("", "# load %s = %" PL_FMT_u, r, int_val);

  while (int_val || wipe)
    {
      slice = int_val & 0xffff;
      if (slice || ((!int_val) && wipe))
	{
	  if (wipe)
	    {
	      Inst_Printf("movz", "%s, #%d, LSL %d", r, slice, shift);
	      wipe = FALSE;
	    }
	  else
	    Inst_Printf("movk", "%s, #%d, LSL %d", r, slice, shift);
	}
      int_val >>= 16;
      shift += 16;
    }
#endif
}




/*-------------------------------------------------------------------------*
 * LOAD_ADDRESS                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Address(char *r, char *addr)
{
  int scope = Scope_Of_Symbol(addr);

  if (scope > 0)
    {				/* 1 for local code, 2 for local data (long) */
      char *un = (scope == 1) ? UN_EXT : "";

      Inst_Printf("adrp", "%s, " PAGE_FMT, r, un, addr);
      Inst_Printf("add", "%s, %s, " PAGEOFF_FMT, r, r, un, addr);
    }
  else
    {
      Inst_Printf("adrp", "%s, " GOT_PAGE_FMT, r, UN_EXT, addr);
      Inst_Printf("ldr", "%s, [%s, " GOT_PAGEOFF_FMT "]", r, r, UN_EXT, addr);
    }
}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Load_Reg_Bank();
  Inst_Printf("ldr", "%s, [%s, #%d]", asm_reg_e, ASM_REG_BANK, MAP_OFFSET_E);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Inst_Printf("b", UN_EXT "%s", label);
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
#ifdef MAP_REG_CP
  Load_Address(MAP_REG_CP, Label_Cont_New());
#else
  Load_Address("x2", Label_Cont_New());
  Load_Reg_Bank();
  Inst_Printf("str", "x2, [%s, #%d]", ASM_REG_BANK, MAP_OFFSET_CP);
#endif
}




/*-------------------------------------------------------------------------*
 * HERE_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Here_CP(void)
{
  Emit_Pool(FALSE);
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
  Inst_Printf("ldr", "x11, [" MAP_REG_B ", #-8]");
#else
  Load_Reg_Bank();
  Inst_Printf("ldr", "x11, [%s, #%d]", ASM_REG_BANK, MAP_OFFSET_B);
  Inst_Printf("ldr", "x11, [x11, #-8]");
#endif
  Inst_Printf("ret", "x11");	/* prefer ret to br since hints it is a function return and optimize branch prediction */
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifdef MAP_REG_CP
  Inst_Printf("ret", MAP_REG_CP);
#else
  Load_Reg_Bank();
  Inst_Printf("ldr", "x11, [%s, #%d]", ASM_REG_BANK, MAP_OFFSET_CP);
  Inst_Printf("ret", "x11");
#endif
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Inst_Printf("b", "%s", label);
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Load_Reg_Bank();
  Inst_Printf("ldr", "x2, [%s, #%d]", ASM_REG_BANK, index * BPW);
}


#define Is_Load_Store_Offset_Immediate(offset)  ((offset) >= -255 && (offset) <= 255)

/*-------------------------------------------------------------------------*
 * LOAD_STORE_REG_Y                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Store_Reg_Y(char *ldr_str, char *r, int index)
{
  int offset = Y_OFFSET(index);	/* we know it is negative anyway... */

  if (Is_Load_Store_Offset_Immediate(offset))
    {
      Inst_Printf(ldr_str, "%s, [%s, #%d]", r, asm_reg_e, offset);
    }
  else
    {
      /* Needs another register R for the mov R, E. 
       * In case of a ldr we can use the same destination register r.
       * In case of a str we cannot use r because it is the source register to store.
       * We cannot use x0..x7, (call_c args) neither a gloal reg used in machine.h. 
       * x9 is OK 
       */
#if 0
      Inst_Printf("mov", "x9, %s", asm_reg_e);
      Increment_Reg("x9", offset);
      Inst_Printf(ldr_str, "%s, [x9]", r);
#else
      Load_Immediate("x9", offset);
      Inst_Printf(ldr_str, "%s, [%s, x9]", r, asm_reg_e);
#endif
    }
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Load_Store_Reg_Y("ldr", "x2", index);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Load_Reg_Bank();
  Inst_Printf("str", "x2, [%s, #%d]", ASM_REG_BANK, index * BPW);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Load_Store_Reg_Y("str", "x2", index);
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




#define STACK_OFFSET(offset)   (offset - arg_reg_no - arg_dbl_reg_no) * BPW

#define BEFORE_ARG					\
{                                                       \
  char r[32];						\
  Bool in_reg = FALSE;					\
							\
  if (arg_reg_no < MAX_ARGS_IN_REGS)			\
    {							\
      sprintf(r, "x%d", arg_reg_no++);			\
      in_reg = TRUE;					\
    }							\
  else							\
    strcpy(r, "x9");




#define BEFORE_ARG_DOUBLE				\
{                                                       \
  char r[32];						\
  Bool in_reg = FALSE;					\
							\
  if (arg_dbl_reg_no < MAX_ARGS_DOUBLE_IN_REGS)		\
    {							\
      sprintf(r, "d%d", arg_dbl_reg_no++);		\
      in_reg = TRUE;					\
    }							\
  else							\
    strcpy(r, "x9");




#define AFTER_ARG					\
  if (!in_reg)       					\
    Inst_Printf("str", "%s, [sp, #%d]", r, STACK_OFFSET(offset)); \
}


#define AFTER_ARG_DOUBLE  AFTER_ARG




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  BEFORE_ARG;

  Load_Immediate(r, int_val);

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
  BEFORE_ARG_DOUBLE;

#ifdef USE_LDR_PSEUDO_OP_AND_POOL
#if defined(__clang__) || defined(M_darwin)
   /* Bug in llvm asm parser: ldr pseudo instruction with a 64 immediate 
    * needs an X reg (D reg is not accepted), while gas aarch64/linux accepts it !
    * decompose via an ldr X reg, =imm64 + fmov 
    * NB: we test __clang__ or M_darwin since on darwin as is provided by llvm 
    * (even gcc-11 uses llvm as) 
    */
  if (in_reg)
    {
      Inst_Printf("ldr", "x9, =%" FMT64_d, d->v.i64);
      Inst_Printf("fmov", "%s, x9", r);
    }
  else
#endif
    Inst_Printf("ldr", "%s, =%" FMT64_d, r, d->v.i64);
#else
  Load_Address("x9", d->symb);
  Inst_Printf("ldr", "%s, [x9]", r);
#endif

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

  Load_Address(r, s->symb);

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

  Load_Address(r, name);
  Increment_Reg(r, index * BPW);
  if (!adr_of)
    Inst_Printf("ldr", "%s, [%s]", r, r);

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

  Load_Reg_Bank();
  Inst_Printf("mov", "%s, %s", r, ASM_REG_BANK);

  if (adr_of)
    Increment_Reg(r, index * BPW);
  else
    Inst_Printf("ldr", "%s, [%s, #%d]", r, r, index * BPW);

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

  Inst_Printf("mov", "%s, %s", r, asm_reg_e);
  if (adr_of)
    Increment_Reg(r, Y_OFFSET(index));
  else
    Load_Store_Reg_Y("ldr", r, index);

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
    return Call_C_Arg_Mem_L(offset, adr_of, "pl_foreign_double", index);

  BEFORE_ARG_DOUBLE;

  Load_Address("x7", "pl_foreign_double");
  Inst_Printf("ldr", "%s, [x7, %d]", r, index * BPW);

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
  Inst_Printf("bl", UN_EXT "%s", fct_name);
  Emit_Pool(TRUE);		/* only if too many call_c */
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
  Inst_Printf("ret", "x0");
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Inst_Printf("cmp", "x0, #0");
  Inst_Printf("bne", "%s", Label_Cont_New());
#if 0				/* see Asm_Start() */
  Inst_Printf("b", "fail");
#else
  Pl_Fail();
#endif
  Emit_Pool(FALSE);
  Label_Printf("%s:", Label_Cont_Get());
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_MEM_L                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Mem_L(char *name, int index)
{
  int offset = index * BPW;
  Load_Address("x7", name);
  if (offset <= 4095)
    {
      Inst_Printf("str", "x0, [x7, #%d]", offset);
    }
  else
    {
      Increment_Reg("x7", offset);
      Inst_Printf("str", "x0, [x7]");
    }
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* similar to Move_To_Reg_X */
  Load_Reg_Bank();
  Inst_Printf("str", "x0, [%s, #%d]", ASM_REG_BANK, index * BPW);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{
  Load_Store_Reg_Y("str", "x0", index);
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
  Load_Address("x7", "pl_foreign_double");
  Inst_Printf("str", "d0, [x7, %d]", index * BPW);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (Nearest_Immediate(int_val) == int_val)
    Inst_Printf("cmp", "x0, #%" PL_FMT_d, int_val);
  else
    {
      Load_Immediate("x7", int_val);
      Inst_Printf("cmp", "x0, x7");
    }
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Inst_Printf("beq", "%s", label);
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Inst_Printf("bgt", "%s", label);
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Inst_Printf("ldr", "x30, [sp]");
  Inst_Printf("add", "sp, sp, #%d", RESERVED_STACK_SPACE);
  Inst_Printf("ret", "%s", "");
  Emit_Pool(FALSE);
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
  /* str1.8 implies 8 bytes alignment, flags: M=Merge, S=Strings A=Alloc */
  /* then add a .align 3 before each string entry or a .space n after each string */
#ifdef M_darwin
  Inst_Printf(".section", "__TEXT,__cstring,cstring_literals");
#else
  Inst_Printf(".section", "%s", ".rodata.str1.8");
#endif
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(StringInf *s)
{
  /* gas .align on arm is same as .p2align, .align 3 means multiple of 2^3 = 8 */
  Inst_Printf(".align", "3");	/* on darwin, gcc emits this .align, clang do not... */
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
#ifndef USE_LDR_PSEUDO_OP_AND_POOL
  
#ifdef M_darwin
  Inst_Printf(".section", "__TEXT,__literal8,8byte_literals");
#else
  Inst_Printf(".section", ".rodata.cst8,\"aM\",@progbits,8");
#endif
  Inst_Printf(".align", "3");

#endif
}


/*-------------------------------------------------------------------------*
 * DICO_DOUBLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_Double(DoubleInf *d)
{
#ifndef USE_LDR_PSEUDO_OP_AND_POOL

  Label_Printf("%s:", d->symb);
  Inst_Printf(".long", "%d", d->v.i32[0]);
  Inst_Printf(".long", "%d", d->v.i32[1]);

  //  Inst_Printf(".double", ASM_DOUBLE_DIRECTIV_PREFIX "%1.20e", d->v.dbl);
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
      if (!l->global)
	Inst_Printf(".lcomm", "%s,%" PL_FMT_d, l->name, l->value * BPW);
      else
	Inst_Printf(".comm", UN_EXT "%s,%" PL_FMT_d ",8", l->name, l->value * BPW);
      break;

    case INITIAL_VALUE:
      if (l->global)
	{
	  Inst_Printf(".global", UN_EXT "%s", l->name);
	  Label_Printf(UN_EXT "%s:", l->name);
	}
      else
	Label_Printf("%s:", l->name);
      Inst_Printf(".xword", "%" PL_FMT_d, l->value);
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
  Inst_Printf(".section", "__DATA,__mod_init_func,mod_init_funcs");
  Inst_Printf(".p2align", "3");
  Inst_Printf(".quad", UN_EXT "%s", initializer_fct);	/* .quad and .xword are synonyms (aliases) */
#else
  Inst_Printf(".section", ".init_array,\"aw\"");
  Inst_Printf(".align", "3");
  Inst_Printf(".xword", UN_EXT "%s", initializer_fct);
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

  Inst_Printf(".xword", UN "obj_chain_start");
#endif
}
