/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : sparc64_any.c                                                   *
 * Descr.: translation file for sparc 64 bits                              *
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


/* Supported arch: sparc64 on Solaris, BSD
 */




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define UN




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

Bool pic_helper_ready = FALSE;

char buff[1024];
int delay_active = FALSE;
char *delay_op = NULL;
char delay_operands[1024];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Delay_Printf(char *op, char *operands, ...);
int Delay_Flush(void);

/* LITTLE_INT for signed 13 bits, ie: ((x) >= -4096 && (x) < 4096) */

#if 1 /* put 0 to force loading into reg instead of simm13 (for check only) */
#define OK_FOR_SIMM13(x)     ((unsigned) ((x) + 4096) < 8192)
#else
#define OK_FOR_SIMM13(x)  0
#endif


/* On sparc64 (sparc v9) / OpenBSD we can use the following registers:
 *
 *    %g1 - %g4 mainly temporaries
 *    %o1 - %o5 argument passing for function call or temporaries
 *    %l0 - %l7 local regs usable because callee saved
 *
 * We can safely use %l registers in Prolog code even if they are callee
 * save and even if in Prolog code we do not save them (no save/restore).
 * This is because Prolog code is called via Pl_Call_Compiled (see engine1.c)
 * which saves them (callee save). It is even possible to use them as global
 * registers to store constant values (e.g. pl_reg_bank) if the are set by
 * Pl_Call_Compiled(). We use
 *
 *    %l0 for pl_reg_bank
 *    %l2 for pl_base_fl (pl_foreign_long)
 *    %l3 for pl_base_fd (pl_foreign_double)
 *
 *    %l1 is used to load E (reloaded after a call to an Allocate function)
 *
 *    %l7 is used for PIC register (on On OpenBSD all code is PIC)
 *    NB: it is important to inform the assembler about this passing -K PIC
 *        (see configure.in)
 *
 * To pass a floating point literal (double) we build it using 64 bits
 * instructions and then move it to a float register (%fnn) using store/load
 * (as done by gcc 4.2.1). It would be possible to store them in the data
 * section and load the float register with this data (as done by gcc 4.8.2).
 * The would be very similar to what is done in x86_64_any.c.
 */



/*-------------------------------------------------------------------------*
 * INIT_MAPPER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Init_Mapper(void)
{
  mi.needs_pre_pass = FALSE;
  mi.can_produce_pic_code = FALSE;
  mi.comment_prefix = "!";	/* NB: # does not work on Solaris 9 */
  mi.local_symb_prefix = "L";	/* TODO check local symbol */
  mi.string_symb_prefix = ".LC";
  mi.double_symb_prefix = "LCD"; /* not used */
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

  Delay_Printf(".section", "\".text\"");

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
  Delay_Printf(".section", ".gnu.linkonce.t.__sparc_get_pc_thunk.l7,\"ax\",@progbits");
  Delay_Printf(".align", "4");
  Delay_Printf(".weak", "__sparc_get_pc_thunk.l7");
  Delay_Printf(".hidden", "__sparc_get_pc_thunk.l7");
  Delay_Printf(".type", "__sparc_get_pc_thunk.l7,@function");
  Delay_Printf(".proc", "020");
  Label("__sparc_get_pc_thunk.l7");
  Delay_Printf("jmp", "%%o7+8");
  Delay_Printf("add", "%%o7, %%l7, %%l7");
}




/*-------------------------------------------------------------------------*
 * CODE_START                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Start(CodeInf *c)
{
  Label_Printf("%s", "");
  Delay_Printf(".align", "4");
  Delay_Printf(".align", "32");
#if defined(M_solaris) || defined(M_bsd)
  Delay_Printf(".type", UN "%s,#function", c->name);
#endif
  Delay_Printf(".proc", "020");

  if (c->global)
    Delay_Printf(".global", UN "%s", c->name);

  Label(c->name);

  if (!c->prolog)
    Delay_Printf("save", "%%sp, -192, %%sp");

  pic_helper_ready = FALSE;
}




/*-------------------------------------------------------------------------*
 * CODE_STOP                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Code_Stop(CodeInf *c)
{
  /* this directive is mandatory else asm calls are wrong (PC relative) */
  Delay_Printf(".size", "%s, .-%s", c->name, c->name);
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
 * ENSURE_PIC_HELPER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Ensure_PIC_Helper(void)
{
  if (pic_helper_ready)
    return;
    
  Delay_Printf("sethi", "%%hi(_GLOBAL_OFFSET_TABLE_-4), %%l7");
  Delay_Printf("call", "__sparc_get_pc_thunk.l7");
  Delay_Printf("add", "%%l7, %%lo(_GLOBAL_OFFSET_TABLE_+4), %%l7");
  pic_helper_ready = TRUE;

}




/*-------------------------------------------------------------------------*
 * RELOAD_E_IN_REGISTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reload_E_In_Register(void)
{
#ifndef MAP_REG_E
  Delay_Printf("ldx", "[%s+%d], %s", asm_reg_bank, MAP_OFFSET_E, asm_reg_e);
#endif
}




/*-------------------------------------------------------------------------*
 * SYNTHETIZE_SETX                                                         *
 *                                                                         *
 * This corresponds to the mapping of the setx synthetic instruction       *
 * (see table 37 of The SPARC Architecture Manual Version 9)               *
 * The following code is adapted from GNU binutils/gas, file tc-sparc.c    *
 *-------------------------------------------------------------------------*/
void
Synthetize_Setx(long value, char *tmpreg, char *dstreg)
{
  int upper32 = value >> 32;
  int lower32 = value;
  char *upper_dstreg = tmpreg;
  Bool need_hh22_p, need_hm10_p, need_hi22_p, need_lo10_p, need_xor10_p = FALSE;

  need_hh22_p = need_hm10_p = need_hi22_p = need_lo10_p = need_xor10_p = FALSE;

  /* What to output depends on the number if it's constant.
     Compute that first, then output what we've decided upon.  */
 
  /* Only need hh22 if `or' insn can't handle constant.  */
  if (!OK_FOR_SIMM13(upper32))
    need_hh22_p = TRUE;

  /* Does bottom part (after sethi) have bits?  */
  if ((need_hh22_p && (upper32 & 0x3ff) != 0)
      /* No hh22, but does upper32 still have bits we can't set from lower32?  */
      || (! need_hh22_p && upper32 != 0 && upper32 != -1))
    need_hm10_p = TRUE;

  /* If the lower half is all zero, we build the upper half directly into the dst reg.  */
  if (lower32 != 0
      /* Need lower half if number is zero or 0xffffffff00000000.  */
      || (! need_hh22_p && ! need_hm10_p))
    {
      /* No need for sethi if `or' insn can handle constant.  */
      if (!OK_FOR_SIMM13(lower32)
	  /* Note that we can't use a negative constant in the `or'
	     insn unless the upper 32 bits are all ones.  */
	  || (lower32 < 0 && upper32 != -1)
	  || (lower32 >= 0 && upper32 == -1))
	need_hi22_p = TRUE;

      if (need_hi22_p && upper32 == -1)
	need_xor10_p = TRUE;

      /* Does bottom part (after sethi) have bits?  */
      else if ((need_hi22_p && (lower32 & 0x3ff) != 0)
	       /* No sethi.  */
	       || (! need_hi22_p && (lower32 & 0x1fff) != 0)
	       /* Need `or' if we didn't set anything else.  */
	       || (! need_hi22_p && ! need_hh22_p && ! need_hm10_p))
	need_lo10_p = TRUE;
    }
  else
    /* Output directly to dst reg if lower 32 bits are all zero.  */
    upper_dstreg = dstreg;


#define MK_IMM22(x)              (((x) >> 10) & 0x3fffff)
#define MK_SIMM10_13(x, want10)  ((x) & ((want10) ?  0x3ff : 0x1fff))

  if (need_hh22_p)
    Delay_Printf("sethi", "%d, %s", MK_IMM22(upper32), upper_dstreg);

  if (need_hi22_p)
    Delay_Printf("sethi", "%d, %s", MK_IMM22(need_xor10_p ? ~lower32 : lower32), dstreg);

  if (need_hm10_p)
    Delay_Printf("or", "%s, %ld, %s", (need_hh22_p ? upper_dstreg : "%g0"), MK_SIMM10_13(upper32, need_hh22_p), upper_dstreg);

  if (need_lo10_p)
    Delay_Printf("or", "%s, %ld, %s", (need_hi22_p ? dstreg : "%g0"), MK_SIMM10_13(lower32, need_hi22_p), dstreg);

  /* If we needed to build the upper part, shift it into place.  */
  if (need_hh22_p || need_hm10_p)
    Delay_Printf("sllx", "%s, 32, %s", upper_dstreg, upper_dstreg);

  /* To get -1 in upper32, we do sethi %hi(~x), r; xor r, -0x400 | x, r.  */
  if (need_xor10_p)
    Delay_Printf("xor", "%s, %ld, %s", dstreg, 0x1c00 | (lower32 & 0x3ff), dstreg);
  /* If we needed to build both upper and lower parts, OR them together.  */
  else if ((need_hh22_p || need_hm10_p) && (need_hi22_p || need_lo10_p))
    Delay_Printf("or", "%s, %s, %s", dstreg, upper_dstreg, dstreg);
 }




/*-------------------------------------------------------------------------*
 * LOAD_LONG_INTO_REG                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Long_Into_Reg(PlLong x, char *reg)
{
  Synthetize_Setx(x, "%g2", reg);
}




/*-------------------------------------------------------------------------*
 * LOAD_MEM_INTO_REG                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Mem_Into_Reg(char *mem_base_reg, int displ, Bool adr_of, char *reg)
{
  static char tmp[32];
  char *str_displ;

  if (OK_FOR_SIMM13(displ))
    {
      str_displ = tmp;
      sprintf(str_displ, "%d", displ);
    }
  else
    {
      str_displ = "%g1";
      Load_Long_Into_Reg(displ, str_displ);
    }

  if (!adr_of)
    Delay_Printf("ldx", "[%s+%s], %s", mem_base_reg, str_displ, reg);
  else if (displ != 0 || strcmp(mem_base_reg, reg) != 0) /* avoid  add r,0,r = nop */
    Delay_Printf("add", "%s, %s, %s", mem_base_reg, str_displ, reg);
}




/*-------------------------------------------------------------------------*
 * STORE_REG_INTO_MEM                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Store_Reg_Into_Mem(char *reg, char *mem_base_reg, int displ)
{
  static char tmp[32];
  char *str_displ;

  if (OK_FOR_SIMM13(displ))
    {
      str_displ = tmp;
      sprintf(str_displ, "%d", displ);
    }
  else
    {
      str_displ = "%g1";
      Load_Long_Into_Reg(displ, str_displ);
    }

  Delay_Printf((reg[1] != 'f') ? "stx" : "std", "%s, [%s+%s]", reg, mem_base_reg, str_displ);
}




/*-------------------------------------------------------------------------*
 * PL_JUMP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Jump(char *label)
{
  Delay_Printf("call", UN "%s, 0", label);
  Delay_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * PREP_CP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prep_CP(void)
{
  Ensure_PIC_Helper();

#if 1
  /* .Lcont - 8 to (un)adjust CP */
  Delay_Printf("sethi", "%%hi(_GLOBAL_OFFSET_TABLE_-(%s-.-8)), %%g1", Label_Cont_New());
  Delay_Printf("or", "%%g1, %%lo(_GLOBAL_OFFSET_TABLE_-(%s-.-8)), %%g1", Label_Cont_Get());
  Delay_Printf("sub", "%%l7, %%g1, %%g1");
  Delay_Printf("stx", "%%g1, %s", asm_reg_cp);
#else
  Delay_Printf("sethi", "%%hi(%s), %%g1", Label_Cont_New());
  Delay_Printf("or", "%%g1, %%lo(%s), %%g1", Label_Cont_Get());
  Delay_Printf("ldx", "[%%l7+%%g1], %%g1");
  Delay_Printf("sub", "%%g1, 8, %%g1"); /* -8 to (un)adjust CP */
  Delay_Printf("stx", "%%g1, %s", asm_reg_cp);
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
  pic_helper_ready = FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call(char *label)
{
  Delay_Printf("call", UN "%s, 0", label);
#ifdef MAP_REG_CP
  Delay_Printf("mov", "%%o7, %s", asm_reg_cp);	/* delay slot */
#else
  Delay_Printf("stx", "%%o7, %s", asm_reg_cp);	/* delay slot */
#endif
  pic_helper_ready = FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_FAIL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fail(void)
{
#ifdef MAP_REG_B
  Delay_Printf("ldx", "[%s-8], %%o0", asm_reg_b);
#else
  Delay_Printf("ldx", "%s, %%o0", asm_reg_b);
  Delay_Printf("ldx", "[%%o0-8], %%o0");
#endif

  Delay_Printf("call", "%%o0");
  Delay_Printf("nop", "%s", "");	/* delay slot */
  pic_helper_ready = FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_RET                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Ret(void)
{
#ifdef MAP_REG_CP

#else
  Delay_Printf("ldx", "%s, %%o0", asm_reg_cp);
  Delay_Printf("jmp", "%%o0+8");
#endif
  Delay_Printf("nop", "%s", "");	/* delay slot */
  pic_helper_ready = FALSE;
}




/*-------------------------------------------------------------------------*
 * JUMP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump(char *label)
{
  Delay_Printf("ba", UN "%s", label);
  Delay_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_X                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_X(int index)
{
  Load_Mem_Into_Reg(asm_reg_bank, index * 8, 0, "%o0");
}




/*-------------------------------------------------------------------------*
 * MOVE_FROM_REG_Y                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_From_Reg_Y(int index)
{
  Load_Mem_Into_Reg(asm_reg_e, Y_OFFSET(index), 0, "%o0");
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_X                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_X(int index)
{
  Store_Reg_Into_Mem("%o0", asm_reg_bank, index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_TO_REG_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_To_Reg_Y(int index)
{
  Store_Reg_Into_Mem("%o0", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * CALL_C_START                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words)
{
  delay_active = TRUE;
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
    strcpy(r, "%l4");

#define STACK_BIAS 2047
#define STACK_OFFSET(offset) (STACK_BIAS + 176 + ((offset) - MAX_ARGS_IN_REGS) * 8)

#define AFTER_ARG							\
  if (offset >= MAX_ARGS_IN_REGS)					\
    Delay_Printf("stx", "%s, [%%sp+%d]", r, STACK_OFFSET(offset));	\
}


#define MAX_FP_ARGS_IN_REGS 16

#define BEFORE_ARG_DOUBLE			\
{						\
  char *r = "%g1";   /* load in a temp */


      /* floating point args in %f0, %f2, %f4 (double precision) */
#define AFTER_ARG_DOUBLE						\
  if (offset < MAX_FP_ARGS_IN_REGS)					\
    {									\
      Delay_Printf("stx", "%s, [%%fp+2023]", r);				\
      Delay_Printf("ldd", "[%%fp+2023], %%f%d", offset * 2);		\
    }									\
 else									\
    Delay_Printf("stx", "%s, [%%sp+%d]", r, STACK_OFFSET(offset));	\
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_INT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_Int(int offset, PlLong int_val)
{
  BEFORE_ARG;

  Load_Long_Into_Reg(int_val, r);

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

  Load_Long_Into_Reg(d->v.i64, r);

  AFTER_ARG_DOUBLE;

  return 1;
}




/*-------------------------------------------------------------------------*
 * LOAD_MEM_BASE_INTO_REG                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Mem_Base_Into_Reg(char *label, int displ, char *reg)
{
  Ensure_PIC_Helper();

  Delay_Printf("sethi", "%%hi(%s), %s", label, reg);
  Delay_Printf("or", "%s, %%lo(%s), %s", reg, label, reg);
  Delay_Printf("ldx", "[%%l7+%s], %s", reg, reg);
}




/*-------------------------------------------------------------------------*
 * CALL_C_ARG_STRING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Call_C_Arg_String(int offset, StringInf *s)
{
  BEFORE_ARG;

  Ensure_PIC_Helper();

  Load_Mem_Base_Into_Reg(s->symb, 0, r);

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

  Load_Mem_Base_Into_Reg(name, 0, r);
  Load_Mem_Into_Reg(r, index * 8, adr_of, r);

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

  Load_Mem_Into_Reg(asm_reg_bank, index * 8, adr_of, r);

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

  Load_Mem_Into_Reg(asm_reg_e,  Y_OFFSET(index), adr_of, r);

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

  Load_Mem_Into_Reg("%l2", index * 8, adr_of, r);

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

      Load_Mem_Into_Reg("%l3", index * 8, adr_of, r);

      AFTER_ARG;

      return 1;
    }


  BEFORE_ARG_DOUBLE;

  Load_Mem_Into_Reg("%l3", index * 8, adr_of, r);

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
  delay_active = FALSE;		/* stop the delay, so Delay_Printf is a Delay_Printf */

  Delay_Printf("call", UN "%s, 0", fct_name);
  if (!Delay_Flush())		/* emit the delay insn to fill the delay slot */
    Delay_Printf("nop", "%s", "");	/* else fill it with a nop */
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
  Delay_Printf("jmp", "%%o0");
  Delay_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * FAIL_RET                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fail_Ret(void)
{
  Delay_Printf("cmp", "%%o0, 0");

#if 0
  Delay_Printf("be", UN "fail");
  Delay_Printf("nop", "%s", "");	/* delay slot */
#else

  Delay_Printf("be", UN "%s+4", "fail"); /* use delay slot */
#ifdef MAP_REG_B
  Delay_Printf("ldx", "[%s-8], %%o0", asm_reg_b); /* use first insn of Pl_Fail  */
#else
  Delay_Printf("ldx", "%s, %%o0", asm_reg_b);
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
  Load_Mem_Base_Into_Reg(name, 0, "%o1");
  Store_Reg_Into_Mem("%o0", "%o1", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_X                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_X(int index)
{				/* same as Move_To_Reg_X */
  Store_Reg_Into_Mem("%o0", asm_reg_bank, index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_REG_Y                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Reg_Y(int index)
{				/* same as Move_To_Reg_Y */
  Store_Reg_Into_Mem("%o0", asm_reg_e, Y_OFFSET(index));
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_L                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_L(int index)
{
  Store_Reg_Into_Mem("%o0", "%l2", index * 8);
}




/*-------------------------------------------------------------------------*
 * MOVE_RET_TO_FOREIGN_D                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Move_Ret_To_Foreign_D(int index)
{
  Store_Reg_Into_Mem("%f0", "%l3", index * 8);
}




/*-------------------------------------------------------------------------*
 * CMP_RET_AND_INT                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cmp_Ret_And_Int(PlLong int_val)
{
  if (OK_FOR_SIMM13(int_val))
    Delay_Printf("cmp", "%%o0, %ld", int_val);
  else
    {
      Load_Long_Into_Reg(int_val, "%g1");
      Delay_Printf("cmp", "%%o0, %%g1");
    }

}




/*-------------------------------------------------------------------------*
 * JUMP_IF_EQUAL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Equal(char *label)
{
  Delay_Printf("be", UN "%s", label);
  Delay_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * JUMP_IF_GREATER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Jump_If_Greater(char *label)
{
  Delay_Printf("bg", UN "%s", label);
  Delay_Printf("nop", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * C_RET                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
C_Ret(void)
{
  Delay_Printf("ret", "%s", "");
  Delay_Printf("restore", "%s", "");	/* delay slot */
}




/*-------------------------------------------------------------------------*
 * DICO_STRING_START                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String_Start(int nb)
{
  Delay_Printf(".section", ".rodata.str1.8,\"aMS\",@progbits,1");
}




/*-------------------------------------------------------------------------*
 * DICO_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Dico_String(StringInf *s)
{
  Delay_Printf(".align", "8");
  Label_Printf("%s:", s->symb);
  Delay_Printf(".asciz", "%s", s->str);
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
  Delay_Printf(".section", "\".data\"");
  Delay_Printf(".align", "8");
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
	Delay_Printf(".reserve", UN "%s,%ld,\"bss\",8", l->name, l->value * 8);
      else
	Delay_Printf(".common", UN "%s,%ld,\"bss\"", l->name, l->value * 8);
#else
      if (!l->global)
	Delay_Printf(".local", UN "%s", l->name);
      Delay_Printf(".common", UN "%s,%ld,8", l->name, l->value * 8);
#endif
      break;

    case INITIAL_VALUE:
      if (l->global)
	Delay_Printf(".globl", UN "%s", l->name);
#if defined(M_solaris) || defined(M_bsd)
      Delay_Printf(".align", "8", l->name);
      Delay_Printf(".type", UN "%s,#object", l->name);
      Delay_Printf(".size", UN "%s,8", l->name);
#endif
      Label_Printf(UN "%s:", l->name);
      Delay_Printf(".xword", "%ld", l->value);
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

  Delay_Printf(".section", "\".ctors\",#alloc,#write");
  Delay_Printf(".align", "8");
  Delay_Printf(".xword", UN "%s", initializer_fct);
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

  va_start(arg_ptr, operands);

  if (!delay_active)		
    { /* warning: the delay_op/delay_operands cannot be used (see Call_C_Invoke) */
      vsprintf(buff, operands, arg_ptr);
      Inst_Out(op, buff);
    }
  else
    {
      if (delay_op)
	Inst_Out(delay_op, delay_operands);

      delay_op = op;
      vsprintf(delay_operands, operands, arg_ptr);
    }

  va_end(arg_ptr);
}




/*-------------------------------------------------------------------------*
 * DELAY_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Delay_Flush(void)
{
  if (delay_op == NULL)
    return 0;

  Inst_Out(delay_op, delay_operands);
  delay_op = NULL;
  return 1;
}
