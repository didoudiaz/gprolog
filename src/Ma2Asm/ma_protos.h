/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_protos.h                                                     *
 * Descr.: code generation - header file                                   *
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


#include "../EnginePl/pl_long.h" /* ensure stdint.h */
#include "../EnginePl/bool.h"

#if 0
#define CHECK_PRINTF_ARGS
#endif

#ifdef CHECK_PRINTF_ARGS
#define GCCPRINTF(x) __attribute__((format(printf, x, x + 1)))
#else
#define GCCPRINTF(x)
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  Bool can_produce_pic_code;
  Bool needs_pre_pass;
  char *comment_prefix;
  char *local_symb_prefix;
  Bool strings_need_null;
  Bool needs_dico_double;
  Bool call_c_reverse_args;
}
MapperInf;




typedef struct
{
  char *name;			/* name of predicate/fct (label) */
  Bool prolog;			/* is prolog code ? (or C code which includes initializer) */
  Bool initializer;		/* is it an initialize (implies !prolog) */
  Bool global;			/* is it global ? */
}
CodeInf;




typedef enum
{
  NONE,				/* symbol is an uninitialized long (should be in a 0 init section like bss) */
  INITIAL_VALUE,		/* symbol is an initilialized symbol (see value) */
  ARRAY_SIZE,			/* symbol is an array of uninitialized longs (value = array size, section like bss) */
}
VType;


typedef struct
{
  char *name;			/* name of the long symbol */
  Bool global;			/* is it global ? */
  VType vtype;			/* what we find in value: NONE (and value=1), INITIAL_VALUE, ARRAY_SZIE */
  PlLong value;			/* the value */
}
LongInf;




typedef struct
{
  char *name;			/* name of the long symbol */
  int no;	 		/* no in the dico double (or -1 if !needs_dico_double) */
  char *ma_str;		/* as read in the MA file (can be a %a printf format) */
  Bool is_ma_str_human;		/* is ma_str human readable (or encoded, e.g. %d printf) ? */
  char *cmt_str;		/* a string usable for a comment */
  union {
    double dbl;			/* the double value */
    int64_t i64;		/* its 64 bits representation */
    int32_t i32[2];		/* its 2x32 bits representation */
  }v;
}
DoubleInf;




typedef struct
{
  char *prefix;			/* prefix of generated labels */
  int no;			/* current label no */
  char label[256];		/* current label */
}
LabelGen;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

	  /* defined in ma_parser.c */

#ifndef MA_PARSER_FILE

extern int reload_e;

#endif

#ifndef MA2ASM_FILE

extern Bool comment;
extern Bool pic_code;
extern MapperInf mi;
extern LabelGen lg_cont; /* used by macros Label_Cont_XXX() below */

#endif






/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


	  /* defined in ma2asm.c - used by the parser */

void Declare_Initializer(char *initializer_fct);

void Decl_Code(CodeInf *c);

void Decl_Long(LongInf *l);

void Call_C(char *fct_name, Bool fc, int nb_args, int nb_args_in_words, ArgInf arg[]);

void Switch_Ret(int nb_swt, SwtInf swt[]);




	  /* defined in ma2asm.c - used by mappers */

int Is_Code_Defined(char *name);

CodeInf *Get_Code_Infos(char *name);

LongInf *Get_Long_Infos(char *name);

int Scope_Of_Symbol(char *name);


void Label_Gen_Init(LabelGen *g, char *prefix);

char *Label_Gen_New(LabelGen *g);

char *Label_Gen_Get(LabelGen *g);

int Label_Gen_No(LabelGen *g);

#define Label_Cont_New() Label_Gen_New(&lg_cont)
#define Label_Cont_Get() Label_Gen_Get(&lg_cont)
#define Label_Cont_No()  Label_Gen_No(&lg_cont)


void Label_Printf(char *label, ...) GCCPRINTF(1);

void Inst_Printf(char *op, char *operands, ...) GCCPRINTF(2);

void Inst_Out(char *op, char *operands);

void Char_Out(char c);

void String_Out(char *s);

void Int_Out(int d);


	  /* defined in each mappers used by parser and ma2asm */

void Init_Mapper(void);

void Asm_Start(void);

void Asm_Stop(void);

void Code_Start(CodeInf *c);

void Code_Stop(CodeInf *c);

void Label(char *label);

void Reload_E_In_Register(void);

void Pl_Jump(char *label);

void Prep_CP(void);

void Here_CP(void);

void Pl_Call(char *label);

void Pl_Fail(void);

void Pl_Ret(void);

void Jump(char *label);

void Move_From_Reg_X(int index);

void Move_From_Reg_Y(int index);

void Move_To_Reg_X(int index);

void Move_To_Reg_Y(int index);

void Call_C_Start(char *fct_name, Bool fc, int nb_args, int nb_args_in_words);

int Call_C_Arg_Int(int offset, PlLong int_val);

int Call_C_Arg_Double(int offset, DoubleInf *d);

int Call_C_Arg_String(int offset, int str_no, char *asciiz);

int Call_C_Arg_Mem_L(int offset, Bool adr_of, char *name, int index);

int Call_C_Arg_Reg_X(int offset, Bool adr_of, int index);

int Call_C_Arg_Reg_Y(int offset, Bool adr_of, int index);

int Call_C_Arg_Foreign_L(int offset, Bool adr_of, int index);

int Call_C_Arg_Foreign_D(int offset, Bool adr_of, int index);

void Call_C_Invoke(char *fct_name, Bool fc, int nb_args, int nb_args_in_words);

void Call_C_Stop(char *fct_name, int nb_args);

void Call_C_Adjust_Stack(int nb_pushes);

void Jump_Ret(void);

void Fail_Ret(void);

void Move_Ret_To_Mem_L(char *name, int index);

void Move_Ret_To_Reg_X(int index);

void Move_Ret_To_Reg_Y(int index);

void Move_Ret_To_Foreign_L(int index);

void Move_Ret_To_Foreign_D(int index);

void Cmp_Ret_And_Int(PlLong int_val);

void Jump_If_Equal(char *label);

void Jump_If_Greater(char *label);

void C_Ret(void);

void Dico_String_Start(int nb);

void Dico_String(int str_no, char *asciiz);

void Dico_String_Stop(int nb);

void Dico_Double_Start(int nb);

void Dico_Double(DoubleInf *d);

void Dico_Double_Stop(int nb);

void Dico_Long_Start(int nb);

void Dico_Long(LongInf *l);

void Dico_Long_Stop(int nb);

void Data_Start(char *initializer_fct);

void Data_Stop(char *initializer_fct);




#define INL_ACCESS_NAME(p)  (p[0])
#define INL_ACCESS_NEXT(p)  (p[1])
#define INL_ACCESS_LEVEL(p) (PlLong) (p[2])
#define INL_ACCESS_INFO(p)  (PlLong) (p[3])

#define INL_NEXT            ((char *) (0))
#define INL_LEVEL(x)        ((char *) (x))
#define INL_INFO(x)         ((char *) (x))
#define INL_LABEL(x)        ((char *) (x))
#define INL_END_FUNC        ((char *) (-1))

