/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_protos.h                                                     *
 * Descr.: code generation - header file                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

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

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

	  /* defined in mapper files */

#ifndef MAPPER_FILE

extern char *comment_prefix;
extern char *local_symb_prefix;
extern int strings_need_null;
extern int call_c_reverse_args;
extern char *inline_asm_data[];

#endif


	  /* defined in ma_parser.c */

#ifndef MA_PARSER_FILE

extern int reload_e;

#endif



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


	  /* defined in ma2asm.c - used by the parser */

void Declare_Initializer(char *initializer_fct);

void Call_C(char *fct_name, int fc, int nb_arg, ArgInf arg[]);

void Switch_Ret(int nb_swt, SwtInf swt[]);

void Decl_Long(char *name, int global, VType vtype, long value);


	  /* defined in ma2asm.c - used by mappers */

void Label_Printf(char *label, ...) GCCPRINTF(1);

void Inst_Printf(char *op, char *operands, ...) GCCPRINTF(2);

void Inst_Out(char *op, char *operands);

void Char_Out(char c);

void String_Out(char *s);

void Int_Out(int d);


	  /* defined in mapper files */

void Asm_Start(void);

void Asm_Stop(void);

void Code_Start(char *label, int prolog, int global);

void Code_Stop(void);

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

void Call_C_Start(char *fct_name, int fc, int nb_args, char **p_inline);

int Call_C_Arg_Int(int offset, long int_val);

int Call_C_Arg_Double(int offset, double dbl_val);

int Call_C_Arg_String(int offset, int str_no);

int Call_C_Arg_Mem_L(int offset, int adr_of, char *name, int index);

int Call_C_Arg_Reg_X(int offset, int adr_of, int index);

int Call_C_Arg_Reg_Y(int offset, int adr_of, int index);

int Call_C_Arg_Foreign_L(int offset, int adr_of, int index);

int Call_C_Arg_Foreign_D(int offset, int adr_of, int index);

void Call_C_Invoke(char *fct_name, int nb_args);

void Call_C_Stop(char *fct_name, int nb_args, char **p_inline);

void Call_C_Adjust_Stack(int nb_pushes);

void Jump_Ret(void);

void Fail_Ret(void);

void Move_Ret_To_Mem_L(char *name, int index);

void Move_Ret_To_Reg_X(int index);

void Move_Ret_To_Reg_Y(int index);

void Move_Ret_To_Foreign_L(int index);

void Move_Ret_To_Foreign_D(int index);

void Cmp_Ret_And_Int(long int_val);

void Jump_If_Equal(char *label);

void Jump_If_Greater(char *label);

void C_Ret(void);

void Dico_String_Start(int nb);

void Dico_String(int str_no, char *asciiz);

void Dico_String_Stop(int nb);

void Dico_Long_Start(int nb);

void Dico_Long(char *name, int global, VType vtype, long value);

void Dico_Long_Stop(int nb);

void Data_Start(char *initializer_fct);

void Data_Stop(char *initializer_fct);




#define INL_ACCESS_NAME(p)  (p[0])
#define INL_ACCESS_NEXT(p)  (p[1])
#define INL_ACCESS_LEVEL(p) (long) (p[2])
#define INL_ACCESS_INFO(p)  (long) (p[3])

#define INL_NEXT            ((char *) (0))
#define INL_LEVEL(x)        ((char *) (x))
#define INL_INFO(x)         ((char *) (x))
#define INL_LABEL(x)        ((char *) (x))
#define INL_END_FUNC        ((char *) (-1))

