/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : mini-assembler to assembler translator                          */
/* File  : ma_protos.h                                                     */
/* Descr.: code generation - header file                                   */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/


/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

void      Declare_Initializer   (char *initializer_fct);
void      Call_C                (char *fct_name,int nb_arg,ArgInf arg[]);
void      Switch_Ret            (int nb_swt,SwtInf swt[]);
void      Decl_Long             (char *name,int global,VType vtype,
                                 long value);



          /* from os_cpu.c files */

void      Source_Line           (int line_no,char *cmt);

void      Asm_Start             (void);
void      Asm_Stop              (void);

void      Code_Start            (char *label,int prolog,int global);
void      Code_Stop             (void);

void      Label                 (char *label);

void      Pl_Jump               (char *label);
void      Pl_Call               (char *label);
void      Pl_Fail               (void);
void      Pl_Ret                (void);

void      Jump                  (char *label);

void      Move_From_Reg_X       (int index);
void      Move_From_Reg_Y       (int index);
void      Move_To_Reg_X         (int index);
void      Move_To_Reg_Y         (int index);

void      Call_C_Start          (char *fct_name,int nb_args);
int       Call_C_Arg_Int        (int offset,int int_val);
int       Call_C_Arg_Double     (int offset,double dbl_val);
int       Call_C_Arg_String     (int offset,int str_no);
int       Call_C_Arg_Mem_L      (int offset,int adr_of,char *name,int index);
int       Call_C_Arg_Reg_X      (int offset,int adr_of,int index);
int       Call_C_Arg_Reg_Y      (int offset,int adr_of,int index);
int       Call_C_Arg_Foreign_L  (int offset,int adr_of,int index);
int       Call_C_Arg_Foreign_D  (int offset,int adr_of,int index);
void      Call_C_Stop           (char *fct_name,int nb_args);
void      Call_C_Adjust_Stack   (int nb_pushes);

void      Jump_Ret              (void);
void      Fail_Ret              (void);
void      Move_Ret_To_Mem_L     (char *name,int index);
void      Move_Ret_To_Reg_X     (int index);
void      Move_Ret_To_Reg_Y     (int index);
void      Move_Ret_To_Foreign_L (int index);
void      Move_Ret_To_Foreign_D (int index);
void      Cmp_Ret_And_Int       (int int_val);
void      Jump_If_Equal         (char *label);
void      Jump_If_Greater       (char *label);

void      C_Ret                 (void);

void      Dico_String_Start     (int nb);
void      Dico_String           (int str_no,char *asciiz);
void      Dico_String_Stop      (int nb);

void      Dico_Long_Start       (int nb);
void      Dico_Long             (char *name,int global,VType vtype,
                                 long value);
void      Dico_Long_Stop        (int nb);

void      Data_Start            (char *initializer_fct);
void      Data_Stop             (char *initializer_fct);
