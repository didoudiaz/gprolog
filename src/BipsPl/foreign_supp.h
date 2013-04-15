/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : foreign_supp.h                                                  *
 * Descr.: foreign interface support - header file                         *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define PL_RECOVER                 0
#define PL_CUT                     1
#define PL_KEEP_FOR_PROLOG         2

#define PL_FAILURE                 FALSE
#define PL_SUCCESS                 TRUE
#define PL_EXCEPTION               2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef WamWord PlTerm;

typedef enum { PL_FALSE, PL_TRUE} PlBool;

typedef struct
{
  Bool is_var;
  Bool unify;
  union
  {
    PlLong l;
    char *s;
    double d;
  }
  value;
}
PlFIOArg;





/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef FOREIGN_SUPP_FILE

int pl_foreign_bkt_counter;
char *pl_foreign_bkt_buffer;

#else

extern int pl_foreign_bkt_counter;
extern char *pl_foreign_bkt_buffer;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Foreign_Create_Choice(CodePtr codep_alt, int arity, int choice_size);

void Pl_Foreign_Update_Choice(CodePtr codep_alt, int arity, int choice_size);

CodePtr Pl_Foreign_Jump_Ret(CodePtr codep);

PlFIOArg *Pl_Foreign_Rd_IO_Arg(int arg_long, WamWord start_word,
			       PlLong (*rd_fct) (), int fio_arg_index);

Bool Pl_Foreign_Un_IO_Arg(int arg_long, Bool (*un_fct) (), PlFIOArg *fa,
			  WamWord start_word);



void Pl_Emit_Syntax_Error(char *file_name, int err_line, int err_col,
			  char *err_msg);



void Pl_Exec_Continuation(int func, int arity, WamWord *arg_adr);

void Pl_Throw(WamWord ball_word);

void Pl_Query_Begin(Bool recoverable);

int Pl_Query_Call(int func, int arity, WamWord *arg_adr);

int Pl_Query_Start(int func, int arity, WamWord *arg_adr, Bool recoverable);

int Pl_Query_Next_Solution(void);

void Pl_Query_End(int op);

WamWord Pl_Get_Exception(void);



#define Pl_Get_Choice_Counter()   pl_foreign_bkt_counter

#define Pl_Get_Choice_Buffer(t)   ((t) pl_foreign_bkt_buffer)

void Pl_No_More_Choice(void);



int Pl_Type_Of_Term(WamWord start_word);


char *Pl_Atom_Name(int atom);

int Pl_Atom_Length(int atom);

Bool Pl_Atom_Needs_Quote(int atom);

Bool Pl_Atom_Needs_Scan(int atom);

Bool Pl_Is_Valid_Atom(int atom);

int Pl_Atom_Char(char c);

int Pl_Atom_Nil(void);

int Pl_Atom_False(void);

int Pl_Atom_True(void);

int Pl_Atom_End_Of_File(void);



PlBool Pl_Unif(PlTerm term1, PlTerm term2);

PlBool Pl_Unif_With_Occurs_Check(PlTerm term1, PlTerm term2);



Bool Pl_Builtin_Var(WamWord term);

Bool Pl_Builtin_Non_Var(WamWord term);

Bool Pl_Builtin_Atom(WamWord term);

Bool Pl_Builtin_Integer(WamWord term);

Bool Pl_Builtin_Float(WamWord term);

Bool Pl_Builtin_Number(WamWord term);

Bool Pl_Builtin_Atomic(WamWord term);

Bool Pl_Builtin_Compound(WamWord term);

Bool Pl_Builtin_Callable(WamWord term);

Bool Pl_Builtin_Fd_Var(WamWord term);

Bool Pl_Builtin_Non_Fd_Var(WamWord term);

Bool Pl_Builtin_Generic_Var(WamWord term);

Bool Pl_Builtin_Non_Generic_Var(WamWord term);

Bool Pl_Builtin_List(WamWord term);

Bool Pl_Builtin_Partial_List(WamWord term);

Bool Pl_Builtin_List_Or_Partial_List(WamWord term);



Bool Pl_Builtin_Term_Eq(WamWord term1, WamWord term2);

Bool Pl_Builtin_Term_Neq(WamWord term1, WamWord term2);

Bool Pl_Builtin_Term_Lt(WamWord term1, WamWord term2);

Bool Pl_Builtin_Term_Lte(WamWord term1, WamWord term2);

Bool Pl_Builtin_Term_Gt(WamWord term1, WamWord term2);

Bool Pl_Builtin_Term_Gte(WamWord term1, WamWord term2);



Bool Pl_Builtin_Compare(WamWord cmp, WamWord term1, WamWord term2);

Bool Pl_Builtin_Arg(WamWord arg_no, WamWord term, WamWord sub_term);

Bool Pl_Builtin_Functor(WamWord term, WamWord functor, WamWord arity);

Bool Pl_Builtin_Univ(WamWord term, WamWord list);




Bool Pl_Builtin_Eq(WamWord expr1, WamWord expr2);

Bool Pl_Builtin_Neq(WamWord expr1, WamWord expr2);

Bool Pl_Builtin_Lt(WamWord expr1, WamWord expr2);

Bool Pl_Builtin_Lte(WamWord expr1, WamWord expr2);

Bool Pl_Builtin_Gt(WamWord expr1, WamWord expr2);

Bool Pl_Builtin_Gte(WamWord expr1, WamWord expr2);

void Pl_Math_Evaluate(WamWord expr, WamWord *result);






#define PLV                    REF

#define Stream_Pointer(s)      (pl_stm_tbl + (s))

