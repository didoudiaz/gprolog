/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bc_supp.c                                                       *
 * Descr.: byte-code support                                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OBJ_INIT Byte_Code_Initializer

#define BC_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"


#if 0
#define DEBUG
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_OP                     100
#define BC_BLOCK_SIZE              1024

#define ERR_UNKNOWN_INSTRUCTION    "bc_supp: Unknown WAM instruction: %s"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/


typedef enum
{
  GET_X_VARIABLE,
  GET_Y_VARIABLE,
  GET_X_VALUE,
  GET_Y_VALUE,
  GET_ATOM,
  GET_ATOM_BIG,
  GET_INTEGER,
  GET_INTEGER_BIG,
  GET_FLOAT,
  GET_NIL,
  GET_LIST,
  GET_STRUCTURE,

  PUT_X_VARIABLE,
  PUT_Y_VARIABLE,
  PUT_VOID,
  PUT_X_VALUE,
  PUT_Y_VALUE,
  PUT_Y_UNSAFE_VALUE,
  PUT_ATOM,
  PUT_ATOM_BIG,
  PUT_INTEGER,
  PUT_INTEGER_BIG,
  PUT_FLOAT,
  PUT_NIL,
  PUT_LIST,
  PUT_STRUCTURE,
  PUT_META_TERM,

  MATH_LOAD_X_VALUE,
  MATH_LOAD_Y_VALUE,

  UNIFY_X_VARIABLE,
  UNIFY_Y_VARIABLE,
  UNIFY_VOID,
  UNIFY_X_VALUE,
  UNIFY_Y_VALUE,
  UNIFY_X_LOCAL_VALUE,
  UNIFY_Y_LOCAL_VALUE,
  UNIFY_ATOM,
  UNIFY_ATOM_BIG,
  UNIFY_INTEGER,
  UNIFY_INTEGER_BIG,
  UNIFY_NIL,
  UNIFY_LIST,
  UNIFY_STRUCTURE,

  ALLOCATE,
  DEALLOCATE,

  CALL,
  CALL_NATIVE,
  EXECUTE,
  EXECUTE_NATIVE,
  PROCEED,
  FAIL,

  GET_CURRENT_CHOICE_X,
  GET_CURRENT_CHOICE_Y,

  CUT_X,
  CUT_Y,

  SOFT_CUT_X,
  SOFT_CUT_Y
}
BCCodOp;



typedef union
{
  struct
  {
    unsigned code_op:8;
    unsigned i8:8;
    unsigned i16:16;
  }
  t1;
  struct
  {
    unsigned code_op:8;
    unsigned i24:24;
  }
  t2;
  unsigned word;
}
BCWord;



typedef union
{
  double d;
#if WORD_SIZE == 64
  int *p;
  PlLong l;
#endif
  unsigned u[2];
}
C64To32;





/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static BCWord op_tbl[MAX_OP];
static int nb_op;

static BCWord *bc;
static BCWord *bc_sp;
static int bc_nb_block;

static int atom_dynamic;
static int atom_public;
static int atom_multifile;
static int atom_built_in;
static int atom_built_in_fd;
static int atom_local;
static int atom_fail;
static int atom_if;
static int atom_soft_if;
static int atom_call;
static int atom_catch;
static int atom_throw;

static int glob_module;		/* for byte-code creation */
static int glob_caller_func;
static int glob_caller_arity;

static int glob_func;
static DynPInf *glob_dyn;
static Bool debug_call;

WamCont pl_debug_call_code;	/* overwritten by debugger_c.c */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Find_Inst_Code_Op(int inst);

static int Compar_Inst_Code_Op(BCWord *w1, BCWord *w2);

static int BC_Arg_X_Or_Y(WamWord arg_word, int *op);

static int BC_Arg_Func_Arity(WamWord arg_word, int *arity);

static int BC_Arg_Module_Func_Arity(WamWord arg_word, int *func, int *arity);



WamCont Pl_BC_Emulate_Pred(int func, DynPInf *dyn);

static WamCont BC_Emulate_Pred_Alt(DynCInf *clause, WamWord *w);

static WamCont BC_Emulate_Clause(DynCInf *clause);

static WamCont BC_Emulate_Byte_Code(BCWord *bc);

static void Prep_Debug_Call(int module, int func, int arity,
			    int caller_func, int caller_arity);



#define BC_EMULATE_CONT            X1_2462635F656D756C6174655F636F6E74
#define CALL_INTERNAL_WITH_CUT     X1_2463616C6C5F696E7465726E616C5F776974685F637574
#define CALL_INTERNAL_AND          X1_2463616C6C5F696E7465726E616C5F616E64
#define CALL_INTERNAL_OR           X1_2463616C6C5F696E7465726E616C5F6F72
#define CALL_INTERNAL_IF           X1_2463616C6C5F696E7465726E616C5F6966
#define CALL_INTERNAL_SOFT_IF      X1_2463616C6C5F696E7465726E616C5F736F66745F6966
#define CATCH_INTERNAL             X1_2463617463685F696E7465726E616C
#define THROW_INTERNAL             X1_247468726F775F696E7465726E616C

Prolog_Prototype(BC_EMULATE_CONT, 0);
Prolog_Prototype(CALL_INTERNAL_WITH_CUT, 4);
Prolog_Prototype(CALL_INTERNAL_AND, 5);
Prolog_Prototype(CALL_INTERNAL_OR, 5);
Prolog_Prototype(CALL_INTERNAL_IF, 5);
Prolog_Prototype(CALL_INTERNAL_SOFT_IF, 5);

Prolog_Prototype(CALL_COMPLEX, 5);
Prolog_Prototype(CATCH_INTERNAL, 5);
Prolog_Prototype(THROW_INTERNAL, 2);

#define BC_Op(w)                   ((w).t1.code_op)

#define BC1_X0(w)                  ((w).t1.i8)

#define BC1_Arity(w)               ((w).t1.i16)

#define BC2_Arity(w)               ((w).t2.i24)

#define BC1_XY(w)                  ((w).t1.i16)

#define BC2_XY(w)                  ((w).t2.i24)

#define BC1_Atom(w)                ((w).t1.i16)

#define BC2_Atom(w)                ((w).t2.i24)

#define BC1_Int(w)                 ((w).t1.i16)

#define BC2_Int(w)                 ((w).t2.i24)

#define Fit_In_16bits(n)           ((PlULong) (n) < (1 << 16))

#define Fit_In_24bits(n)           ((PlULong) (n) < (1 << 24))

#define Op_In_Tbl(str, op)  BC_Op(*p) = op; BC2_Atom(*p) = Pl_Create_Atom(str); p++



#if WORD_SIZE == 32

#define Compute_Branch_Adr(bc, codep)   \
{                                       \
  codep = (WamCont) (bc->word); bc++;   \
}

#else

#define Compute_Branch_Adr(bc, codep)     \
{                                         \
  cv.u[0] = (unsigned) (bc->word); bc++;  \
  cv.u[1] = (unsigned) (bc->word); bc++;  \
  codep = (WamCont) (cv.p);               \
}

#endif



/*-------------------------------------------------------------------------*
 * BYTE_CODE_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Byte_Code_Initializer(void)
{
  BCWord *p = op_tbl;

  Op_In_Tbl("get_variable", GET_X_VARIABLE);
  Op_In_Tbl("get_value", GET_X_VALUE);
  Op_In_Tbl("get_atom", GET_ATOM);
  Op_In_Tbl("get_integer", GET_INTEGER);
  Op_In_Tbl("get_float", GET_FLOAT);
  Op_In_Tbl("get_nil", GET_NIL);
  Op_In_Tbl("get_list", GET_LIST);
  Op_In_Tbl("get_structure", GET_STRUCTURE);

  Op_In_Tbl("put_variable", PUT_X_VARIABLE);
  Op_In_Tbl("put_void", PUT_VOID);
  Op_In_Tbl("put_value", PUT_X_VALUE);
  Op_In_Tbl("put_unsafe_value", PUT_Y_UNSAFE_VALUE - 1);
  Op_In_Tbl("put_atom", PUT_ATOM);
  Op_In_Tbl("put_integer", PUT_INTEGER);
  Op_In_Tbl("put_float", PUT_FLOAT);
  Op_In_Tbl("put_nil", PUT_NIL);
  Op_In_Tbl("put_list", PUT_LIST);
  Op_In_Tbl("put_structure", PUT_STRUCTURE);
  Op_In_Tbl("put_meta_term", PUT_META_TERM);
  Op_In_Tbl("math_load_value", MATH_LOAD_X_VALUE);

  Op_In_Tbl("unify_variable", UNIFY_X_VARIABLE);
  Op_In_Tbl("unify_void", UNIFY_VOID);
  Op_In_Tbl("unify_value", UNIFY_X_VALUE);
  Op_In_Tbl("unify_local_value", UNIFY_X_LOCAL_VALUE);
  Op_In_Tbl("unify_atom", UNIFY_ATOM);
  Op_In_Tbl("unify_integer", UNIFY_INTEGER);
  Op_In_Tbl("unify_nil", UNIFY_NIL);
  Op_In_Tbl("unify_list", UNIFY_LIST);
  Op_In_Tbl("unify_structure", UNIFY_STRUCTURE);

  Op_In_Tbl("allocate", ALLOCATE);
  Op_In_Tbl("deallocate", DEALLOCATE);

  Op_In_Tbl("call", CALL);
  Op_In_Tbl("execute", EXECUTE);
  Op_In_Tbl("proceed", PROCEED);
  Op_In_Tbl("fail", FAIL);

  Op_In_Tbl("get_current_choice", GET_CURRENT_CHOICE_X);
  Op_In_Tbl("cut", CUT_X);
  Op_In_Tbl("soft_cut", SOFT_CUT_X);

  nb_op = p - op_tbl;

  qsort(op_tbl, nb_op, sizeof(op_tbl[0]),
	(int (*)(const void *, const void *)) Compar_Inst_Code_Op);


  bc_nb_block = 1;
  bc = (BCWord *) Malloc(bc_nb_block * BC_BLOCK_SIZE * sizeof(BCWord));

  atom_dynamic = Pl_Create_Atom("dynamic");
  atom_public = Pl_Create_Atom("public");
  atom_multifile = Pl_Create_Atom("multifile");
  atom_built_in = Pl_Create_Atom("built_in");
  atom_built_in_fd = Pl_Create_Atom("built_in_fd");
  atom_local = Pl_Create_Atom("local");
  atom_fail = Pl_Create_Atom("fail");
  atom_if = Pl_Create_Atom("->");
  atom_soft_if = Pl_Create_Atom("*->");
  atom_call = Pl_Create_Atom("call");
  atom_catch = Pl_Create_Atom("catch");
  atom_throw = Pl_Create_Atom("throw");
}




/*-------------------------------------------------------------------------*
 * Part I. Byte-Code creation.                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/



/*-------------------------------------------------------------------------*
 * FIND_INST_CODE_OP                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Find_Inst_Code_Op(int inst)
{
  BCWord *p;
  BCWord w;

  BC2_Atom(w) = inst;
  p = (BCWord *) bsearch(&w, op_tbl, nb_op, sizeof(op_tbl[0]),
			 (int (*)(const void *, const void *))
			 Compar_Inst_Code_Op);
  if (p == NULL)
    Pl_Fatal_Error(ERR_UNKNOWN_INSTRUCTION, pl_atom_tbl[inst].name);

  return BC_Op(*p);
}




/*-------------------------------------------------------------------------*
 * COMPAR_INST_CODE_OP                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Compar_Inst_Code_Op(BCWord *p1, BCWord *p2)
{
  return BC2_Atom(*p1) - BC2_Atom(*p2);
}




/*-------------------------------------------------------------------------*
 * PL_BC_START_PRED_9                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Start_Pred_9(WamWord module_word, WamWord func_word, WamWord arity_word,
		   WamWord pl_file_word, WamWord pl_line_word,
		   WamWord sta_dyn_word, WamWord pub_priv_word,
		   WamWord mono_multi_word, WamWord us_blp_bfd_word)
{
  int module, func, arity;
  int pl_file, pl_line;
  int prop = 0;
  int atom;
  int multi = 0;
  PredInf *pred;

  module = Pl_Rd_Atom_Check(module_word);
  func = Pl_Rd_Atom_Check(func_word);
  arity = Pl_Rd_Integer_Check(arity_word);
  pl_file = Pl_Rd_Atom_Check(pl_file_word);
  pl_line = Pl_Rd_Integer_Check(pl_line_word);

  if (Pl_Rd_Atom_Check(sta_dyn_word) == atom_dynamic)
    prop = MASK_PRED_DYNAMIC | MASK_PRED_PUBLIC;
  else if (Pl_Rd_Atom_Check(pub_priv_word) == atom_public)
    prop = MASK_PRED_PUBLIC;

  if (Pl_Rd_Atom_Check(mono_multi_word) == atom_multifile)
    {
      prop |= MASK_PRED_MULTIFILE;
      multi = 1;
    }

  atom = Pl_Rd_Atom_Check(us_blp_bfd_word);
  if (atom == atom_built_in)
    prop |= MASK_PRED_BUILTIN;
  else if (atom == atom_built_in_fd)
    prop |= MASK_PRED_BUILTIN_FD;
  else if (atom != atom_local)	/* an exported predicate (in case of module) - works for user, local, exported */
    prop |= MASK_PRED_EXPORTED;

  pred = Pl_Update_Dynamic_Pred(module, func, arity, 0, (multi) ? pl_file : -1);
  if (pred == NULL)
    pred = Pl_Create_Pred(module, func, arity, pl_file, pl_line, prop, NULL);
  else
    {
      if (multi)
	pred->prop |= prop;
      else
	{
	  pred->pl_file = pl_file;
	  pred->pl_line = pl_line;
	  pred->prop = prop;
	}
    }

  glob_module = module;
#if 1
  glob_caller_func = Pl_Pred_Without_Aux(func, arity, &glob_caller_arity);
#else
  glob_caller_func = func;
  glob_caller_arity = arity;
#endif

#ifdef DEBUG
  DBGPRINTF("BC start %s/%d\n", pl_atom_tbl[func].name, arity);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_BC_START_EMIT_0                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Start_Emit_0(void)
{
  bc_sp = bc;
}




/*-------------------------------------------------------------------------*
 * PL_BC_STOP_EMIT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Stop_Emit_0(void)
{
  int i;

  pl_byte_len = bc_sp - bc;

#ifdef DEBUG
  DBGPRINTF("byte-code size:%d\n", pl_byte_len);
#endif

  pl_byte_code = (unsigned *) Malloc(pl_byte_len * sizeof(BCWord));

  for (i = 0; i < pl_byte_len; i++)
    pl_byte_code[i] = bc[i].word;
}




#define ASSEMBLE_INST(bc_sp, op, nb_word, w, w1, w2, w3)	\
  BC_Op(w) = op;						\
  *bc_sp++ = w;							\
  if (nb_word >= 2)						\
    {								\
      bc_sp->word = w1;						\
      bc_sp++;							\
								\
      if (nb_word >= 3)						\
	{							\
	  bc_sp->word = w2;					\
	  bc_sp++;						\
	  if (nb_word >= 4)					\
	    {							\
	      bc_sp->word = w3;					\
	      bc_sp++;						\
	    }							\
	}							\
    }





/*-------------------------------------------------------------------------*
 * PL_BC_EMIT_INST_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Emit_Inst_1(WamWord inst_word)
{
  int module, func, arity;
  WamWord *arg_adr;
  int op;
  int size_bc;
  BCWord w;			/* code-op word */
  unsigned w1, w2, w3;		/* additional words */
  PlLong l;
  int nb_word;
  C64To32 cv;

  PredInf *pred;

  arg_adr = Pl_Rd_Callable_Check(inst_word, &func, &arity);

  op = Find_Inst_Code_Op(func);

  size_bc = bc_sp - bc;
  if (size_bc + 3 >= bc_nb_block * BC_BLOCK_SIZE)
    {
      bc_nb_block++;
      bc = (BCWord *) Realloc((char *) bc,
			      bc_nb_block * BC_BLOCK_SIZE * sizeof(BCWord));
      bc_sp = bc + size_bc;
    }


  w.word = 0;
  nb_word = 1;
  switch (op)
    {
    case GET_X_VARIABLE:
    case GET_X_VALUE:
    case PUT_X_VARIABLE:
    case PUT_X_VALUE:
    case PUT_Y_UNSAFE_VALUE - 1:
    case MATH_LOAD_X_VALUE:
      BC1_XY(w) = BC_Arg_X_Or_Y(*arg_adr++, &op);
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case GET_ATOM:
    case PUT_ATOM:
      w1 = Pl_Rd_Atom(*arg_adr++);
      if (Fit_In_16bits(w1))
	BC1_Atom(w) = w1;
      else
	{
	  op++;
	  nb_word = 2;
	}
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case GET_INTEGER:
    case PUT_INTEGER:
      l = Pl_Rd_Integer(*arg_adr++);
      if (Fit_In_16bits(l))
	BC1_Atom(w) = l;
      else
	{
	  op++;
#if WORD_SIZE == 32
	  w1 = l;
	  nb_word = 2;
#else
	  cv.l = l;
	  w1 = cv.u[0];
	  w2 = cv.u[1];
	  nb_word = 3;
#endif
	}
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case GET_FLOAT:
    case PUT_FLOAT:
      nb_word = 3;
      cv.d = Pl_Rd_Float(*arg_adr++);
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      w1 = cv.u[0];
      w2 = cv.u[1];
      break;

    case GET_NIL:
    case GET_LIST:
    case PUT_NIL:
    case PUT_LIST:
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case GET_STRUCTURE:
    case PUT_STRUCTURE:
      nb_word = 2;
      w1 = BC_Arg_Func_Arity(*arg_adr++, &arity);
      BC1_Arity(w) = arity;
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case PUT_META_TERM:
      nb_word = 2;
      w1 = Pl_Rd_Atom(*arg_adr++);
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr++);
      BC1_XY(w) = Pl_Rd_Integer(*arg_adr);
      break;

    case PUT_VOID:
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr);
      break;


    case UNIFY_X_VARIABLE:
    case UNIFY_X_VALUE:
    case UNIFY_X_LOCAL_VALUE:
    case GET_CURRENT_CHOICE_X:
    case CUT_X:
    case SOFT_CUT_X:
      BC2_XY(w) = BC_Arg_X_Or_Y(*arg_adr, &op);
      break;

    case UNIFY_ATOM:
      w1 = Pl_Rd_Atom(*arg_adr);
      if (Fit_In_24bits(w1))
	BC2_Atom(w) = w1;
      else
	{
	  op++;
	  nb_word = 2;
	}
      break;

    case UNIFY_INTEGER:
      l = Pl_Rd_Integer(*arg_adr++);
      if (Fit_In_24bits(l))
	BC2_Int(w) = l;
      else
	{
	  op++;
#if WORD_SIZE == 32
	  w1 = l;
	  nb_word = 2;
#else
	  cv.l = l;
	  w1 = cv.u[0];
	  w2 = cv.u[1];
	  nb_word = 3;
#endif
	}
      break;

    case UNIFY_STRUCTURE:
      w1 = BC_Arg_Func_Arity(*arg_adr++, (int *) &arity);
      BC2_Arity(w) = arity;
      nb_word = 2;
      break;

    case UNIFY_VOID:
    case ALLOCATE:
      BC2_Int(w) = Pl_Rd_Integer(*arg_adr);
      break;


    case CALL:
    case EXECUTE:
      module = BC_Arg_Module_Func_Arity(*arg_adr++, &func, &arity);
      BC2_Atom(w) = (module == -1) ? pl_atom_user : module;
      w1 = (unsigned) Functor_Arity(func, arity);
      pred = Pl_Lookup_Pred_Visible(pl_atom_user, func, arity); /* FIXME Module */
      if (pred && (pred->prop & MASK_PRED_NATIVE_CODE))
	{
	  op++;			/* switch to _NATIVE codep */
#if WORD_SIZE == 32
	  nb_word = 3;
	  w2 = (unsigned) (pred->codep);
	  w3 = 0;		/* to avoid MSVC warning */
#else
	  nb_word = 4;
	  cv.p = (int *) (pred->codep);
	  w2 = cv.u[0];
	  w3 = cv.u[1];
#endif
	}
      else
	{
	  nb_word = 3;
	  w2 = (unsigned) Functor_Arity(glob_caller_func, glob_caller_arity);
	}
      break;
    }


  ASSEMBLE_INST(bc_sp, op, nb_word, w, w1, w2, w3);


#ifdef DEBUG
  DBGPRINTF("   op: %3d  bc: %10.10x  ", op, w.word);

  if (nb_word >= 2)
    DBGPRINTF("%10.10x  ", w1);
  else
    DBGPRINTF("            ");

  if (nb_word >= 3)
    DBGPRINTF("%10.10x  ", w2);
  else
    DBGPRINTF("            ");

  if (nb_word >= 4)
    DBGPRINTF("%10.10x  ", w3);
  else
    DBGPRINTF("            ");

  Pl_Write_Simple(inst_word);
  DBGPRINTF("\n");
#endif
}



/*-------------------------------------------------------------------------*
 * BC_EMIT_INST_EXECUTE_NATIVE                                             *
 *                                                                         *
 * This function is called for dynamic or multifile predicate.             *
 * Each clause has been compiled to native code (aux pred).                *
 * We here create a call to this clause.                                   *
 * This function is called between Pl_BC_Start_Emit_0 and Pl_BC_Stop_Emit_0*
 * The buffer always bc has enough room for our 3 or 4 words.              *
 *-------------------------------------------------------------------------*/
static void
BC_Emit_Inst_Execute_Native(int module, int func, int arity, PlLong *codep) /* FIXME use module */
{
  BCWord w;			/* code-op word */
  unsigned w1, w2, w3;		/* additional words */
  int nb_word;
#if WORD_SIZE == 64
  C64To32 cv;
#endif

  w1 = func;
  BC2_Arity(w) = arity;
#if WORD_SIZE == 32
  nb_word = 3;
  w2 = (unsigned) codep;
  w3 = 0;		/* to avoid MSVC warning */
#else
  nb_word = 4;
  cv.p = (int *) codep;
  w2 = cv.u[0];
  w3 = cv.u[1];
#endif

  ASSEMBLE_INST(bc_sp, EXECUTE_NATIVE, nb_word, w, w1, w2, w3);
}




/*-------------------------------------------------------------------------*
 * PL_EMIT_BC_EXECUTE_WRAPPER                                              *
 *                                                                         *
 * This function is called by the compiled code for dynamic or multifile   *
 * predicate. Each clause has been compiled to native code (aux pred).     *
 * We here create a call to this clause.                                   *
 *-------------------------------------------------------------------------*/
void
Pl_Emit_BC_Execute_Wrapper(int module, int func, int arity, PlLong *codep)
{
  Pl_BC_Start_Emit_0();
  BC_Emit_Inst_Execute_Native(module, func, arity, codep);
  Pl_BC_Stop_Emit_0();
}




/*-------------------------------------------------------------------------*
 * BC_ARG_X_OR_Y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
BC_Arg_X_Or_Y(WamWord arg_word, int *op)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(arg_word, word, tag_mask);
  adr = UnTag_STC(word);

  if (Functor(adr) != ATOM_CHAR('x'))
    (*op)++;			/* +1 for op when Y is involved */

  return Pl_Rd_Integer(Arg(adr, 0));
}




/*-------------------------------------------------------------------------*
 * BC_ARG_FUNC_ARITY                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
BC_Arg_Func_Arity(WamWord arg_word, int *arity)
{
  WamWord word, tag_mask;
  WamWord *stc_adr;

  DEREF(arg_word, word, tag_mask);              /* functor/arity */
  stc_adr = UnTag_STC(word);

  DEREF(Arg(stc_adr, 1), word, tag_mask);	/* arity */
  *arity = UnTag_INT(word);

  DEREF(Arg(stc_adr, 0), word, tag_mask);	/* functor */
  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * BC_ARG_MODULE_FUNC_ARITY                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
BC_Arg_Module_Func_Arity(WamWord arg_word, int *func, int *arity)
{
  WamWord word, tag_mask;
  WamWord *stc_adr;
  int module = -1;

  DEREF(arg_word, word, tag_mask);              /* module:functor/arity or func/arity*/
  stc_adr = UnTag_STC(word);

  if (Functor_And_Arity(stc_adr) == Functor_Arity(ATOM_CHAR(':'), 2))
    {
      DEREF(Arg(stc_adr, 0), word, tag_mask);
      module = UnTag_ATM(word);
      arg_word = Arg(stc_adr, 1);
    }

  *func = BC_Arg_Func_Arity(arg_word, arity);
  return module;
}




/*-------------------------------------------------------------------------*
 * Part II. Byte-Code emulation                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 * EXECUTE_PRED                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
Execute_Pred(int module, int func, int arity, WamWord *arg_adr, 
	     WamWord call_info_word)
{
  PredInf *pred;
  int i;

  pred = Pl_Lookup_Pred_Visible(module, func, arity);
  if (pred == NULL)
    {				/* case: fail/0 from '$call_from_debugger' */
      if (func != atom_fail || arity != 0)
	{
	  Pl_Call_Info_Bip_Name_1(call_info_word);
	  Pl_Unknown_Pred_Error(module, func, arity);
	}
      return ALTB(B);		/* i.e. fail */
    }
#if 0				/* FIXME test module != current module (1st = type_in_module) */
  if (!(pred->prop & MASK_PRED_EXPORTED))
    {
      WamWord word = Pl_Built_Pred_Indic_Error(pred);
      Pl_Err_Permission(pl_permission_operation_execute, 
                        pl_permission_type_non_exported_procedure, word);                        
    }
#endif

  if (arg_adr != &A(0))
    memcpy(&A(0), arg_adr, sizeof(WamWord) * arity);

#if 1
  if (pred->prop & MASK_PRED_META_PRED)
    {
      MetaSpec meta_spec = pred->meta_spec;
      int x;

      for (i = 0; i < arity; i++)
	{
	  x = meta_spec & 0xf;
	  meta_spec >>= 4;

	  if (x <= META_PRED_ARG_COLON)
	    {
	      printf("in meta-call to %s/%d meta-arg %d = ", pl_atom_tbl[func].name, arity, i);
	      Pl_Write_Simple(A(i));
	      A(i) = Pl_Put_Meta_Term(module, A(i));
	      printf(" becomes ");
	      Pl_Write_Simple(A(i));
	      printf("\n");
	    }
	}
    }
#endif

  if (pred->prop & MASK_PRED_NATIVE_CODE)	/* native code */
    return (WamCont) (pred->codep);

  return Pl_BC_Emulate_Pred(func, (DynPInf *) (pred->dyn));
}




/*-------------------------------------------------------------------------*
 * PL_BC_CALL_INITIAL                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Call_Initial(int module, int func, int arity, WamWord *arg_adr, 
		   int caller_func, int caller_arity, Bool debug_call)
{
  WamWord call_info_word = Tag_INT(Call_Info(caller_func, caller_arity, 1));
  CodePtr codep;

  Pl_Set_Bip_Name_2(Tag_ATM(caller_func), Tag_INT(caller_arity));

  /* NB: arg_adr can be &A(0): beware when writting in A(i) to not overwrite args */

 terminal_rec:

  /* treat control constructs */

  switch(arity)
    {
    case 0:
      if (func == atom_fail)
	return ALTB(B);		/* i.e. fail */
      
      if (func == pl_atom_true || func == ATOM_CHAR('!'))
	return UnAdjust_CP(CP);

      break;

    case 1:
      if (func == atom_call)
	{
	  arg_adr = Pl_Rd_Callable_Check(*arg_adr, &func, &arity);
	  goto terminal_rec;
	}

      if (func == atom_throw)
	{
	  A(0) = *arg_adr;
	  A(1) = call_info_word;
	  return (CodePtr) Prolog_Predicate(THROW_INTERNAL, 2);
	}

#ifdef META_TERM_HIDDEN
      if (func == pl_atom_meta_term)
	{
	  arg_adr++;		      /* skip Offset (should we test it ?) */
	  if (Tag_Mask_Of(*arg_adr) != TAG_ATM_MASK) /* should be dereferenced */
	    break;		/* treat it as a normal goal ! */
	  module = UnTag_ATM(*arg_adr);
	  arg_adr = Pl_Rd_Callable_Check(*++arg_adr, &func, &arity);
	  goto terminal_rec;
	}
#endif      
      break;
      

    case 2:
      if (func == ATOM_CHAR(':'))
	{
	  module = Pl_Rd_Atom_Check(*arg_adr);
	  arg_adr = Pl_Rd_Callable_Check(*++arg_adr, &func, &arity);
	  goto terminal_rec;
	}

      /* complex call: needs Pl_Term_To_Goal transformation */


      if (func == ATOM_CHAR(','))
	{
	  codep = (CodePtr) Prolog_Predicate(CALL_INTERNAL_AND, 5);
	complex_call:

	  /* NB: arg_adr can be &A(0), thus args can be in A(0) and A(1). Beware */

	  A(0) = Pl_Term_To_Goal(arg_adr[0], module, call_info_word);
	  A(1) = Pl_Term_To_Goal(arg_adr[1], module, call_info_word);
	  A(2) = Tag_ATM(module);
	  A(3) = call_info_word;
	  A(4) = Pl_Get_Current_Choice(); /* VarCut */
	  return codep;
	}

      if (func == ATOM_CHAR(';'))
	{
	  codep = (CodePtr) Prolog_Predicate(CALL_INTERNAL_OR, 5);
	  goto complex_call;
	}

      if (func == atom_if)
	{
	  codep = (CodePtr) Prolog_Predicate(CALL_INTERNAL_IF, 5);
	  goto complex_call;
	}

      if (func == atom_soft_if)
	{
	  codep = (CodePtr) Prolog_Predicate(CALL_INTERNAL_SOFT_IF, 5);
	  goto complex_call;
	}

      break;
      
    case 3:
      if (func == atom_catch)
	{
	  if (arg_adr != &A(0))
	    {
	      A(0) = *arg_adr++;
	      A(1) = *arg_adr++;
	      A(2) = *arg_adr;
	    }
	  A(3) = Tag_ATM(module);
	  A(4) = call_info_word;
	  return (CodePtr) Prolog_Predicate(CATCH_INTERNAL, 5);
	}
     
      break;
    }

  /* here we have a simple predicate to call */

  debug_call = 1;
#if 0				  /* FIXME */
  if (pl_debug_call_code != NULL) /* inform the debugger if active */
    {
      A(0) = pred_word;
      A(1) = Tag_ATM(module);
      A(2) = call_info_word;
      return pl_debug_call_code;
    }
#endif
  return Execute_Pred(module, func, arity, arg_adr, call_info_word);
}




/*-------------------------------------------------------------------------*
 * PL_BC_CALL_5                                                            *
 *                                                                         *
 * called by '$call'/4 and '$call_no_debug'/5                              *
 *-------------------------------------------------------------------------*/
WamCont 
Pl_BC_Call_5(WamWord goal_word, WamWord module_word, 
	     WamWord caller_func_word, WamWord caller_arity_word,
	     Bool debug_call)
{
  int module, func, arity;
  int caller_func, caller_arity;
  WamWord *arg_adr;


  Pl_Set_Bip_Name_2(caller_func_word, caller_arity_word);

  module = Pl_Rd_Atom(module_word);
  Pl_Set_Calling_Module(module);

  caller_func = Pl_Rd_Atom(caller_func_word);
  caller_arity = Pl_Rd_Integer(caller_arity_word);

  arg_adr = Pl_Rd_Callable_Check(goal_word, &func, &arity);

  return Pl_BC_Call_Initial(module, func, arity, arg_adr, caller_func, caller_arity, debug_call);
}




/*-------------------------------------------------------------------------*
 * PL_BC_CALL_TERMINAL_PRED_4                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Call_Terminal_Pred_4(WamWord pred_word, WamWord module_word,
			   WamWord call_info_word, WamWord first_call_word)
{
  int func, arity;
  WamWord *arg_adr;
  int module = Pl_Rd_Atom(module_word);

  arg_adr = Pl_Rd_Callable(pred_word, &func, &arity); /* we know it is a callable */

  debug_call = (call_info_word & (1 << TAG_SIZE_LOW)) != 0;

  if (pl_debug_call_code != NULL && debug_call &&
      (first_call_word & (1 << TAG_SIZE_LOW)))
    {
      A(0) = pred_word;
      A(1) = module_word;
      A(2) = call_info_word;
      return pl_debug_call_code;
    }

  return Execute_Pred(module, func, arity, arg_adr, call_info_word);
}




/*-------------------------------------------------------------------------*
 * PL_BC_EMULATE_PRED                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Emulate_Pred(int func, DynPInf *dyn)
{
  DynCInf *clause;
  WamCont codep;
  int arity;

start:
  if (dyn == NULL)
    goto fail;

  arity = dyn->arity;
  A(arity) = Pl_Get_Current_Choice();	/* init cut register */
  A(arity + 1) = debug_call;

  clause = Pl_Scan_Dynamic_Pred(func, arity, dyn, A(0),
				(PlLong (*)()) BC_Emulate_Pred_Alt,
				DYN_ALT_FCT_FOR_JUMP, arity + 2, &A(0));
  if (clause == NULL)
    goto fail;

  codep = BC_Emulate_Clause(clause);
  if (codep)
    return (codep);

  func = glob_func;
  dyn = glob_dyn;
  goto start;

fail:
  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_PRED_ALT                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
BC_Emulate_Pred_Alt(DynCInf *clause, WamWord *w)
{
  DynPInf *dyn;
  int arity;
  WamCont codep;
  WamWord *adr;

  dyn = clause->dyn;
  arity = dyn->arity;
  adr = &A(0);

  do
    *adr++ = *w++;
  while (--arity >= 0);		/* >=0 to also restore cut register */

  debug_call = *w;

  codep = BC_Emulate_Clause(clause);
  return (codep) ? codep : Pl_BC_Emulate_Pred(glob_func, glob_dyn);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_CLAUSE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
BC_Emulate_Clause(DynCInf *clause)
{
  WamWord head_word, body_word;
  WamWord *arg_adr;
  BCWord *bc;
  int func, arity;
  int i;

  bc = (BCWord *) clause->byte_code;

  if (bc)			/* emulated code */
    return BC_Emulate_Byte_Code(bc);
				/* interpreted code */
  Pl_Copy_Clause_To_Heap(clause, &head_word, &body_word);

  arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  for (i = 0; i < arity; i++)	/* head unification */
    if (!Pl_Unify(A(i), *arg_adr++))
      goto fail;

  A(3) = A(arity);		/* NB: must be first */
  A(0) = body_word;
  A(1) = Tag_ATM(pl_atom_user); /* caller module   TODO: FIXME */
  A(2) = Tag_INT(Call_Info(func, arity, debug_call));
  return (CodePtr) Prolog_Predicate(CALL_INTERNAL_WITH_CUT, 4);

fail:
  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_BYTE_CODE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
BC_Emulate_Byte_Code(BCWord *bc)
{
  int code_op;
  BCWord w;
  int x0, x, y;
  int w1;
  PlLong l;
  WamCont codep;
  int func, arity;
  int module;
  PredInf *pred;
  C64To32 cv;
  int caller_func, caller_arity;


bc_loop:
  w = *bc++;
  switch (code_op = BC_Op(w))
    {
    case GET_X_VARIABLE:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      X(x) = X(x0);
      goto bc_loop;

    case GET_Y_VARIABLE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      Y(E, y) = X(x0);
      goto bc_loop;

    case GET_X_VALUE:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      if (!Pl_Unify(X(x), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_Y_VALUE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      if (!Pl_Unify(Y(E, y), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_ATOM:
      x0 = BC1_X0(w);
      if (!Pl_Get_Atom(BC1_Atom(w), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_ATOM_BIG:
      x0 = BC1_X0(w);
      w1 = bc->word;
      bc++;
      if (!Pl_Get_Atom(w1, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_INTEGER:
      x0 = BC1_X0(w);
      if (!Pl_Get_Integer(BC1_Int(w), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_INTEGER_BIG:
      x0 = BC1_X0(w);
#if WORD_SIZE == 32
      l = bc->word;
      bc++;
#else
      cv.u[0] = bc->word;
      bc++;
      cv.u[1] = bc->word;
      bc++;
      l = cv.l;
#endif
      if (!Pl_Get_Integer(l, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_FLOAT:
      x0 = BC1_X0(w);
      cv.u[0] = bc->word;
      bc++;
      cv.u[1] = bc->word;
      bc++;
      if (!Pl_Get_Float(cv.d, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_NIL:
      x0 = BC1_X0(w);
      if (!Pl_Get_Nil(X(x0)))
	goto fail;
      goto bc_loop;

    case GET_LIST:
      x0 = BC1_X0(w);
      if (!Pl_Get_List(X(x0)))
	goto fail;
      goto bc_loop;

    case GET_STRUCTURE:
      x0 = BC1_X0(w);
      arity = BC1_Arity(w);
      func = bc->word;
      bc++;
      if (!Pl_Get_Structure(func, arity, X(x0)))
	goto fail;
      goto bc_loop;


    case PUT_X_VARIABLE:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      X(x) = X(x0) = Pl_Put_X_Variable();
      goto bc_loop;

    case PUT_Y_VARIABLE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      X(x0) = Pl_Put_Y_Variable(&Y(E, y));
      goto bc_loop;

    case PUT_VOID:
      x0 = BC1_X0(w);
      X(x0) = Pl_Put_X_Variable();
      goto bc_loop;

    case PUT_X_VALUE:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      X(x0) = X(x);
      goto bc_loop;

    case PUT_Y_VALUE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      X(x0) = Y(E, y);
      goto bc_loop;

    case PUT_Y_UNSAFE_VALUE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      X(x0) = Pl_Put_Unsafe_Value(Y(E, y));
      goto bc_loop;

    case PUT_ATOM:
      x0 = BC1_X0(w);
      X(x0) = Pl_Put_Atom(BC1_Atom(w));
      goto bc_loop;

    case PUT_ATOM_BIG:
      x0 = BC1_X0(w);
      w1 = bc->word;
      bc++;
      X(x0) = Pl_Put_Atom(w1);
      goto bc_loop;

    case PUT_INTEGER:
      x0 = BC1_X0(w);
      X(x0) = Pl_Put_Integer(BC1_Int(w));
      goto bc_loop;

    case PUT_INTEGER_BIG:
      x0 = BC1_X0(w);
#if WORD_SIZE == 32
      l = bc->word;
      bc++;
#else
      cv.u[0] = bc->word;
      bc++;
      cv.u[1] = bc->word;
      bc++;
      l = cv.l;
#endif
      X(x0) = Pl_Put_Integer(l);
      goto bc_loop;

    case PUT_FLOAT:
      x0 = BC1_X0(w);
      cv.u[0] = bc->word;
      bc++;
      cv.u[1] = bc->word;
      bc++;
      X(x0) = Pl_Put_Float(cv.d);
      goto bc_loop;

    case PUT_NIL:
      x0 = BC1_X0(w);
      X(x0) = NIL_WORD;		/* faster than Pl_Put_Nil() */
      goto bc_loop;

    case PUT_LIST:
      x0 = BC1_X0(w);
      X(x0) = Pl_Put_List();
      goto bc_loop;

    case PUT_STRUCTURE:
      x0 = BC1_X0(w);
      arity = BC1_Arity(w);
      func = bc->word;
      bc++;
      X(x0) = Pl_Put_Structure(func, arity);
      goto bc_loop;

    case PUT_META_TERM:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      module = bc->word;
      bc++;
      X(x) = Pl_Put_Meta_Term(module, X(x0));
      goto bc_loop;

    case MATH_LOAD_X_VALUE:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      Pl_Math_Load_Value(X(x), &X(x0));
      goto bc_loop;

    case MATH_LOAD_Y_VALUE:
      x0 = BC1_X0(w);
      y = BC1_XY(w);
      Pl_Math_Load_Value(Y(E, y), &X(x0));
      goto bc_loop;

    case UNIFY_X_VARIABLE:
      x = BC2_XY(w);
      X(x) = Pl_Unify_Variable();
      goto bc_loop;

    case UNIFY_Y_VARIABLE:
      y = BC2_XY(w);
      Y(E, y) = Pl_Unify_Variable();
      goto bc_loop;

    case UNIFY_VOID:
      Pl_Unify_Void(BC2_Int(w));
      goto bc_loop;

    case UNIFY_X_VALUE:
      x = BC2_XY(w);
      if (!Pl_Unify_Value(X(x)))
	goto fail;
      goto bc_loop;

    case UNIFY_Y_VALUE:
      y = BC2_XY(w);
      if (!Pl_Unify_Value(Y(E, y)))
	goto fail;
      goto bc_loop;

    case UNIFY_X_LOCAL_VALUE:
      x = BC2_XY(w);
      if (!Pl_Unify_Local_Value(X(x)))
	goto fail;
      goto bc_loop;

    case UNIFY_Y_LOCAL_VALUE:
      y = BC2_XY(w);
      if (!Pl_Unify_Local_Value(Y(E, y)))
	goto fail;
      goto bc_loop;

    case UNIFY_ATOM:
      if (!Pl_Unify_Atom(BC2_Atom(w)))
	goto fail;
      goto bc_loop;

    case UNIFY_ATOM_BIG:
      w1 = bc->word;
      bc++;
      if (!Pl_Unify_Atom(w1))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER:
      if (!Pl_Unify_Integer(BC2_Int(w)))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER_BIG:
#if WORD_SIZE == 32
      l = bc->word;
      bc++;
#else
      cv.u[0] = bc->word;
      bc++;
      cv.u[1] = bc->word;
      bc++;
      l = cv.l;
#endif
      if (!Pl_Unify_Integer(l))
	goto fail;
      goto bc_loop;

    case UNIFY_NIL:
      if (!Pl_Unify_Nil())
	goto fail;
      goto bc_loop;

    case UNIFY_LIST:
      if (!Pl_Unify_List())
	goto fail;
      goto bc_loop;

    case UNIFY_STRUCTURE:
      arity = BC2_Arity(w);
      func = bc->word;
      bc++;
      if (!Pl_Unify_Structure(func, arity))
	goto fail;
      goto bc_loop;

    case ALLOCATE:
      Pl_Allocate(BC2_Int(w));
      goto bc_loop;

    case DEALLOCATE:
      Pl_Deallocate();
      goto bc_loop;

    case CALL:
      BCI = (WamWord) (bc + 2) | debug_call;	/* use low bit of adr */
      CP = Adjust_CP(Prolog_Predicate(BC_EMULATE_CONT, 0));
    case EXECUTE:
      module = BC2_Atom(w);
      w1 = bc->word;
      func = Functor_Of(w1);
      arity = Arity_Of(w1);
      bc++;
      if (pl_debug_call_code != NULL && debug_call && Pl_Detect_If_Aux_Name(func) == NULL)
	{
	  w1 = bc->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Prep_Debug_Call(module, func, arity, caller_func, caller_arity);
	  return pl_debug_call_code;
	}

      if ((pred = Pl_Lookup_Pred_Visible(module, func, arity)) == NULL)
	{
	  w1 = bc->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Pl_Set_Bip_Name_2(Tag_ATM(caller_func), Tag_INT(caller_arity));
	  Pl_Unknown_Pred_Error(module, func, arity);
	  goto fail;
	}

#if 0
      bc++;			/* useless since CP already set */
#endif
      glob_func = func;
      glob_dyn = (DynPInf *) (pred->dyn);
      return NULL;		/* to then call BC_Emulate_Pred */

    case CALL_NATIVE:
    case EXECUTE_NATIVE:
      module = BC2_Atom(w);
      w1 = bc->word;
      func = Functor_Of(w1);
      arity = Arity_Of(w1);
      bc++;      
      Compute_Branch_Adr(bc, codep);
      if (code_op == CALL_NATIVE)
	{
	  BCI = (WamWord) bc | debug_call; /* use low bit of adr */
	  CP = Adjust_CP(Prolog_Predicate(BC_EMULATE_CONT, 0));
	}
      if (pl_debug_call_code != NULL && debug_call)
	{
	  Prep_Debug_Call(module, func, arity, 0, 0);
	  return pl_debug_call_code;
	}
      return codep;

    case PROCEED:
      return UnAdjust_CP(CP);

    case FAIL:
      if (pl_debug_call_code != NULL && debug_call)
	{			/* invoke the debugger which will then call fail/0 */
	  Prep_Debug_Call(pl_atom_system, atom_fail, 0, 0, 0);
	  return pl_debug_call_code;
	}
      goto fail;

    case GET_CURRENT_CHOICE_X:
      x = BC2_XY(w);
      X(x) = Pl_Get_Current_Choice();
      goto bc_loop;

    case GET_CURRENT_CHOICE_Y:
      y = BC2_XY(w);
      Y(E, y) = Pl_Get_Current_Choice();
      goto bc_loop;

    case CUT_X:
      x = BC2_XY(w);
      Pl_Cut(X(x));
      goto bc_loop;

    case CUT_Y:
      y = BC2_XY(w);
      Pl_Cut(Y(E, y));
      goto bc_loop;

    case SOFT_CUT_X:
      x = BC2_XY(w);
      Pl_Soft_Cut(X(x));
      goto bc_loop;

    case SOFT_CUT_Y:
      y = BC2_XY(w);
      Pl_Soft_Cut(Y(E, y));
      goto bc_loop;
    }

fail:
  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * PL_BC_EMULATE_CONT_0                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Emulate_Cont_0(void)
{
  WamCont codep;
  BCWord *bc;

  debug_call = BCI & 1;
  bc = (BCWord *) ((BCI >> 1) << 1);

  codep = BC_Emulate_Byte_Code(bc);
  return (codep) ? codep : Pl_BC_Emulate_Pred(glob_func, glob_dyn);
}




/*-------------------------------------------------------------------------*
 * PREP_DEBUG_CALL                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Prep_Debug_Call(int module, int func, int arity, int caller_func, int caller_arity)
{
  int i;
  WamWord word;

  if (arity == 0)
    A(0) = Tag_ATM(func);
  else
    {
      word = Tag_STC(H);
      Global_Push(Functor_Arity(func, arity));

      for (i = 0; i < arity; i++)
	Global_Push(A(i));

      A(0) = word;
    }
  A(1) = Tag_ATM(module);
  A(2) = Tag_INT(Call_Info(caller_func, caller_arity, debug_call));
}
