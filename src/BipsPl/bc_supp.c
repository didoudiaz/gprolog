/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bc_supp.c                                                       *
 * Descr.: byte-code support                                               *
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

#include <stdio.h>
#include <stdlib.h>

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

#define MAX_OP                     256
#define BC_BLOCK_SIZE              1024

#define ERR_UNKNOWN_INSTRUCTION    "bc_supp: Unknown wam instruction: %s"




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
  PUT_INTEGER,
  PUT_INTEGER_BIG,
  PUT_FLOAT,
  PUT_NIL,
  PUT_LIST,
  PUT_STRUCTURE,

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

  CUT_X,
  CUT_Y
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
  unsigned u[2];
}
DblUns;


#if WORD_SIZE == 64

typedef union
{
  int *p;
  unsigned u[2];
}
PtrUns;

#endif




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
static int atom_built_in;
static int atom_built_in_fd;
static int atom_fail;

static int caller_func;
static int caller_arity;

static int glob_func;
static DynPInf *glob_dyn;
static Bool debug_call;

WamCont debug_call_code;	/* overwritten by debugger_c.c */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Find_Inst_Code_Op(int inst);

static int Compar_Inst_Code_Op(BCWord *w1, BCWord *w2);

static int BC_Arg_X_Or_Y(WamWord arg_word, int *op);

static int BC_Arg_Func_Arity(WamWord arg_word, int *arity);



WamCont BC_Emulate_Pred(int func, DynPInf *dyn);

static WamCont BC_Emulate_Pred_Alt(DynCInf *clause, WamWord *w);

static WamCont BC_Emulate_Clause(DynCInf *clause);

static WamCont BC_Emulate_Byte_Code(BCWord *bc);

static void Prep_Debug_Call(int func, int arity, int caller_func,
			    int caller_arity);



#define BC_EMULATE_CONT            X2462635F656D756C6174655F636F6E74

#define CALL_INTERNAL_WITH_CUT     X2463616C6C5F696E7465726E616C5F776974685F637574

Prolog_Prototype(BC_EMULATE_CONT, 0);
Prolog_Prototype(CALL_INTERNAL_WITH_CUT, 3);

#define BC_Op(w)                   ((w).t1.code_op)

#define BC_X0(w)                   ((w).t1.i8)

#define BC_Ari(w)                  ((w).t1.i8)

#define BC_XY(w)                   ((w).t1.i16)

#define BC_X(w)                    ((w).t1.i16)

#define BC_Y(w)                    ((w).t1.i16)

#define BC_Atm(w)                  ((w).t1.i16)

#define BC_Int(w)                  ((w).t1.i16)

#define BC_Fun(w)                  ((w).t1.i16)

#define BC_Adr(w)                  ((w).t2.i24)

#define Fit_In_16bits(n)           ((unsigned) (n) < (1<<16))

#define Fit_In_24bits(n)           ((unsigned) (n) < (1<<24))

#define Op_In_Tbl(str, op)  BC_Op(*p)=op; BC_Atm(*p) = Create_Atom(str); p++

#if WORD_SIZE == 32

#define Compute_Branch_Adr(bc, codep)   \
{                                       \
  codep = (WamCont) (bc->word); bc++;   \
}

#else

#define Compute_Branch_Adr(bc, codep)     \
{                                         \
  PtrUns pu;                              \
  pu.u[0] = (unsigned) (bc->word); bc++;  \
  pu.u[1] = (unsigned) (bc->word); bc++;  \
  codep = (WamCont) (pu.p);               \
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

  Op_In_Tbl("cut", CUT_X);

  nb_op = p - op_tbl;

  qsort(op_tbl, nb_op, sizeof(op_tbl[0]),
	(int (*)(const void *, const void *)) Compar_Inst_Code_Op);


  bc_nb_block = 1;
  bc = (BCWord *) Malloc(bc_nb_block * BC_BLOCK_SIZE * sizeof(BCWord));

  atom_dynamic = Create_Atom("dynamic");
  atom_public = Create_Atom("public");
  atom_built_in = Create_Atom("built_in");
  atom_built_in_fd = Create_Atom("built_in_fd");
  atom_fail = Create_Atom("fail");
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

  BC_Atm(w) = inst;
  p = (BCWord *) bsearch(&w, op_tbl, nb_op, sizeof(op_tbl[0]),
			 (int (*)(const void *, const void *))
			 Compar_Inst_Code_Op);
  if (p == NULL)
    Fatal_Error(ERR_UNKNOWN_INSTRUCTION, atom_tbl[inst].name);

  return BC_Op(*p);
}




/*-------------------------------------------------------------------------*
 * COMPAR_INST_CODE_OP                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Compar_Inst_Code_Op(BCWord *p1, BCWord *p2)
{
  return BC_Atm(*p1) - BC_Atm(*p2);
}




/*-------------------------------------------------------------------------*
 * BC_START_PRED_7                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BC_Start_Pred_7(WamWord func_word, WamWord arity_word,
		WamWord pl_file_word, WamWord pl_line_word,
		WamWord sta_dyn_word, WamWord pub_priv_word,
		WamWord us_blp_bfd_word)
{
  int func, arity;
  int pl_file, pl_line;
  int prop = 0;
  int atom;
  PredInf *pred;

  func = Rd_Atom_Check(func_word);
  arity = Rd_Integer_Check(arity_word);
  pl_file = Rd_Atom_Check(pl_file_word);
  pl_line = Rd_Integer_Check(pl_line_word);

  if (Rd_Atom_Check(sta_dyn_word) == atom_dynamic)
    prop = MASK_PRED_DYNAMIC | MASK_PRED_PUBLIC;
  else if (Rd_Atom_Check(pub_priv_word) == atom_public)
    prop = MASK_PRED_PUBLIC;

  atom = Rd_Atom_Check(us_blp_bfd_word);
  if (atom == atom_built_in)
    prop |= MASK_PRED_BUILTIN;
  else if (atom == atom_built_in_fd)
    prop |= MASK_PRED_BUILTIN_FD;


  pred = Update_Dynamic_Pred(func, arity, 0);
  if (pred == NULL)
    pred = Create_Pred(func, arity, pl_file, pl_line, prop, NULL);
  else
    {
      pred->pl_file = pl_file;
      pred->pl_line = pl_line;
      pred->prop = prop;
    }

#if 1
  caller_func = Pred_Without_Aux(func, arity, &caller_arity);
#else
  caller_func = func;
  caller_arity = arity;
#endif

#ifdef DEBUG
  DBGPRINTF("BC start %s/%d\n", atom_tbl[func].name, arity);
#endif
}




/*-------------------------------------------------------------------------*
 * BC_START_EMIT_0                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BC_Start_Emit_0(void)
{
  bc_sp = bc;
}




/*-------------------------------------------------------------------------*
 * BC_STOP_EMIT_0                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BC_Stop_Emit_0(void)
{
  int i;

  byte_len = bc_sp - bc;

#ifdef DEBUG
  DBGPRINTF("byte-code size:%d\n", byte_len);
#endif

  byte_code = (unsigned *) Malloc(byte_len * sizeof(BCWord));

  for (i = 0; i < byte_len; i++)
    byte_code[i] = bc[i].word;
}




/*-------------------------------------------------------------------------*
 * BC_EMIT_INST_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BC_Emit_Inst_1(WamWord inst_word)
{
  int func, arity;
  WamWord *arg_adr;
  int op;
  int size_bc;
  BCWord w;
  unsigned w1, w2;
  int nb_word;
  DblUns du;

#if WORD_SIZE == 64
  PtrUns pu;
#endif
  PredInf *pred;

  arg_adr = Rd_Callable_Check(inst_word, &func, &arity);

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
      BC_XY(w) = BC_Arg_X_Or_Y(*arg_adr++, &op);
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;

    case GET_ATOM:
    case PUT_ATOM:
      BC_Atm(w) = Rd_Atom(*arg_adr++);
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;

    case GET_INTEGER:
    case PUT_INTEGER:
      w1 = Rd_Integer(*arg_adr++);
      if (Fit_In_16bits(w1))
	BC_Int(w) = w1;
      else
	{
	  op++;
	  nb_word = 2;
	}
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;

    case GET_FLOAT:
    case PUT_FLOAT:
      nb_word = 3;
      du.d = Rd_Float(*arg_adr++);
      BC_X0(w) = Rd_Integer(*arg_adr);
      w1 = du.u[0];
      w2 = du.u[1];
      break;

    case GET_NIL:
    case GET_LIST:
    case PUT_NIL:
    case PUT_LIST:
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;

    case GET_STRUCTURE:
    case PUT_STRUCTURE:
      nb_word = 2;
      BC_Fun(w) = BC_Arg_Func_Arity(*arg_adr++, &w1);
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;

    case PUT_VOID:
      BC_X0(w) = Rd_Integer(*arg_adr);
      break;


    case UNIFY_X_VARIABLE:
    case UNIFY_X_VALUE:
    case UNIFY_X_LOCAL_VALUE:
    case CUT_X:
      BC_XY(w) = BC_Arg_X_Or_Y(*arg_adr, &op);
      break;

    case UNIFY_ATOM:
      BC_Atm(w) = Rd_Atom(*arg_adr);
      break;

    case UNIFY_INTEGER:
      w1 = Rd_Integer(*arg_adr++);
      if (Fit_In_16bits(w1))
	BC_Int(w) = w1;
      else
	{
	  op++;
	  nb_word = 2;
	}
      break;

    case UNIFY_STRUCTURE:
      BC_Fun(w) = BC_Arg_Func_Arity(*arg_adr++, &w1);
      BC_Ari(w) = w1;
      break;

    case UNIFY_VOID:
    case ALLOCATE:
      BC_Int(w) = Rd_Integer(*arg_adr);
      break;


    case CALL:
    case EXECUTE:
      BC_Fun(w) = func = BC_Arg_Func_Arity(*arg_adr++, &arity);
      BC_Ari(w) = arity;
      pred = Lookup_Pred(func, arity);
      if (pred && (pred->prop & MASK_PRED_NATIVE_CODE))
	{
	  op++;
#if WORD_SIZE == 32
	  nb_word = 2;
	  w1 = (unsigned) (pred->codep);
#else
	  nb_word = 3;
	  pu.p = (int *) (pred->codep);
	  w1 = pu.u[0];
	  w2 = pu.u[1];
#endif
	}
      else
	{
	  nb_word = 2;
	  w1 = (unsigned) Functor_Arity(caller_func, caller_arity);
	}
      break;
    }

  BC_Op(w) = op;

  *bc_sp++ = w;
  if (nb_word >= 2)
    {
      bc_sp->word = w1;
      bc_sp++;

      if (nb_word >= 3)
	{
	  bc_sp->word = w2;
	  bc_sp++;
	}
    }


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

  Write_Simple(inst_word);
  DBGPRINTF("\n");
#endif
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

  return Rd_Integer(Arg(adr, 0));
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
 * Part II. Byte-Code emulation                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 * BC_CALL_TERMINAL_PRED_3                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
BC_Call_Terminal_Pred_3(WamWord pred_word, WamWord call_info_word,
			WamWord first_call_word)
{
  int func, arity;
  WamWord *arg_adr;
  PredInf *pred;
  int i;

  arg_adr = Rd_Callable_Check(pred_word, &func, &arity);

  debug_call = (call_info_word & (1 << TAG_SIZE_LOW)) != 0;

  if (debug_call_code != NULL && debug_call && 
      (first_call_word & (1 << TAG_SIZE_LOW)))
    {
      A(0) = pred_word;
      A(1) = call_info_word;
      return debug_call_code;
    }

  pred = Lookup_Pred(func, arity);
  if (pred == NULL)
    {				/* case: fail/0 from '$call_from_debugger' */
      if (func != atom_fail || arity != 0)
	{
	  Call_Info_Bip_Name_1(call_info_word);
	  Unknown_Pred_Error(func, arity);
	}
      return ALTB(B);		/* i.e. fail */
    }

  for (i = 0; i < arity; i++)
    A(i) = *arg_adr++;

  if (pred->prop & MASK_PRED_NATIVE_CODE)	/* native code */
    return (WamCont) (pred->codep);

  return BC_Emulate_Pred(func, (DynPInf *) (pred->dyn));
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_PRED                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
BC_Emulate_Pred(int func, DynPInf *dyn)
{
  DynCInf *clause;
  WamCont codep;
  int arity;

start:
  if (dyn == NULL)
    goto fail;

  arity = dyn->arity;
  Load_Cut_Level(&A(arity));	/* init cut register */
  A(arity + 1) = debug_call;

  clause = Scan_Dynamic_Pred(func, arity, dyn, A(0),
			     (long (*)()) BC_Emulate_Pred_Alt,
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
  return (codep) ? codep : BC_Emulate_Pred(glob_func, glob_dyn);
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
  Copy_Clause_To_Heap(clause, &head_word, &body_word);

  arg_adr = Rd_Callable_Check(head_word, &func, &arity);

  for (i = 0; i < arity; i++)	/* head unification */
    if (!Unify(A(i), *arg_adr++))
      goto fail;

  A(2) = A(arity);		/* before since pb with cut if arity <= 1 */
  A(0) = body_word;
  A(1) = Tag_INT(Call_Info(func, arity, debug_call));
  return (CodePtr) Prolog_Predicate(CALL_INTERNAL_WITH_CUT, 3);

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
  BCWord w;
  int x0, x, y;
  int w1;
  WamCont codep;
  int func, arity;
  PredInf *pred;
  DblUns du;


bc_loop:
  w = *bc++;
  switch (BC_Op(w))
    {
    case GET_X_VARIABLE:
      x0 = BC_X0(w);
      x = BC_XY(w);
      X(x) = X(x0);
      goto bc_loop;

    case GET_Y_VARIABLE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      Y(E, y) = X(x0);
      goto bc_loop;

    case GET_X_VALUE:
      x0 = BC_X0(w);
      x = BC_XY(w);
      if (!Unify(X(x), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_Y_VALUE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      if (!Unify(Y(E, y), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_ATOM:
      x0 = BC_X0(w);
      if (!Get_Atom(BC_Atm(w), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_INTEGER:
      x0 = BC_X0(w);
      if (!Get_Integer(BC_Int(w), X(x0)))
	goto fail;
      goto bc_loop;

    case GET_INTEGER_BIG:
      x0 = BC_X0(w);
      w1 = bc->word;
      bc++;
      if (!Get_Integer(w1, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_FLOAT:
      x0 = BC_X0(w);
      du.u[0] = bc->word;
      bc++;
      du.u[1] = bc->word;
      bc++;
      if (!Get_Float(du.d, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_NIL:
      x0 = BC_X0(w);
      if (!Get_Nil(X(x0)))
	goto fail;
      goto bc_loop;

    case GET_LIST:
      x0 = BC_X0(w);
      if (!Get_List(X(x0)))
	goto fail;
      goto bc_loop;

    case GET_STRUCTURE:
      x0 = BC_X0(w);
      func = BC_Fun(w);
      arity = bc->word;
      bc++;
      if (!Get_Structure(func, arity, X(x0)))
	goto fail;
      goto bc_loop;


    case PUT_X_VARIABLE:
      x0 = BC_X0(w);
      x = BC_XY(w);
      X(x) = X(x0) = Put_X_Variable();
      goto bc_loop;

    case PUT_Y_VARIABLE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      X(x0) = Put_Y_Variable(&Y(E, y));
      goto bc_loop;

    case PUT_VOID:
      x0 = BC_X0(w);
      X(x0) = Put_X_Variable();
      goto bc_loop;

    case PUT_X_VALUE:
      x0 = BC_X0(w);
      x = BC_XY(w);
      X(x0) = X(x);
      goto bc_loop;

    case PUT_Y_VALUE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      X(x0) = Y(E, y);
      goto bc_loop;

    case PUT_Y_UNSAFE_VALUE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      X(x0) = Put_Unsafe_Value(Y(E, y));
      goto bc_loop;

    case PUT_ATOM:
      x0 = BC_X0(w);
      X(x0) = Put_Atom(BC_Atm(w));
      goto bc_loop;

    case PUT_INTEGER:
      x0 = BC_X0(w);
      X(x0) = Put_Integer(BC_Int(w));
      goto bc_loop;

    case PUT_INTEGER_BIG:
      x0 = BC_X0(w);
      w1 = bc->word;
      bc++;
      X(x0) = Put_Integer(w1);
      goto bc_loop;

    case PUT_FLOAT:
      x0 = BC_X0(w);
      du.u[0] = bc->word;
      bc++;
      du.u[1] = bc->word;
      bc++;
      X(x0) = Put_Float(du.d);
      goto bc_loop;

    case PUT_NIL:
      x0 = BC_X0(w);
      X(x0) = NIL_WORD;		/* faster than Put_Nil() */
      goto bc_loop;

    case PUT_LIST:
      x0 = BC_X0(w);
      X(x0) = Put_List();
      goto bc_loop;

    case PUT_STRUCTURE:
      x0 = BC_X0(w);
      func = BC_Fun(w);
      arity = bc->word;
      bc++;
      X(x0) = Put_Structure(func, arity);
      goto bc_loop;

    case MATH_LOAD_X_VALUE:
      x0 = BC_X0(w);
      x = BC_XY(w);
      Math_Load_Value(X(x), &X(x0));
      goto bc_loop;

    case MATH_LOAD_Y_VALUE:
      x0 = BC_X0(w);
      y = BC_XY(w);
      Math_Load_Value(Y(E, y), &X(x0));
      goto bc_loop;

    case UNIFY_X_VARIABLE:
      x = BC_XY(w);
      X(x) = Unify_Variable();
      goto bc_loop;

    case UNIFY_Y_VARIABLE:
      y = BC_XY(w);
      Y(E, y) = Unify_Variable();
      goto bc_loop;

    case UNIFY_VOID:
      Unify_Void(BC_Int(w));
      goto bc_loop;

    case UNIFY_X_VALUE:
      x = BC_XY(w);
      if (!Unify_Value(X(x)))
	goto fail;
      goto bc_loop;

    case UNIFY_Y_VALUE:
      y = BC_XY(w);
      if (!Unify_Value(Y(E, y)))
	goto fail;
      goto bc_loop;

    case UNIFY_X_LOCAL_VALUE:
      x = BC_XY(w);
      if (!Unify_Local_Value(X(x)))
	goto fail;
      goto bc_loop;

    case UNIFY_Y_LOCAL_VALUE:
      y = BC_XY(w);
      if (!Unify_Local_Value(Y(E, y)))
	goto fail;
      goto bc_loop;

    case UNIFY_ATOM:
      if (!Unify_Atom(BC_Atm(w)))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER:
      if (!Unify_Integer(BC_Int(w)))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER_BIG:
      w1 = bc->word;
      bc++;
      if (!Unify_Integer(w1))
	goto fail;
      goto bc_loop;

    case UNIFY_NIL:
      if (!Unify_Nil())
	goto fail;
      goto bc_loop;

    case UNIFY_LIST:
      if (!Unify_List())
	goto fail;
      goto bc_loop;

    case UNIFY_STRUCTURE:
      func = BC_Fun(w);
      arity = BC_Ari(w);
      if (!Unify_Structure(func, arity))
	goto fail;
      goto bc_loop;

    case ALLOCATE:
      Allocate(BC_Int(w));
      goto bc_loop;

    case DEALLOCATE:
      Deallocate();
      goto bc_loop;

    case CALL:
      BCI = (WamWord) (bc + 1) | debug_call;	/* use low bit of adr */
      CP = Adjust_CP(Prolog_Predicate(BC_EMULATE_CONT, 0));
    case EXECUTE:
      func = BC_Fun(w);
      arity = BC_Ari(w);
      if (debug_call_code != NULL && debug_call &&
	  Detect_If_Aux_Name(func) == NULL)
	{
	  w1 = bc->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Prep_Debug_Call(func, arity, caller_func, caller_arity);
	  return debug_call_code;
	}

      if ((pred = Lookup_Pred(func, arity)) == NULL)
	{
	  w1 = bc->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Set_Bip_Name_2(Tag_ATM(caller_func),
			 Tag_INT(caller_arity));
	  Unknown_Pred_Error(func, arity);
	  goto fail;
	}

#if 0
      bc++;			/* useless since CP already set */
#endif
      glob_func = func;
      glob_dyn = (DynPInf *) (pred->dyn);
      return NULL;		/* to then call BC_Emulate_Pred */

    case CALL_NATIVE:
      Compute_Branch_Adr(bc, codep);
      BCI = (WamWord) bc | debug_call;
      CP = Adjust_CP(Prolog_Predicate(BC_EMULATE_CONT, 0));
      if (debug_call_code != NULL && debug_call)
	{
	  func = BC_Fun(w);
	  arity = BC_Ari(w);
	  Prep_Debug_Call(func, arity, 0, 0);
	  return debug_call_code;
	}
      return codep;

    case EXECUTE_NATIVE:
      Compute_Branch_Adr(bc, codep);
      if (debug_call_code != NULL && debug_call)
	{
	  func = BC_Fun(w);
	  arity = BC_Ari(w);
	  Prep_Debug_Call(func, arity, 0, 0);
	  return debug_call_code;
	}
      return codep;

    case PROCEED:
      return UnAdjust_CP(CP);

    case FAIL:
      if (debug_call_code != NULL && debug_call)
	{			/* invoke the debugger that will then call fail/0 */
	  Prep_Debug_Call(atom_fail, 0, 0, 0);
	  return debug_call_code;
	}
      goto fail;

    case CUT_X:
      x = BC_XY(w);
      Cut(X(x));
      goto bc_loop;

    case CUT_Y:
      y = BC_XY(w);
      Cut(Y(E, y));
      goto bc_loop;
    }

fail:
  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_CONT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
BC_Emulate_Cont_0(void)
{
  WamCont codep;
  BCWord *bc;

  debug_call = BCI & 1;
  bc = (BCWord *) ((BCI >> 1) << 1);

  codep = BC_Emulate_Byte_Code(bc);
  return (codep) ? codep : BC_Emulate_Pred(glob_func, glob_dyn);
}




/*-------------------------------------------------------------------------*
 * PREP_DEBUG_CALL                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Prep_Debug_Call(int func, int arity, int caller_func, int caller_arity)
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
  A(1) = Tag_INT(Call_Info(caller_func, caller_arity, debug_call));
}
