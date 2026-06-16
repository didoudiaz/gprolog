/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bc_supp.c                                                       *
 * Descr.: byte-code support                                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
//  PUT_META_TERM,

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
  uint32_t word;
}
BCWord;



typedef union
{
  double d;
#if WORD_SIZE == 64
  int *p;
  PlLong l; /* same size as uint64_t */
#endif
  uint32_t u[2];
}
C64To32;





/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static BCWord op_tbl[MAX_OP];
static int nb_op;

static BCWord *bc_buff;
static BCWord *bc_sp;
static int bc_nb_block;

static int atom_dynamic;
static int atom_public;
static int atom_multifile;
static int atom_built_in;
static int atom_built_in_fd;
static int atom_fail;

static int glob_caller_func;
static int glob_caller_arity;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Find_Inst_Code_Op(int inst);

static int Compar_Inst_Code_Op(const void *p1, const void *p2);

static int BC_Arg_X_Or_Y(WamWord arg_word, int *op);

static int BC_Arg_Func_Arity(WamWord arg_word, int *arity);




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

#define Op_In_Tbl(str, op) \
  BC_Op(*p) = op; BC2_Atom(*p) = Pl_Create_Atom(str); p++




/*-------------------------------------------------------------------------*
 * BYTE_CODE_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PL_INITIALIZER(Byte_Code_Initializer)
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
//  Op_In_Tbl("put_meta_term", PUT_META_TERM);
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

  nb_op = (int) (p - op_tbl);

  qsort(op_tbl, nb_op, sizeof(op_tbl[0]),
	(int (*)(const void *, const void *)) Compar_Inst_Code_Op);

  bc_nb_block = 1;
  bc_buff = (BCWord *) Malloc(bc_nb_block * BC_BLOCK_SIZE * sizeof(BCWord));

  atom_dynamic = Pl_Create_Atom("dynamic");
  atom_public = Pl_Create_Atom("public");
  atom_multifile = Pl_Create_Atom("multifile");
  atom_built_in = Pl_Create_Atom("built_in");
  atom_built_in_fd = Pl_Create_Atom("built_in_fd");
  atom_fail = Pl_Create_Atom("fail");
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
Compar_Inst_Code_Op(const void *p1, const void *p2)
{
  BCWord w1 = *(BCWord *) p1;
  BCWord w2 = *(BCWord *) p2;
  return BC2_Atom(w1) - BC2_Atom(w2);
}




/*-------------------------------------------------------------------------*
 * PL_BC_START_PRED_8                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Start_Pred_8(WamWord func_word, WamWord arity_word,
		   WamWord pl_file_word, WamWord pl_line_word,
		   WamWord sta_dyn_word, WamWord pub_priv_word,
		   WamWord mono_multi_word, WamWord us_blp_bfd_word)
{
  int func, arity;
  int pl_file, pl_line;
  int prop = 0;
  int atom;
  int multi = 0;
  PredInf *pred;

  func = Pl_Rd_Atom_Check(func_word);
  arity = Pl_Rd_C_Int_Check(arity_word);
  pl_file = Pl_Rd_Atom_Check(pl_file_word);
  pl_line = Pl_Rd_C_Int_Check(pl_line_word);

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


  pred = Pl_Update_Dynamic_Pred(func, arity, 0, (multi) ? pl_file : -pl_file);
  if (pred == NULL)
    pred = Pl_Create_Pred(func, arity, pl_file, pl_line, prop, NULL);
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

#if 1
  glob_caller_func = Pl_Pred_Without_Aux(func, arity, &glob_caller_arity);
#else
  glob_caller_func = func;
  glob_caller_arity = arity;
#endif

#ifdef DEBUG
  DBGPRINTF("\nBC start for %s/%d\n", pl_atom_tbl[func].name, arity);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_BC_START_EMIT_0                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Start_Emit_0(void)
{
  bc_sp = bc_buff;
}




/*-------------------------------------------------------------------------*
 * PL_BC_STOP_EMIT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Stop_Emit_0(void)
{
  int pl_byte_code_len;	/* could be declared in bc_supp.h if needed elsewhere */
  int bc_len_bytes;
  int i;
  
  pl_byte_code_len = (int) (bc_sp - bc_buff);
  bc_len_bytes = pl_byte_code_len * sizeof(BCWord);
 
  pl_byte_code = Malloc(bc_len_bytes);

#if 0
  memcpy(pl_byte_code, bc_buff, bc_len_bytes);
#else
  for (i = 0; i < pl_byte_code_len; i++)
    pl_byte_code[i] = bc_buff[i].word;
#endif

#ifdef DEBUG
  DBGPRINTF(" byte-code at: %p  bc size: %d words (%d bytes)\n",
	    pl_byte_code, pl_byte_code_len, bc_len_bytes);
#endif
}



#ifdef DEBUG
#define DBG_PRINT_INST(iw, bc_sp, op, nb_word, w, w1, w2, w3)	\
  DBGPRINTF("%6d: op: %3d (%2x)  bc_buff: %010x  ",		\
	    (int) ((char *) bc_sp - (char *) bc_buff),		\
	    op, op, w.word);					\
								\
  if (nb_word >= 2)						\
    DBGPRINTF("%010x  ", w1);					\
  else								\
    DBGPRINTF("            ");					\
								\
  if (nb_word >= 3)						\
    DBGPRINTF("%010x  ", w2);					\
  else								\
    DBGPRINTF("            ");					\
								\
  if (nb_word >= 4)						\
    DBGPRINTF("%010x  ", w3);					\
  else								\
    DBGPRINTF("            ");					\
								\
  if (iw != NOT_A_WAM_WORD)					\
    Pl_Write(iw);						\
  DBGPRINTF("\n")
#else
#define DBG_PRINT_INST(bc_sp, op, nb_word, w, w1, w2, w3, iw)
#endif


#define ASSEMBLE_INST(bc_sp, op, nb_word, w, w1, w2, w3, iw)	\
  BC_Op(w) = op;						\
  DBG_PRINT_INST(iw, bc_sp, op, nb_word, w, w1, w2, w3);	\
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
  int func, arity;
  WamWord *arg_adr;
  int op;
  int size_bc;
  BCWord w;			/* code-op word */
  unsigned w1 = 0, w2 = 0, w3 = 0; /* additional words (init for the compiler)*/
  PlLong l;
  int nb_word;
  C64To32 cv;

  PredInf *pred;

  arg_adr = Pl_Rd_Callable_Check(inst_word, &func, &arity);

  op = Find_Inst_Code_Op(func);

  size_bc = (int) (bc_sp - bc_buff);
  if (size_bc + 3 >= bc_nb_block * BC_BLOCK_SIZE)
    {
      bc_nb_block++;
      bc_buff = Realloc(bc_buff, bc_nb_block * BC_BLOCK_SIZE * sizeof(BCWord));
      bc_sp = bc_buff + size_bc;
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
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
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
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
      break;

    case GET_INTEGER:
    case PUT_INTEGER:
      l = Pl_Rd_Integer(*arg_adr++);
      if (Fit_In_16bits(l))
	BC1_Atom(w) = (int) l;
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
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
      break;

    case GET_FLOAT:
    case PUT_FLOAT:
      nb_word = 3;
      cv.d = Pl_Rd_Float(*arg_adr++);
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
      w1 = cv.u[0];
      w2 = cv.u[1];
      break;

    case GET_NIL:
    case GET_LIST:
    case PUT_NIL:
    case PUT_LIST:
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
      break;

    case GET_STRUCTURE:
    case PUT_STRUCTURE:
      nb_word = 2;
      w1 = BC_Arg_Func_Arity(*arg_adr++, &arity);
      BC1_Arity(w) = arity;
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
      break;
/*
    case PUT_META_TERM:
      nb_word = 2;
      w1 = Pl_Rd_Atom(*arg_adr++);
      BC1_X0(w) = Pl_Rd_Integer(*arg_adr++);
      BC1_XY(w) = Pl_Rd_Integer(*arg_adr);
      break;
*/
    case PUT_VOID:
      BC1_X0(w) = Pl_Rd_C_Int(*arg_adr);
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
	BC2_Int(w) = (int) l;
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
      BC2_Int(w) = Pl_Rd_C_Int(*arg_adr);
      break;


    case CALL:
    case EXECUTE:
      w1 = func = BC_Arg_Func_Arity(*arg_adr++, &arity);
      BC2_Arity(w) = arity;
      pred = Pl_Lookup_Pred(func, arity);
      if (pred && (pred->prop & MASK_PRED_NATIVE_CODE))
	{
	  op++;			/* change op by op_NATIVE */
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

  ASSEMBLE_INST(bc_sp, op, nb_word, w, w1, w2, w3, inst_word);
}



/*-------------------------------------------------------------------------*
 * PL_BC_EMIT_INST_EXECUTE_NATIVE                                          *
 *                                                                         *
 * This function is called by the compiled code for dynamic or multifile   *
 * predicate. Each clause has been compiled to native code (aux pred).     *
 * We here create a call to this clause.                                   *
 * This function is called between Pl_BC_Start_Emit_0 and Pl_BC_Stop_Emit_0*
 * NB: bc_buff has always enough room for our 3 or 4 words.                *
 *-------------------------------------------------------------------------*/
void
Pl_BC_Emit_Inst_Execute_Native(int func, int arity, PlLong *codep)
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

#ifdef DEBUG
  DBGPRINTF("\nBC Wrapper: execute native %s/%d, code at %p\n",
	    pl_atom_tbl[func].name, arity, codep);
#endif

  ASSEMBLE_INST(bc_sp, EXECUTE_NATIVE, nb_word, w, w1, w2, w3, NOT_A_WAM_WORD);
}




/*-------------------------------------------------------------------------*
 * PL_EMIT_BC_EXECUTE_WRAPPER                                              *
 *                                                                         *
 * Called by pl2wam for each clause of a dynamic or multifile predicate    *
 * Each clause has been compiled to native code (aux pred).                *
 * We here create a call to this clause.                                   *
 *-------------------------------------------------------------------------*/
void
Pl_Emit_BC_Execute_Wrapper(int func, int arity, PlLong *codep)
{
  Pl_BC_Start_Emit_0();
  Pl_BC_Emit_Inst_Execute_Native(func, arity, codep);
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

  return Pl_Rd_C_Int(Arg(adr, 0));
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
  *arity = (int) UnTag_INT(word);

  DEREF(Arg(stc_adr, 0), word, tag_mask);	/* functor */
  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * Part II. Byte-Code emulation                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/*
 * This byte-code (BC) emulator handles the execution of the WAM code of a
 * dynamic clause (clause selection by indexing is performed beforehand by
 * the dynamic code manager; see dynam_supp.c).
 *
 * The BC emulator requires specific information, at minimum the byte-code
 * program counter (BC PC) and a boolean indicating whether execution is
 * in debug_call mode (for interaction with the debugger). In an initial
 * version, this information was grouped into a new WAM register (BCI),
 * which therefore had to be saved in NC frames (environments and
 * choicepoints).
 *
 * With the introduction of GC-clause (see dynam_supp.c), it is also
 * necessary to detect clauses whose byte-code is currently executing, in
 * order to delay their deletion. This detection is performed by scanning
 * the local stack, which requires reliably identifying the environments
 * associated with BC execution and the corresponding clause. Introducing
 * new WAM registers or extending NC frames is not desirable.
 *
 * The adopted approach therefore keeps the WAM for native code (NC)
 * strictly unchanged: NC frames (environments and choicepoints) are not
 * extended. The information specific to BC execution is stored in the
 * first permanent variables of the environment.
 *
 * To reliably recognize a BC frame during GC-clause, CPE(E) is
 * temporarily replaced by a sentinel code address. The original CPE(E)
 * must therefore be saved when the environment is created (allocate) and
 * restored when it is destroyed (deallocate). The clause associated with
 * the currently executing byte-code must also be recorded.
 *
 * Three permanent variables are thus reserved for BC execution:
 *
 *   - Y_SAVE_CPE(e): saved value of CP(E)
 *   - Y_CLAUSE(e)  : pointer to the clause associated with the running BC
 *   - Y_BCPC_DBG(e): BC PC with the debug_call flag encoded in bit 0
 *
 * Macros:
 *
 *   - BC_FRAME_BEGIN: executed immediately after ALLOCATE to save CP(E)
 *     and replace CPE(E) with the sentinel.
 *
 *   - BC_FRAME_END: executed immediately before DEALLOCATE to restore
 *     CPE(E).
 *
 *   - BC_BEFORE_CALL: executed just before a CALL to save the BC
 *     continuation (BC PC + debug_call) and set CP to a native
 *     continuation that restarts the emulator (BC_EMULATE_CONT_0, which
 *     calls the trampoline function BC_Emulate_Cont_0).
 *
 *   - BC_AFTER_CALL: executed in BC_Emulate_Cont_0 to restore the BC PC
 *     and the debug_call mode, and then resume byte-code execution.
 *
 * When modifying Y_BCPC_DBG(e), it may be necessary to trail it in order 
 * to restore its value upon backtracking. Currently, it is possible that
 * several trailings occur (only one trail per choicepoint is necessary). In
 * practice, these multiple trails are rare and shallow (dbg_check_Trail).
 * If needed, the same timestamp technique used for the FD solver could be
 * applied. This would require one additional Y variable to store the STAMP.
 *
 * The functions that manage execution return a native code address
 * (WamCont) to which control must be transferred, or NULL in the case of
 * a call to an interpreted predicate.
 */


static DynPInf *glob_dyn;   /* BC_Emulate_Byte_Code to return multiple values */
static Bool debug_call;	    /* debug call ? (for the whole clause byte-code) */

WamCont pl_debug_call_code;	/* overwritten by debugger_c.c */


static PlLong BC_Emulate_Pred_Alt(DynCInf *clause, WamWord *w, Bool is_last);

static WamCont BC_Emulate_Clause(DynCInf *clause);

static WamCont BC_Emulate_Byte_Code(BCWord *bc_start, DynCInf *clause);

static void Prep_Debug_Call(int func, int arity, int caller_func,
			    int caller_arity);




#define BC_EMULATE_CONT        X1_2462635F656D756C6174655F636F6E74
#define CALL_INTERNAL_WITH_CUT X1_2463616C6C5F696E7465726E616C5F776974685F637574

Prolog_Prototype(BC_EMULATE_CONT, 0);
Prolog_Prototype(CALL_INTERNAL_WITH_CUT, 3);

#define Y_SAVE_CPE(e) Y(e, 0)
#define Y_CLAUSE(e)   Y(e, 1)
#define Y_BCPC_DBG(e) Y(e, 2)


#define BC_GET_CLAUSE(e)  ((DynCInf *) Y_CLAUSE(e))

#define BC_BEFORE_CALL(bc_pc)				\
do { /* prep cont: save BC pc, debug_call and set CP */	\
  WamWord w = (WamWord) (bc_pc) | debug_call;		\
  Bind_OV(&Y_BCPC_DBG(E), w);				\
  Dbg_Check_Trail(&Y_BCPC_DBG(E)); 			\
  CP = Adjust_CP(Prolog_Predicate(BC_EMULATE_CONT, 0));	\
}  while(0)

#define BC_AFTER_CALL(bc_pc)				\
do { /* in continuation: restore BC pc, debug_call */	\
  WamWord w = Y_BCPC_DBG(E);				\
  debug_call = w & 1;					\
  /*bc_pc = (BCWord *) (w - debug_call);*/		\
  bc_pc = (BCWord *) (w & ~(WamWord) 1);		\
} while(0)


#define BC_FRAME_BEGIN					\
  Y_CLAUSE(E)   = (WamWord) clause;			\
  Y_SAVE_CPE(E) = (WamWord) CP; /* NB: CP == CPE(E) */	\
  CPE(E)        = (WamCont) BC_Frame_Sentinel

#define BC_FRAME_END					\
  CPE(E) = (WamCont) Y_SAVE_CPE(E)

#define Is_BC_Frame(e) (CPE(e) == (WamCont) BC_Frame_Sentinel)

static void
BC_Frame_Sentinel(void)
{
  printf("!!! ERROR: in BC_Frame_Sentinel !!!\n"); exit(1);
}

#if 0
void
Dbg_Check_Trail(WamWord *adr_save)
{
  WamWord *base_tr = TRB(B);
  /*WamWord *base_tr = Trail_Stack;*/
  WamWord *tr = TR;
  WamWord *adr;
  WamWord word;
  int c = 0;
  int nb;
  
  while (tr > base_tr)
    {
      word = *--tr;
      adr = (WamWord *) (Trail_Value_Of(word));

      switch (Trail_Tag_Of(word))
        {
        case TUV:
          break;

        case TOV:
	  word = *--tr;
	  if (adr == adr_save)
	    c++;
	  break;

	case TMV:
	  nb = (int) *--tr;
	  tr -= nb;
	  break;

	default:                /* TFC */
	  --tr;
	  nb = (int) (*--tr);
	  tr -= nb;
	}
    }

  if (c > 1)
    printf("------------ SAVED %d times\n", c);

}
#else
#define Dbg_Check_Trail(a)
#endif




/*-------------------------------------------------------------------------*
 * PL_BC_CALL_TERMINAL_PRED_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Call_Terminal_Pred_3(WamWord pred_word, WamWord call_info_word,
			   WamWord first_call_word)
{
  int func, arity;
  WamWord *arg_adr;
  PredInf *pred;
  int i;

  arg_adr = Pl_Rd_Callable_Check(pred_word, &func, &arity);

  debug_call = (call_info_word & (1 << TAG_SIZE_LOW)) != 0;

  if (pl_debug_call_code && debug_call &&
      (first_call_word & (1 << TAG_SIZE_LOW)))
    {
      A(0) = pred_word;
      A(1) = call_info_word;
      return pl_debug_call_code;
    }

  pred = Pl_Lookup_Pred(func, arity);
  if (pred == NULL)
    {				/* case: fail/0 from '$call_from_debugger' */
      if (func != atom_fail || arity != 0)
	{
	  Pl_Call_Info_Bip_Name_1(call_info_word);
	  Pl_Unknown_Pred_Error(func, arity);
	}
      return ALTB(B);		/* i.e. fail */
    }

  for (i = 0; i < arity; i++)
    A(i) = *arg_adr++;

  if (pred->prop & MASK_PRED_NATIVE_CODE)	/* native code */
    return (WamCont) (pred->codep);

  return Pl_BC_Emulate_Pred(pred->dyn);
}




/*-------------------------------------------------------------------------*
 * PL_BC_EMULATE_PRED                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Emulate_Pred(DynPInf *dyn)
{
  DynCInf *clause;
  WamCont codep;
  int func, arity;

  while (dyn)
    {
      func = dyn->func;
      arity = dyn->arity;
      A(arity) = Pl_Get_Current_Choice();	/* init cut register */
      A(arity + 1) = debug_call;

      clause = Pl_Scan_Dynamic_Pred(func, arity, dyn, A(0),
				    (ScanFct) BC_Emulate_Pred_Alt,
				    DYN_ALT_FCT_FOR_JUMP, arity + 2, &A(0));
      if (clause == NULL)
	break;			/* fail */

      codep = BC_Emulate_Clause(clause);
      if (codep)
	return codep;

      dyn = glob_dyn;
    }
  /* fail */
  return ALTB(B);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_PRED_ALT                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong /* a WamCont in fact but respect type ScanFct see dynam_supp.h */
BC_Emulate_Pred_Alt(DynCInf *clause, WamWord *w, Bool is_last)
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
  while (--arity >= 0);		/* >= 0 to also restore cut register */

  debug_call = (Bool) *w;

  codep = BC_Emulate_Clause(clause);
  return (PlLong) ((codep) ? codep : Pl_BC_Emulate_Pred(glob_dyn));
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

  /* Issue #12: infinite loop when debugger is active on Call ports for
   * dynamic/multifile compiled code.
   * Each clause of such a predicate gives rise to both an assert
   * (suitable for interpretation) and a native code which is invoked by BC
   * with an EXECUTE_NATIVE instruction. See in .wam call_c to
   * Pl_Emit_BC_Execute_Wrapper and Pl_BC_Emit_Inst_Execute_Native.
   *
   * To fix the issue: either do not use BC if debug is active
   * (use interpreted code - see below) or do not call the debugger inside
   * EXECUTE_NATIVE (and CALL_NATIVE for consistency ?)
   */
#if 1
  /* emulated code (see above for test !debug_call) */
  if (bc && (!debug_call || pl_debug_call_code == NULL)) 
    return BC_Emulate_Byte_Code(bc, clause);
#else
  if (bc && !debug_call)
    return BC_Emulate_Byte_Code(bc, clause);
#endif

  /* interpreted code */
  Pl_Copy_Clause_To_Heap(clause, &head_word, &body_word);

  arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  for (i = 0; i < arity; i++)	/* head unification */
    if (!Pl_Unify(A(i), *arg_adr++))
      return ALTB(B);		/* fail */

  A(2) = A(arity);		/* set first else overwritte if arity <= 1 */
  A(0) = body_word;
  A(1) = Tag_INT(Call_Info(func, arity, debug_call));

  return (CodePtr) Prolog_Predicate(CALL_INTERNAL_WITH_CUT, 3);
}




/*-------------------------------------------------------------------------*
 * BC_EMULATE_BYTE_CODE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
BC_Emulate_Byte_Code(BCWord *bc_start, DynCInf *clause)
{
  BCWord *bc_sp = bc_start;
  BCWord w;
  unsigned code_op;
  int x0, x, y;
  int w1;
  PlLong l;
  WamCont codep;
  int func, arity;
  int caller_func, caller_arity;
  PredInf *pred;
  C64To32 cv;

  /* This function is also called fron Pl_BC_Emulate_Cont_0 (cotinuation).
   * In this, clause = NULL, this is OK the allocate instruction has
   * already been encountered. 
   * However, if necessary, we could get the clause from Y_CLAUSE.
   */

bc_loop:
  w = *bc_sp++;
  code_op = BC_Op(w);

  switch (code_op)
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
      w1 = bc_sp->word;
      bc_sp++;
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
      l = bc_sp->word;
      bc_sp++;
#else
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
      l = cv.l;
#endif
      if (!Pl_Get_Integer(l, X(x0)))
	goto fail;
      goto bc_loop;

    case GET_FLOAT:
      x0 = BC1_X0(w);
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
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
      func = bc_sp->word;
      bc_sp++;
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
      w1 = bc_sp->word;
      bc_sp++;
      X(x0) = Pl_Put_Atom(w1);
      goto bc_loop;

    case PUT_INTEGER:
      x0 = BC1_X0(w);
      X(x0) = Pl_Put_Integer(BC1_Int(w));
      goto bc_loop;

    case PUT_INTEGER_BIG:
      x0 = BC1_X0(w);
#if WORD_SIZE == 32
      l = bc_sp->word;
      bc_sp++;
#else
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
      l = cv.l;
#endif
      X(x0) = Pl_Put_Integer(l);
      goto bc_loop;

    case PUT_FLOAT:
      x0 = BC1_X0(w);
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
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
      func = bc_sp->word;
      bc_sp++;
      X(x0) = Pl_Put_Structure(func, arity);
      goto bc_loop;
/*
    case PUT_META_TERM:
      x0 = BC1_X0(w);
      x = BC1_XY(w);
      module = bc_sp->word;
      bc_sp++;
      X(x) = Pl_Put_Meta_Term(module, X(x0));
      goto bc_loop;
*/
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
      w1 = bc_sp->word;
      bc_sp++;
      if (!Pl_Unify_Atom(w1))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER:
      if (!Pl_Unify_Integer(BC2_Int(w)))
	goto fail;
      goto bc_loop;

    case UNIFY_INTEGER_BIG:
#if WORD_SIZE == 32
      l = bc_sp->word;
      bc_sp++;
#else
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
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
      func = bc_sp->word;
      bc_sp++;
      if (!Pl_Unify_Structure(func, arity))
	goto fail;
      goto bc_loop;

    case ALLOCATE:
      Pl_Allocate(BC2_Int(w));
      BC_FRAME_BEGIN;
      goto bc_loop;

    case DEALLOCATE:
      BC_FRAME_END;
      Pl_Deallocate();
      goto bc_loop;

    case CALL:
      BC_BEFORE_CALL(bc_sp + 2);
      // fall through
    case EXECUTE:
      arity = BC2_Arity(w);
      func = bc_sp->word;
      bc_sp++;
      if (pl_debug_call_code && debug_call && !Pl_Detect_If_Aux_Name(func))
	{
	  w1 = bc_sp->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Prep_Debug_Call(func, arity, caller_func, caller_arity);
	  return pl_debug_call_code;
	}

      if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
	{
	  w1 = bc_sp->word;
	  caller_func = Functor_Of(w1);
	  caller_arity = Arity_Of(w1);
	  Pl_Set_Bip_Name_2(Tag_ATM(caller_func), Tag_INT(caller_arity));
	  Pl_Unknown_Pred_Error(func, arity);
	  goto fail;
	}
#if 0
      bc_sp++;			/* useless since BC_BEFORE_CALL already done */
#endif
      glob_dyn = pred->dyn;
      return NULL;		/* to then call BC_Emulate_Pred */

    case CALL_NATIVE:
      BC_BEFORE_CALL(bc_sp + (1 + WORD_SIZE / 32));
      // fall through
    case EXECUTE_NATIVE:
      arity = BC2_Arity(w);
      func = bc_sp->word;
      bc_sp++;
#if WORD_SIZE == 32
      codep = (WamCont) (bc_sp->word);
      bc_sp++;
#else
      cv.u[0] = bc_sp->word;
      bc_sp++;
      cv.u[1] = bc_sp->word;
      bc_sp++;
      codep = (WamCont) (cv.p);
#endif
      /* Issue #12: alternative fix could be to never call the debugger here */
      if (pl_debug_call_code && debug_call)
	{
	  Prep_Debug_Call(func, arity, 0, 0);
	  return pl_debug_call_code;
	}
      return codep;

    case PROCEED:
      return UnAdjust_CP(CP);

    case FAIL:
      if (pl_debug_call_code && debug_call)
	{		   /* invoke the debugger which will then call fail/0 */
	  Prep_Debug_Call(atom_fail, 0, 0, 0);
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
 * Trampoline function (pl->bc) that receives a continuation of bytecode   *
 * (after a WAM call or call_native) and restarts the bytecode emulator.   *
 * So, we know E is the BC frame (bc_sp between allocate and deallocate).  *
 *-------------------------------------------------------------------------*/
WamCont
Pl_BC_Emulate_Cont_0(void)
{
  WamCont codep;
  BCWord *bc_sp;

  BC_AFTER_CALL(bc_sp);

  		/* see BC_Emulate_Byte_Code for the NULL */
  codep = BC_Emulate_Byte_Code(bc_sp, NULL); 
  return (codep) ? codep : Pl_BC_Emulate_Pred(glob_dyn);
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




/*-------------------------------------------------------------------------*
 * PL_BC_CLAUSE_ENVIRONMENT                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
DynCInf *
Pl_BC_Clause_Environment(WamWord *e)
{
  if (Is_BC_Frame(e))
    return BC_GET_CLAUSE(e);
  
  return NULL;
}
