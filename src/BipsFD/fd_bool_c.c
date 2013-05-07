/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_bool_c.c                                                     *
 * Descr.: boolean and Meta-constraint predicate management - C part       *
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


#define OBJ_INIT Fd_Bool_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define BOOL_STACK_SIZE            100000
#define VARS_STACK_SIZE            100000


#define NOT                        0

#define EQUIV                      1
#define NEQUIV                     2
#define IMPLY                      3
#define NIMPLY                     4
#define AND                        5
#define NAND                       6
#define OR                         7
#define NOR                        8

#define EQ                         9	/* warning EQ must have same */
#define NEQ                        10	/* parity than EQ_F and ZERO */
#define LT                         11
#define GTE                        12
#define GT                         13
#define LTE                        14

#define EQ_F                       15
#define NEQ_F                      16
#define LT_F                       17
#define GTE_F                      18
#define GT_F                       19
#define LTE_F                      20

#define ZERO                       21
#define ONE                        22	/* must be last */

#define IsVar(op)  ((op)>=ONE)

#define NB_OF_OP                   ZERO




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord bool_tbl[NB_OF_OP];
static WamWord bool_xor;

static WamWord stack[BOOL_STACK_SIZE];
static WamWord *sp;

static WamWord vars_tbl[VARS_STACK_SIZE];
static WamWord *vars_sp;

static Bool (*func_tbl[NB_OF_OP + 2]) (WamWord *exp, int result, WamWord *load_word);




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static WamWord *Simplify(int sign, WamWord e_word);

static void Add_Fd_Variables(WamWord e_word);

static Bool Load_Bool_Into_Word(WamWord *exp, int result,
				WamWord *load_word);



static Bool Set_Var(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Not(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Equiv(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Nequiv(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Imply(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Nimply(WamWord *exp, int result, WamWord *load_word);

static Bool Set_And(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Nand(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Or(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Nor(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Eq(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Neq(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Lt(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Lte(WamWord *exp, int result, WamWord *load_word);

static Bool Set_Zero(WamWord *exp, int result, WamWord *load_word);

static Bool Set_One(WamWord *exp, int result, WamWord *load_word);



#ifdef DEBUG

static void Display_Stack(WamWord *exp);

void Pl_Write_1(WamWord term_word);

#endif



	  /* defined in fd_math_c.c */

Bool Pl_Fd_Eq_2(WamWord le_word, WamWord re_word);

Bool Pl_Fd_Neq_2(WamWord le_word, WamWord re_word);

Bool Pl_Fd_Lt_2(WamWord le_word, WamWord re_word);

Bool Pl_Fd_Lte_2(WamWord le_word, WamWord re_word);



#define BOOL_CSTR_2(f, a1, a2)                    	\
  do							\
    {							\
      if (!Pl_Fd_Check_For_Bool_Var(a1))		\
	return FALSE;					\
      if (!Pl_Fd_Check_For_Bool_Var(a2))		\
	return FALSE;					\
      PRIM_CSTR_2(f, a1, a2);				\
    }							\
  while (0)


#define BOOL_CSTR_3(f, a1, a2, a3)                    	\
  do							\
    {							\
      if (!Pl_Fd_Check_For_Bool_Var(a1))		\
	return FALSE;					\
      if (!Pl_Fd_Check_For_Bool_Var(a2))		\
	return FALSE;					\
                                      /* a3 is OK */	\
      PRIM_CSTR_3(f, a1, a2, a3);			\
    }							\
  while (0)




/*-------------------------------------------------------------------------*
 * FD_BOOL_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Fd_Bool_Initializer(void)
{
  bool_tbl[NOT] = Functor_Arity(Pl_Create_Atom("#\\"), 1);

  bool_tbl[EQUIV] = Functor_Arity(Pl_Create_Atom("#<=>"), 2);
  bool_tbl[NEQUIV] = Functor_Arity(Pl_Create_Atom("#\\<=>"), 2);
  bool_tbl[IMPLY] = Functor_Arity(Pl_Create_Atom("#==>"), 2);
  bool_tbl[NIMPLY] = Functor_Arity(Pl_Create_Atom("#\\==>"), 2);
  bool_tbl[AND] = Functor_Arity(Pl_Create_Atom("#/\\"), 2);
  bool_tbl[NAND] = Functor_Arity(Pl_Create_Atom("#\\/\\"), 2);
  bool_tbl[OR] = Functor_Arity(Pl_Create_Atom("#\\/"), 2);
  bool_tbl[NOR] = Functor_Arity(Pl_Create_Atom("#\\\\/"), 2);

  bool_tbl[EQ] = Functor_Arity(Pl_Create_Atom("#="), 2);
  bool_tbl[NEQ] = Functor_Arity(Pl_Create_Atom("#\\="), 2);
  bool_tbl[LT] = Functor_Arity(Pl_Create_Atom("#<"), 2);
  bool_tbl[GTE] = Functor_Arity(Pl_Create_Atom("#>="), 2);
  bool_tbl[GT] = Functor_Arity(Pl_Create_Atom("#>"), 2);
  bool_tbl[LTE] = Functor_Arity(Pl_Create_Atom("#=<"), 2);

  bool_tbl[EQ_F] = Functor_Arity(Pl_Create_Atom("#=#"), 2);
  bool_tbl[NEQ_F] = Functor_Arity(Pl_Create_Atom("#\\=#"), 2);
  bool_tbl[LT_F] = Functor_Arity(Pl_Create_Atom("#<#"), 2);
  bool_tbl[GTE_F] = Functor_Arity(Pl_Create_Atom("#>=#"), 2);
  bool_tbl[GT_F] = Functor_Arity(Pl_Create_Atom("#>#"), 2);
  bool_tbl[LTE_F] = Functor_Arity(Pl_Create_Atom("#=<#"), 2);

  bool_xor = Functor_Arity(Pl_Create_Atom("##"), 2);


  func_tbl[NOT] = Set_Not;

  func_tbl[EQUIV] = Set_Equiv;
  func_tbl[NEQUIV] = Set_Nequiv;
  func_tbl[IMPLY] = Set_Imply;
  func_tbl[NIMPLY] = Set_Nimply;
  func_tbl[AND] = Set_And;
  func_tbl[NAND] = Set_Nand;
  func_tbl[OR] = Set_Or;
  func_tbl[NOR] = Set_Nor;

  func_tbl[EQ] = Set_Eq;
  func_tbl[NEQ] = Set_Neq;
  func_tbl[LT] = Set_Lt;
  func_tbl[GTE] = NULL;
  func_tbl[GT] = NULL;
  func_tbl[LTE] = Set_Lte;

  func_tbl[EQ_F] = NULL;
  func_tbl[NEQ_F] = NULL;
  func_tbl[LT_F] = NULL;
  func_tbl[GTE_F] = NULL;
  func_tbl[GT_F] = NULL;
  func_tbl[LTE_F] = NULL;

  func_tbl[ZERO] = Set_Zero;
  func_tbl[ONE] = Set_One;
}




/*-------------------------------------------------------------------------*
 * PL_FD_BOOL_META_3                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Bool_Meta_3(WamWord le_word, WamWord re_word, WamWord op_word)
{
  WamWord word, tag_mask;
  WamWord *adr, *fdv_adr;
  WamWord *exp;
  int op;
  static WamWord h[3];		/* static to avoid high address */


  DEREF(op_word, word, tag_mask);
  op = UnTag_INT(op_word);

  h[0] = bool_tbl[op];		/* also works for NOT/1 */
  h[1] = le_word;
  h[2] = re_word;

  sp = stack;
  vars_sp = vars_tbl;

  exp = Simplify(1, Tag_STC(h));

#ifdef DEBUG
  Display_Stack(exp);
  DBGPRINTF("\n");
#endif

  if (!Load_Bool_Into_Word(exp, 1, NULL))
    return FALSE;

  while (--vars_sp >= vars_tbl)
    if (*vars_sp-- == 0)	/* bool var */
      {
	if (!Pl_Fd_Check_For_Bool_Var(*vars_sp))
	  return FALSE;
      }
    else			/* FD var */
      {
	DEREF(*vars_sp, word, tag_mask);
	if (tag_mask == TAG_REF_MASK)
	  {
	    adr = UnTag_REF(word);
	    fdv_adr = Pl_Fd_New_Variable();
	    Bind_UV(adr, Tag_REF(fdv_adr));
	  }
      }


  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SIMPLIFY                                                                *
 *                                                                         *
 * This function returns the result of the simplified boolean expression   *
 * given in e_word. NOT operators are only applied to variables.           *
 *                                                                         *
 * Input:                                                                  *
 *    sign  : current sign of the boolean term (-1 (inside a ~) or +1)     *
 *    e_word: boolean term to simplify                                     *
 *                                                                         *
 * Output:                                                                 *
 *    The returned result is a pointer to a node of the following form:    *
 *                                                                         *
 *    for binary boolean not operator (~):                                 *
 *        [1]: variable involved (tagged word)                             *
 *        [0]: operator NOT                                                *
 *                                                                         *
 *    for unary boolean operators (<=> ~<=> ==> ~==> /\ ~/\ \/ ~\/):       *
 *        [2]: right boolean exp (pointer to node)                         *
 *        [1]: left  boolean exp (pointer to node)                         *
 *        [0]: operator (EQUIV, NEQUIV, IMPLY, NIMPLY, AND, NAND, OR, NOR) *
 *                                                                         *
 *    for boolean false value (0):                                         *
 *        [0]: ZERO                                                        *
 *                                                                         *
 *    for boolean true value (1):                                          *
 *        [0]: ONE                                                         *
 *                                                                         *
 *    for boolean variable:                                                *
 *        [0]: tagged word                                                 *
 *                                                                         *
 *    for binary math operators (= \= < >= > <=) (partial / full AC):      *
 *        [2]: right math exp (tagged word)                                *
 *        [1]: left  math exp (tagged word)                                *
 *        [0]: operator (EQ, NEQ, LT, LTE, EQ_F, NEQ_F, LT_F, LTE_F)       *
 *             (GT, GTE, GT_F, and GTE_F becomes LT, LTE, LT_F and LTE_F)  *
 *                                                                         *
 * These nodes are stored in a hybrid stack. NB: XOR same as NEQUIV        *
 *-------------------------------------------------------------------------*/
static WamWord *
Simplify(int sign, WamWord e_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord f_n, le_word, re_word;
  int op, n;
  WamWord *exp, *sp1;
  WamWord l, r;

#ifdef DEBUG
  printf("ENTERING %5ld: %2d: ", sp - stack, sign);
  Pl_Write(e_word);
  printf("\n");
#endif

  exp = sp;

  if (sp - stack > BOOL_STACK_SIZE - 5)
    Pl_Err_Resource(pl_resource_too_big_fd_constraint);

  DEREF(e_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_FDV_MASK)
    {
      adr = UnTag_Address(word);
      if (vars_sp - vars_tbl == VARS_STACK_SIZE)
	Pl_Err_Resource(pl_resource_too_big_fd_constraint);

      *vars_sp++ = word;
      *vars_sp++ = 0;		/* bool var */

      if (sign != 1)
	*sp++ = NOT;

      *sp++ = Tag_REF(adr);
      return exp;
    }

  if (tag_mask == TAG_INT_MASK)
    {
      n = UnTag_INT(word);
      if ((unsigned) n > 1)
	goto type_error;

      *sp++ = ZERO + ((sign == 1) ? n : 1 - n);
      return exp;
    }

  if (tag_mask == TAG_ATM_MASK)
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Value(e_word);
      Pl_Unify_Integer(0);
    type_error:
      Pl_Err_Type(pl_type_fd_bool_evaluable, word);
    }


  if (tag_mask != TAG_STC_MASK)
    goto type_error;

  adr = UnTag_STC(word);

  f_n = Functor_And_Arity(adr);
  if (bool_xor == f_n)
    op = NEQUIV;
  else
    {
      for (op = 0; op < NB_OF_OP; op++)
	if (bool_tbl[op] == f_n)
	  break;

      if (op == NB_OF_OP)
	{
	  word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
	  Pl_Unify_Atom(Functor(adr));
	  Pl_Unify_Integer(Arity(adr));
	  goto type_error;
	}
    }

  le_word = Arg(adr, 0);
  re_word = Arg(adr, 1);

  if (op == NOT)
    return Simplify(-sign, le_word);

  if (sign != 1)
    op = (op % 2 == EQ % 2) ? op + 1 : op - 1;

  if (op >= EQ && op <= LTE_F)
    {
      Add_Fd_Variables(le_word);
      Add_Fd_Variables(re_word);

      n = (op == GT || op == GT_F) ? op - 2 :
	(op == GTE || op == GTE_F) ? op + 2 : op;

      *sp++ = n;
      *sp++ = (n == op) ? le_word : re_word;
      *sp++ = (n == op) ? re_word : le_word;
      return exp;
    }

  sp += 3;
  exp[0] = op;
  exp[1] = (WamWord) Simplify(1, le_word);
  sp1 = sp;
  exp[2] = (WamWord) Simplify(1, re_word);

  l = *(WamWord *) (exp[1]);
  r = *(WamWord *) (exp[2]);

  /* NB: beware when calling below Simplify() (while has been just called above)
   * this can ran into stack overflow (N^2 space complexity). 
   * Try to recover the stack before calling Simplify().
   * Other stack recovery are less important (e.g. when only using exp[1]).
   *
   * In the following exp[] += sizeof(WamWord) is used to "skip" the NOT
   * in a simplification (points to the next cell).
   */

  switch (op)
    {
    case EQUIV:
      if (l == ZERO)		/* 0 <=> R is ~R */
	{
	  sp = exp;
	  return Simplify(-1, re_word);
	}

      if (l == ONE)		/* 1 <=> R is R */
	{
	  return (WamWord *) exp[2];
	}

      if (r == ZERO)		/* L <=> 0 is ~L */
	{
	  sp = exp;
	  return Simplify(-1, le_word);
	}

      if (r == ONE)		/* L <=> 1 is L */
	{
	  sp = sp1;
	  return (WamWord *) exp[1];
	}

      if (l == NOT)		/* ~X <=> R is X <=> ~R */
	{
	  exp[1] += sizeof(WamWord); 
	  sp = sp1;
	  exp[2] = (WamWord) Simplify(-1, re_word);
	  break;
	}

      if (r == NOT)		/* L <=> ~X is ~L <=> X */
	{			/* NB: cannot recover the stack */	  
	  exp[1] = (WamWord) Simplify(-1, le_word);
	  exp[2] += sizeof(WamWord);
	  break;
	}
      break;

    case NEQUIV:
      if (l == ZERO)		/* 0 ~<=> R is R */
	{
	  return (WamWord *) exp[2];
	}

      if (l == ONE)		/* 1 ~<=> R is ~R */
	{
	  sp = exp;
	  return Simplify(-1, re_word);
	}

      if (r == ZERO)		/* L ~<=> 0 is L */
	{
	  sp = sp1;
	  return (WamWord *) exp[1];
	}

      if (r == ONE)		/* L ~<=> 1 is ~L */
	{
	  sp = exp;
	  return Simplify(-1, le_word);
	}

      if (l == NOT)		/* ~X ~<=> R is X <=> R */
	{
	  exp[0] = EQUIV;
	  exp[1] += sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L ~<=> ~X is L <=> X */
	{
	  exp[0] = EQUIV;
	  exp[2] += sizeof(WamWord);
	  break;
	}

      if (IsVar(l) && !IsVar(r)) /* X ~<=> R is X <=> ~R */
	{
	  exp[0] = EQUIV;
	  sp = sp1;
	  exp[2] = (WamWord) Simplify(-1, re_word);
	  break;
	}

      if (IsVar(r) && !IsVar(l)) /* L ~<=> X is L <=> ~X */
	{
	  exp[0] = EQUIV;	/* NB: cannot recover the stack */
	  exp[1] = (WamWord) Simplify(-1, le_word);
	  break;
	}
      break;

    case IMPLY:
      if (l == ZERO || r == ONE) /* 0 ==> R is 1 , L ==> 1 is 1 */
	{
	  sp = exp;
	  *sp++ = ONE;
	  break;
	}

      if (l == ONE)		/* 1 ==> R is R */
	{
	  return (WamWord *) exp[2];
	}

      if (r == ZERO)		/* L ==> 0 is ~L */
	return sp = exp, Simplify(-1, le_word);

      if (l == NOT)		/* ~X ==> R is X \/ R */
	{
	  exp[0] = OR;
	  exp[1] += sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L ==> ~X is X ==> ~L */
	{
	  exp[1] = exp[2] + sizeof(WamWord);
	  exp[2] = (WamWord) Simplify(-1, le_word);
	  break;
	}
      break;

    case NIMPLY:
      if (l == ZERO || r == ONE) /* 0 ~==> R is 0 , L ~==> 1 is 0 */
	{
	  sp = exp;
	  *sp++ = ZERO;
	  break;
	}

      if (l == ONE)		/* 1 ~==> R is ~R */
	{
	  sp = exp;
	  return Simplify(-1, re_word);
	}

      if (r == ZERO)		/* L ~==> 0 is L */
	{
	  sp = sp1;
	  return (WamWord *) exp[1];
	}

      if (l == NOT)		/* ~X ~==> R is X ~\/ R */
	{
	  exp[0] = NOR;
	  exp[1] += sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L ~==> ~X is L /\ X */
	{
	  exp[0] = AND;
	  exp[2] += sizeof(WamWord);
	  break;
	}
      break;

    case AND:
      if (l == ZERO || r == ZERO) /* 0 /\ R is 0 , L /\ 0 is 0 */
	{
	  sp = exp;
	  *sp++ = ZERO;
	  break;
	}

      if (l == ONE)		/* 1 /\ R is R */
	{
	  return (WamWord *) exp[2];
	}

      if (r == ONE)		/* L /\ 1 is L */
	{
	  sp = sp1;
	  return (WamWord *) exp[1];
	}

      if (l == NOT)		/* ~X /\ R is R ~==> X */
	{
	  exp[0] = NIMPLY;
	  word = exp[1];
	  exp[1] = exp[2];
	  exp[2] = word + sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L /\ ~X is L ~==> X */
	{
	  exp[0] = NIMPLY;
	  exp[2] += sizeof(WamWord);
	  break;
	}
      break;

    case NAND:
      if (l == ZERO || r == ZERO) /* 0 ~/\ R is 1 , L ~/\ 0 is 1 */
	{
	  sp = exp;
	  *sp++ = ONE;
	  break;
	}

      if (l == ONE)		/* 1 ~/\ R is ~R */
	{
	  sp = exp;
	  return Simplify(-1, re_word);
	}

      if (r == ONE)		/* L ~/\ 1 is ~L */
	{
	  sp = exp;
	  return Simplify(-1, le_word);
	}

      if (l == NOT)		/* ~X ~/\ R is R ==> X */
	{
	  exp[0] = IMPLY;
	  word = exp[1];
	  exp[1] = exp[2];
	  exp[2] = word + sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L ~/\ ~X is L ==> X */
	{
	  exp[0] = IMPLY;
	  exp[2] += sizeof(WamWord);
	  break;
	}
      break;

    case OR:
      if (l == ONE || r == ONE)	/* 1 \/ R is 1 , L \/ 1 is 1 */
	{
	  sp = exp;
	  *sp++ = ONE;
	  break;
	}

      if (l == ZERO)		/* 0 \/ R is R */
	{
	  return (WamWord *) exp[2];
	}

      if (r == ZERO)		/* L \/ 0 is L */
	{
	  sp = sp1;
	  return (WamWord *) exp[1];
	}

      if (l == NOT)		/* ~X \/ R is X ==> R */
	{
	  exp[0] = IMPLY;
	  exp[1] += sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L \/ ~X is X ==> L */
	{
	  exp[0] = IMPLY;
	  word = exp[1];
	  exp[1] = exp[2] + sizeof(WamWord);
	  exp[2] = word;
	  break;
	}
      break;

    case NOR:
      if (l == ONE || r == ONE)	/* 1 ~\/ R is 0 , L ~\/ 1 is 0 */
	{
	  sp = exp;
	  *sp++ = ZERO;
	  break;
	}

      if (l == ZERO)		/* 0 ~\/ R is ~R */
	{
	  sp = exp;
	  return Simplify(-1, re_word);
	}

      if (r == ZERO)		/* L ~\/ 0 is ~L */
	{
	  sp = exp;
	  return Simplify(-1, le_word);
	}

      if (l == NOT)		/* ~X ~\/ R is X ~==> R */
	{
	  exp[0] = NIMPLY;
	  exp[1] += sizeof(WamWord);
	  break;
	}

      if (r == NOT)		/* L ~\/ ~X is X ~==> L */
	{
	  exp[0] = NIMPLY;
	  word = exp[1];
	  exp[1] = exp[2] + sizeof(WamWord);
	  exp[2] = word;
	  break;
	}
      break;
    }

  return exp;
}




#ifdef DEBUG
/*-------------------------------------------------------------------------*
 * DISPLAY_STACK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_Stack(WamWord *exp)
{
  int op = exp[0];
  WamWord *le = (WamWord *) (exp[1]);
  WamWord *re = (WamWord *) (exp[2]);

  switch (op)
    {
    case NOT:
      DBGPRINTF("%s", pl_atom_tbl[Functor_Of(bool_tbl[op])].name);
      DBGPRINTF(" ");
      Pl_Write_1(exp[1]);
      break;

    case EQUIV:
    case NEQUIV:
    case IMPLY:
    case NIMPLY:
    case AND:
    case NAND:
    case OR:
    case NOR:
      DBGPRINTF("(");
      Display_Stack(le);
      DBGPRINTF(" ");
      DBGPRINTF("%s", pl_atom_tbl[Functor_Of(bool_tbl[op])].name);
      DBGPRINTF(" ");
      Display_Stack(re);
      DBGPRINTF(")");
      break;

    case EQ:
    case NEQ:
    case LT:
    case LTE:
    case GT:
    case GTE:

    case EQ_F:
    case NEQ_F:
    case LT_F:
    case LTE_F:
    case GT_F:
    case GTE_F:
      Pl_Write_1(exp[1]);
      DBGPRINTF(" ");
      DBGPRINTF("%s", pl_atom_tbl[Functor_Of(bool_tbl[op])].name);
      DBGPRINTF(" ");
      Pl_Write_1(exp[2]);
      break;

    case ZERO:
      DBGPRINTF("0");
      break;

    case ONE:
      DBGPRINTF("1");
      break;

    default:
      Pl_Write_1(*exp);
    }
}
#endif




/*-------------------------------------------------------------------------*
 * ADD_FD_VARIABLES                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Add_Fd_Variables(WamWord e_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int i;

  DEREF(e_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    {
      if (vars_sp - vars_tbl == VARS_STACK_SIZE)
	Pl_Err_Resource(pl_resource_too_big_fd_constraint);

      *vars_sp++ = word;
      *vars_sp++ = 1;		/* FD var */
      return;
    }


  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);

      Add_Fd_Variables(Car(adr));
      Add_Fd_Variables(Cdr(adr));
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);

      i = Arity(adr);
      do
	Add_Fd_Variables(Arg(adr, --i));
      while (i);
    }
}




/*-------------------------------------------------------------------------*
 * LOAD_BOOL_INTO_WORD                                                     *
 *                                                                         *
 * This loads a boolean term into a tagged word.                           *
 * Input:                                                                  *
 *    exp      : boolean term to load                                      *
 *    result   : which result is expected:                                 *
 *               0=false, 1=true,                                          *
 *               2=result into the word pointed by load_word.              *
 *                                                                         *
 * Output:                                                                 *
 *    load_word: if result=2 it will contain the tagged word of the term:  *
 *               a <INT,0/1> or a <REF,adr>                                *
 *-------------------------------------------------------------------------*/
static Bool
Load_Bool_Into_Word(WamWord *exp, int result, WamWord *load_word)
{
  PlULong op = *exp;

  if (op >= EQ_F && op <= LTE_F)
    {
      pl_full_ac = 1;
      op = op - EQ_F + EQ;
    }
  else
    pl_full_ac = 0;

  return (*((op <= ONE) ? func_tbl[op] : Set_Var)) (exp, result, load_word);
}




/*-------------------------------------------------------------------------*
 * SET_ZERO                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Zero(WamWord *exp, int result, WamWord *load_word)
{
  if (result == 0)		/* 0 is false */
    return TRUE;

  if (result == 1)		/* 0 is true */
    return FALSE;

				/* 0 = B */
  return Pl_Get_Integer(0, *load_word);
}




/*-------------------------------------------------------------------------*
 * SET_ONE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_One(WamWord *exp, int result, WamWord *load_word)
{
  if (result == 0)		/* 1 is false */
    return FALSE;

  if (result == 1)		/* 1 is true */
    return TRUE;

				/* 1 = B */
  return Pl_Get_Integer(1, *load_word);
}




/*-------------------------------------------------------------------------*
 * SET_VAR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Var(WamWord *exp, int result, WamWord *load_word)
{
  if (result == 0)		/* X is false */
    return Pl_Get_Integer(0, *exp);

  if (result == 1)		/* X is true */
    return Pl_Get_Integer(1, *exp);

  *load_word = *exp;		/* X = B */
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NOT                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Not(WamWord *exp, int result, WamWord *load_word)
{
  if (result == 0)		/* ~X is false */
    return Pl_Get_Integer(1, exp[1]);

  if (result == 1)		/* ~X is true */
    return Pl_Get_Integer(0, exp[1]);

				/* ~X=B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_2(pl_not_x_eq_b, exp[1], *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_EQUIV                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Equiv(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

  if (result == 0)		/* L <=> R is false */
    {
      BOOL_CSTR_2(pl_not_x_eq_b, load_l, load_r);
      return TRUE;
    }

  if (result == 1)		/* L <=> R is true */
    return Pl_Fd_Math_Unify_X_Y(load_l, load_r);

				/* L <=> R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_equiv_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NEQUIV                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Nequiv(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result <= 1)		/* L ~<=> R is true or false */
    return Set_Equiv(exp, 1 - result, load_word);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

				/* L ~<=> R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_nequiv_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_IMPLY                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Imply(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result == 0)		/* L ==> R is false */
    return Load_Bool_Into_Word((WamWord *) (exp[1]), 1, &load_l) &&
      Load_Bool_Into_Word((WamWord *) (exp[2]), 0, &load_r);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

  if (result == 1)		/* L ==> R is true */
    {
      BOOL_CSTR_2(pl_x_imply_y_eq_1, load_l, load_r);
      return TRUE;
    }

				/* L ==> R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_imply_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NIMPLY                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Nimply(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result <= 1)		/* L ~==> R is true or false */
    return Set_Imply(exp, 1 - result, load_word);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

				/* L ~==> R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_nimply_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_AND                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_And(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result == 1)		/* L /\ R is true */
    return Load_Bool_Into_Word((WamWord *) (exp[1]), 1, NULL) &&
      Load_Bool_Into_Word((WamWord *) (exp[2]), 1, NULL);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

  if (result == 0)		/* L /\ R is false */
    {
      BOOL_CSTR_2(pl_x_and_y_eq_0, load_l, load_r);
      return TRUE;
    }

				/* L /\ R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_and_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NAND                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Nand(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result <= 1)		/* L ~/\ R is true or false */
    return Set_And(exp, 1 - result, load_word);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

				/* L ~/\ R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_nand_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_OR                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Or(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result == 0)		/* L \/ R is false */
    return Load_Bool_Into_Word((WamWord *) (exp[1]), 0, NULL) &&
      Load_Bool_Into_Word((WamWord *) (exp[2]), 0, NULL);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

  if (result == 1)		/* L \/ R is true */
    {
      BOOL_CSTR_2(pl_x_or_y_eq_1, load_l, load_r);
      return TRUE;
    }

				/* L \/ R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_or_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NOR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Nor(WamWord *exp, int result, WamWord *load_word)
{
  WamWord load_l, load_r;

  if (result <= 1)		/* L ~\/ R is true or false */
    return Set_Or(exp, 1 - result, load_word);

  if (!Load_Bool_Into_Word((WamWord *) (exp[1]), 2, &load_l) ||
      !Load_Bool_Into_Word((WamWord *) (exp[2]), 2, &load_r))
    return FALSE;

				/* L ~\/ R = B */
  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());
  BOOL_CSTR_3(pl_x_nor_y_eq_b, load_l, load_r, *load_word);
  return TRUE;
}








/*-------------------------------------------------------------------------*
 * SET_EQ                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Eq(WamWord *exp, int result, WamWord *load_word)
{
  WamWord le_word, re_word;
  int mask;
  WamWord l_word, r_word;
  PlLong c;

  le_word = exp[1];
  re_word = exp[2];

  if (result == 0)		/* L = R is false */
    return Pl_Fd_Neq_2(le_word, re_word);

  if (result == 1)		/* L = R is true */
    return Pl_Fd_Eq_2(le_word, re_word);

  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());

#ifdef DEBUG
  cur_op = (pl_full_ac) ? "truth#=#" : "truth#=";
#endif

  if (!Pl_Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word, &r_word)
      || !Pl_Term_Math_Loading(l_word, r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      return Pl_Get_Integer(c == 0, *load_word);

    case MASK_LEFT:
      if (c > 0)
	return Pl_Get_Integer(0, *load_word);

      MATH_CSTR_3(pl_truth_x_eq_c, l_word, Tag_INT(-c), *load_word);
      return TRUE;

    case MASK_RIGHT:
      if (c < 0)
	return Pl_Get_Integer(0, *load_word);

      MATH_CSTR_3(pl_truth_x_eq_c, r_word, Tag_INT(c), *load_word);
      return TRUE;
    }

  if (c > 0)
    {
      MATH_CSTR_4(pl_truth_x_plus_c_eq_y, l_word, Tag_INT(c), r_word,
		  *load_word);
      return TRUE;
    }

  if (c < 0)
    {
      MATH_CSTR_4(pl_truth_x_plus_c_eq_y, r_word, Tag_INT(-c), l_word,
		  *load_word);
      return TRUE;
    }

  MATH_CSTR_3(pl_truth_x_eq_y, l_word, r_word, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_NEQ                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Neq(WamWord *exp, int result, WamWord *load_word)
{
  WamWord le_word, re_word;
  int mask;
  WamWord l_word, r_word;
  PlLong c;

  le_word = exp[1];
  re_word = exp[2];

  if (result == 0)		/* L \= R is false */
    return Pl_Fd_Eq_2(le_word, re_word);

  if (result == 1)		/* L \= R is true */
    return Pl_Fd_Neq_2(le_word, re_word);

  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());

#ifdef DEBUG
  cur_op = (pl_full_ac) ? "truth#\\=#" : "truth#\\=";
#endif

  if (!Pl_Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word, &r_word)
      || !Pl_Term_Math_Loading(l_word, r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      return Pl_Get_Integer(c != 0, *load_word);

    case MASK_LEFT:
      if (c > 0)
	return Pl_Get_Integer(1, *load_word);

      MATH_CSTR_3(pl_truth_x_neq_c, l_word, Tag_INT(-c), *load_word);
      return TRUE;

    case MASK_RIGHT:
      if (c < 0)
	return Pl_Get_Integer(1, *load_word);

      MATH_CSTR_3(pl_truth_x_neq_c, r_word, Tag_INT(c), *load_word);
      return TRUE;
    }

  if (c > 0)
    {
      MATH_CSTR_4(pl_truth_x_plus_c_neq_y, l_word, Tag_INT(c), r_word,
		  *load_word);
      return TRUE;
    }

  if (c < 0)
    {
      MATH_CSTR_4(pl_truth_x_plus_c_neq_y, r_word, Tag_INT(-c), l_word,
		  *load_word);
      return TRUE;
    }


  MATH_CSTR_3(pl_truth_x_neq_y, l_word, r_word, *load_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SET_LT                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Lt(WamWord *exp, int result, WamWord *load_word)
{
  WamWord le_word, re_word;
  int mask;
  WamWord l_word, r_word;
  PlLong c;

  le_word = exp[1];
  re_word = exp[2];

  if (result == 0)		/* L < R is false */
    return Pl_Fd_Lte_2(re_word, le_word);

  if (result == 1)		/* L < R is true */
    return Pl_Fd_Lt_2(le_word, re_word);

  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());

#ifdef DEBUG
  cur_op = (pl_full_ac) ? "truth#<#" : "truth#<";
#endif

  if (!Pl_Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word, &r_word)
      || !Pl_Term_Math_Loading(l_word, r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      return Pl_Get_Integer(c < 0, *load_word);

    case MASK_LEFT:
      if (c >= 0)
	return Pl_Get_Integer(0, *load_word);

      PRIM_CSTR_3(pl_truth_x_lte_c, l_word, Tag_INT(-c - 1), *load_word);
      return TRUE;

    case MASK_RIGHT:
      if (c < 0)
	return Pl_Get_Integer(1, *load_word);

      PRIM_CSTR_3(pl_truth_x_gte_c, r_word, Tag_INT(c + 1), *load_word);
      return TRUE;
    }

  if (c > 0)
    {
      PRIM_CSTR_4(pl_truth_x_plus_c_lte_y, l_word, Tag_INT(c + 1), r_word,
		  *load_word);
      return TRUE;
    }

  if (c < 0)
    {
      PRIM_CSTR_4(pl_truth_x_plus_c_gte_y, r_word, Tag_INT(-c - 1), l_word,
		  *load_word);
      return TRUE;
    }


  PRIM_CSTR_3(pl_truth_x_lt_y, l_word, r_word, *load_word);
  return TRUE;
}



/*-------------------------------------------------------------------------*
 * SET_LTE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Set_Lte(WamWord *exp, int result, WamWord *load_word)
{
  WamWord le_word, re_word;
  int mask;
  WamWord l_word, r_word;
  PlLong c;

  le_word = exp[1];
  re_word = exp[2];

  if (result == 0)		/* L <= R is false */
    return Pl_Fd_Lt_2(re_word, le_word);

  if (result == 1)		/* L <= R is true */
    return Pl_Fd_Lte_2(le_word, re_word);

  *load_word = Tag_REF(Pl_Fd_New_Bool_Variable());

#ifdef DEBUG
  cur_op = (pl_full_ac) ? "truth#=<#" : "truth#=<";
#endif

  if (!Pl_Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word, &r_word)
      || !Pl_Term_Math_Loading(l_word, r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      return Pl_Get_Integer(c <= 0, *load_word);

    case MASK_LEFT:
      if (c > 0)
	return Pl_Get_Integer(0, *load_word);

      PRIM_CSTR_3(pl_truth_x_lte_c, l_word, Tag_INT(-c), *load_word);
      return TRUE;

    case MASK_RIGHT:
      if (c <= 0)
	return Pl_Get_Integer(1, *load_word);

      PRIM_CSTR_3(pl_truth_x_gte_c, r_word, Tag_INT(c), *load_word);
      return TRUE;
    }

  if (c > 0)
    {
      PRIM_CSTR_4(pl_truth_x_plus_c_lte_y, l_word, Tag_INT(c), r_word,
		  *load_word);
      return TRUE;
    }

  if (c < 0)
    {
      PRIM_CSTR_4(pl_truth_x_plus_c_gte_y, r_word, Tag_INT(-c), l_word,
		  *load_word);
      return TRUE;
    }


  PRIM_CSTR_3(pl_truth_x_lte_y, l_word, r_word, *load_word);
  return TRUE;
}






/*-------------------------------------------------------------------------*
 * PL_FD_REIFIED_IN                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Reified_In(WamWord x_word, WamWord l_word, WamWord u_word, WamWord b_word)
{
  WamWord word, tag_mask;
  WamWord b_tag_mask, x_tag_mask;
  WamWord *adr, *fdv_adr;
  PlLong x;
  PlLong b = -1;		/* a var */
  int min, max;
  int x_min, x_max;
  Range *r;
 
  //  Bool pl_fd_domain(WamWord x_word, WamWord l_word, WamWord u_word);
  /* from fd_values_c.c (optimized version) */
  Bool Pl_Fd_Domain_Interval(WamWord x_word, int min, int max);

  /* from fd_values_fd.fd */
  Bool pl_fd_not_domain(WamWord x_word, WamWord l_word, WamWord u_word);


  min = Pl_Fd_Prolog_To_Value(l_word);
  if (min < 0)
    min = 0;
  max = Pl_Fd_Prolog_To_Value(u_word);


  DEREF(x_word, word, tag_mask);
  x_word = word;
  x_tag_mask = tag_mask;

  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_FDV_MASK && tag_mask != TAG_INT_MASK)
    {
    err_type_fd:
      Pl_Err_Type(pl_type_fd_variable, word);
      return FALSE;
    }

  DEREF(b_word, word, tag_mask);
  b_word = word;
  b_tag_mask = tag_mask;
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_FDV_MASK && tag_mask != TAG_INT_MASK)
    goto err_type_fd;

  if (x_tag_mask == TAG_INT_MASK)
    {
      x = UnTag_INT(x_word);
      b = (x >= min) && (x <= max);
    unif_b:
      return Pl_Get_Integer(b, b_word);
    }

  if (b_tag_mask == TAG_INT_MASK)
    {
      b = UnTag_INT(b_word);
      if (b == 0)
	return pl_fd_not_domain(x_word, l_word, u_word);
      return (b == 1) && Pl_Fd_Domain_Interval(x_word, min, max);
    }


  if (x_tag_mask == TAG_REF_MASK) /* make an FD var */
    {
      adr = UnTag_REF(x_word);
      fdv_adr = Pl_Fd_New_Variable();
      Bind_UV(adr, Tag_REF(fdv_adr));
    }
  else
    fdv_adr = UnTag_FDV(x_word);

  r = Range(fdv_adr);

  x_min = r->min;
  x_max = r->max;

  if (x_min >= min && x_max <= max)
    {
      b = 1;
      goto unif_b;
    }

  if (min > max || x_max < min || x_min > max) /* NB: if L..U is empty then B = 0 */
    {
      b = 0;
      goto unif_b;
    }


  if (!Pl_Fd_Check_For_Bool_Var(b_word))
    return FALSE;

  PRIM_CSTR_4(pl_truth_x_in_l_u, x_word, l_word, u_word, b_word);

  return TRUE;
}
