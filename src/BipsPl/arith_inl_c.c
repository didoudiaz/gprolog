/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : arith_inl_c.c                                                   *
 * Descr.: arithmetic (inline) management - C part                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define OBJ_INIT Arith_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#ifdef M_ix86_win32
#define rint(x)  (floor((x) + (double) 0.5))
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define START_ARITH_TBL_SIZE       64




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  WamWord f_n;
  WamWord (FC *fct) ();
}
ArithInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char *arith_tbl;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static WamWord Make_Tagged_Float(double d);

static double To_Double(WamWord x);

static WamWord Load_Math_Expression(WamWord exp);



#define ADD_ARITH_OPER(atom_str, arity, f)                      \
  arith_info.f_n = Functor_Arity(Create_Atom(atom_str), arity); \
  arith_info.fct = f;                                           \
  Hash_Insert(arith_tbl, (char *) &arith_info, FALSE)




/*-------------------------------------------------------------------------*
 * ARITH_INITIALIZER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Arith_Initializer(void)
{
  ArithInf arith_info;

  arith_tbl = Hash_Alloc_Table(START_ARITH_TBL_SIZE, sizeof(ArithInf));

  ADD_ARITH_OPER("+", 1, Fct_Identity);

  ADD_ARITH_OPER("-", 1, Fct_Neg);
  ADD_ARITH_OPER("inc", 1, Fct_Inc);
  ADD_ARITH_OPER("dec", 1, Fct_Dec);
  ADD_ARITH_OPER("+", 2, Fct_Add);
  ADD_ARITH_OPER("-", 2, Fct_Sub);
  ADD_ARITH_OPER("*", 2, Fct_Mul);
  ADD_ARITH_OPER("//", 2, Fct_Div);
  ADD_ARITH_OPER("/", 2, Fct_Float_Div);
  ADD_ARITH_OPER("mod", 2, Fct_Mod);
  ADD_ARITH_OPER("/\\", 2, Fct_And);
  ADD_ARITH_OPER("\\/", 2, Fct_Or);
  ADD_ARITH_OPER("^", 2, Fct_Xor);
  ADD_ARITH_OPER("\\", 1, Fct_Not);
  ADD_ARITH_OPER("<<", 2, Fct_Shl);
  ADD_ARITH_OPER(">>", 2, Fct_Shr);
  ADD_ARITH_OPER("abs", 1, Fct_Abs);
  ADD_ARITH_OPER("sign", 1, Fct_Sign);

  ADD_ARITH_OPER("min", 2, Fct_Min);
  ADD_ARITH_OPER("max", 2, Fct_Max);
  ADD_ARITH_OPER("**", 2, Fct_Pow);
  ADD_ARITH_OPER("sqrt", 1, Fct_Sqrt);
  ADD_ARITH_OPER("atan", 1, Fct_Atan);
  ADD_ARITH_OPER("cos", 1, Fct_Cos);
  ADD_ARITH_OPER("acos", 1, Fct_Acos);
  ADD_ARITH_OPER("sin", 1, Fct_Sin);
  ADD_ARITH_OPER("asin", 1, Fct_Asin);
  ADD_ARITH_OPER("exp", 1, Fct_Exp);
  ADD_ARITH_OPER("log", 1, Fct_Log);
  ADD_ARITH_OPER("float", 1, Fct_Float);
  ADD_ARITH_OPER("ceiling", 1, Fct_Ceiling);
  ADD_ARITH_OPER("floor", 1, Fct_Floor);
  ADD_ARITH_OPER("round", 1, Fct_Round);
  ADD_ARITH_OPER("truncate", 1, Fct_Truncate);
  ADD_ARITH_OPER("float_fractional_part", 1, Fct_Float_Fract_Part);
  ADD_ARITH_OPER("float_integer_part", 1, Fct_Float_Integ_Part);

  ADD_ARITH_OPER("rem", 2, Fct_Rem);
}




/*-------------------------------------------------------------------------*
 * DEFINE_MATH_BIP_2                                                       *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Define_Math_Bip_2(WamWord func_word, WamWord arity_word)
{
  char *cur_bip_func;
  int cur_bip_arity;

  cur_bip_func = Rd_String_Check(func_word);
  cur_bip_arity = Rd_Integer_Check(arity_word);
  Set_C_Bip_Name(cur_bip_func, cur_bip_arity);
}




/*-------------------------------------------------------------------------*
 * MATH_LOAD_VALUE                                                         *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Math_Load_Value(WamWord start_word, WamWord *word_adr)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  if (tag_mask != TAG_INT_MASK && tag_mask != TAG_FLT_MASK)
    word = Load_Math_Expression(word);

  *word_adr = word;
}




/*-------------------------------------------------------------------------*
 * MATH_FAST_LOAD_VALUE                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  *word_adr = word;
}




/*-------------------------------------------------------------------------*
 * MAKE_TAGGED_FLOAT                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Make_Tagged_Float(double d)
{
  WamWord x = Tag_FLT(H);

  Global_Push_Float(d);

  return x;
}




/*-------------------------------------------------------------------------*
 * TO_DOUBLE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static double
To_Double(WamWord x)
{
  return (Tag_Is_INT(x)) ? (double) (UnTag_INT(x)) : 
    Obtain_Float(UnTag_FLT(x));
}




/*-------------------------------------------------------------------------*
 * LOAD_MATH_EXPRESSION                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Load_Math_Expression(WamWord exp)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *lst_adr;
  ArithInf *arith;

  DEREF(exp, word, tag_mask);

  if (tag_mask == TAG_INT_MASK || tag_mask == TAG_FLT_MASK)
    return word;

  if (tag_mask == TAG_LST_MASK)
    {
      lst_adr = UnTag_LST(word);
      DEREF(Cdr(lst_adr), word, tag_mask);
      if (word != NIL_WORD)
	{
	  word = Put_Structure(ATOM_CHAR('/'), 2);
	  Unify_Atom(ATOM_CHAR('.'));
	  Unify_Integer(2);
	  Pl_Err_Type(type_evaluable, word);
	}
      return Load_Math_Expression(Car(lst_adr));
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);

      arith = (ArithInf *) Hash_Find(arith_tbl, Functor_And_Arity(adr));
      if (arith == NULL)
	{
	  word = Put_Structure(ATOM_CHAR('/'), 2);
	  Unify_Atom(Functor(adr));
	  Unify_Integer(Arity(adr));
	  Pl_Err_Type(type_evaluable, word);
	}
      
      if (Arity(adr) == 1)
	return (*(arith->fct)) (Load_Math_Expression(Arg(adr, 0)));

      return (*(arith->fct)) (Load_Math_Expression(Arg(adr, 0)),
			      Load_Math_Expression(Arg(adr, 1)));
    }

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask == TAG_ATM_MASK)
    {
      word = Put_Structure(ATOM_CHAR('/'), 2);
      Unify_Value(exp);
      Unify_Integer(0);		/* then type_error */
    }

  Pl_Err_Type(type_evaluable, word);
  return word;
}




/*-------------------------------------------------------------------------*
 * ARITH_EVAL_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Arith_Eval_2(WamWord exp_word, WamWord x_word)
{
  return Unify(Load_Math_Expression(exp_word), x_word);
}




	  /* Mathematic Operations */

#define C_Neg(x)     (- (x))

#define C_Add(x, y)  ((x) + (y))

#define C_Sub(x, y)  ((x) - (y))

#define C_Mul(x, y)  ((x) * (y))

#define C_Div(x, y)  ((y) != 0 ? (x) / (y) : (Pl_Err_Evaluation(evluation_zero_divisor), 0))

#define Identity(x)  (x)

#define DInc(x)      ((x) + 1)

#define DDec(x)      ((x) - 1)

#define DSign(x)     ((x) < 0.0 ? -1.0 : (x) > 0.0 ? 1.0 : 0.0)

#define DInteg(x)    (((x) > 0) ? floor(x) : ceil(x))

#define DFract(x)    ((x) - DInteg(x))


#define X_and_Y_are_INT(x, y)  Tag_Is_INT(x & y)


#define IFxIFtoIF(x, y, c_op, fast_op)                     \
  return (X_and_Y_are_INT(x, y))                           \
    ? fast_op(x, y)                                        \
    : Make_Tagged_Float(c_op(To_Double(x), To_Double(y)))



#define IFxIFtoF(x, y, c_op)                                \
  return Make_Tagged_Float(c_op(To_Double(x), To_Double(y)))



#define IxItoI(x, y, fast_op)                    \
  if (Tag_Is_FLT(x))		/* error case */ \
    Pl_Err_Type(type_integer, x);                \
  if (Tag_Is_FLT(y))		/* error case */ \
    Pl_Err_Type(type_integer, y);                \
  return fast_op(x, y)



#define IFtoIF(x, c_op, fast_op)                 \
  return (Tag_Is_INT(x)) ? fast_op(x) :          \
    Make_Tagged_Float(c_op(To_Double(x)))



#define ItoI(x, fast_op)                         \
  if (Tag_Is_FLT(x))            /* error case */ \
    Pl_Err_Type(type_integer, x);                \
  return fast_op(x)



#define IFtoF(x, c_op)                            \
  return Make_Tagged_Float(c_op(To_Double(x)))



#define FtoI(x, c_op)                            \
  double d;                                      \
  if (Tag_Is_INT(x))            /* error case */ \
    Pl_Err_Type(type_float, x);                  \
  else                                           \
    d = Obtain_Float(UnTag_FLT(x));              \
  return Tag_INT((long) c_op(d))



#define FtoF(x, c_op)                            \
  double d;                                      \
  if (Tag_Is_INT(x))            /* error case */ \
    Pl_Err_Type(type_float, x);                  \
  else                                           \
    d = Obtain_Float(UnTag_FLT(x));              \
  return Make_Tagged_Float(c_op(d))



	  /* fast-math version */

WamWord FC
Fct_Fast_Neg(WamWord x)
{
  long vx = UnTag_INT(x);
  return Tag_INT(-vx);
}

WamWord FC
Fct_Fast_Inc(WamWord x)
{
  long vx = UnTag_INT(x);
  return Tag_INT(vx + 1);
}

WamWord FC
Fct_Fast_Dec(WamWord x)
{
  long vx = UnTag_INT(x);
  return Tag_INT(vx - 1);
}

WamWord FC
Fct_Fast_Add(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return Tag_INT(vx + vy);
}

WamWord FC
Fct_Fast_Sub(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return Tag_INT(vx - vy);
}

WamWord FC
Fct_Fast_Mul(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return Tag_INT(vx * vy);
}

WamWord FC
Fct_Fast_Div(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);

  if (vy == 0)
    Pl_Err_Evaluation(evluation_zero_divisor);

  return Tag_INT(vx / vy);
}

WamWord FC
Fct_Fast_Rem(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);

  if (vy == 0)
    Pl_Err_Evaluation(evluation_zero_divisor);

  return Tag_INT(vx % vy);
}

WamWord FC
Fct_Fast_Mod(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  long m;

  if (vy == 0)
    Pl_Err_Evaluation(evluation_zero_divisor);

  m = vx % vy;

  if (m != 0 && (m ^ vy) < 0)	/* have m and vy different signs ? */
    m += vy;

  return Tag_INT(m);
}

WamWord FC
Fct_Fast_And(WamWord x, WamWord y)
{
  return x & y;
}

WamWord FC
Fct_Fast_Or(WamWord x, WamWord y)
{
  return x | y;
}

WamWord FC
Fct_Fast_Xor(WamWord x, WamWord y)
{
  return (x ^ y) | TAG_INT_MASK;
}

WamWord FC
Fct_Fast_Not(WamWord x)
{
  long vx = UnTag_INT(x);
  return Tag_INT(~vx);
}

WamWord FC
Fct_Fast_Shl(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return Tag_INT(vx << vy);
}

WamWord FC
Fct_Fast_Shr(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return Tag_INT(vx >> vy);
}

WamWord FC
Fct_Fast_Abs(WamWord x)
{
  long vx = UnTag_INT(x);
  return (vx < 0) ? Tag_INT(-vx) : x;
}

WamWord FC
Fct_Fast_Sign(WamWord x)
{
  long vx = UnTag_INT(x);
  return (vx < 0) ? Tag_INT(-1) : (vx == 0) ? Tag_INT(0) : Tag_INT(1);
}



	  /* standard version */

WamWord FC
Fct_Neg(WamWord x)
{
  IFtoIF(x, C_Neg, Fct_Fast_Neg);
}

WamWord FC
Fct_Inc(WamWord x)
{
  IFtoIF(x, DInc, Fct_Fast_Inc);
}

WamWord FC
Fct_Dec(WamWord x)
{
  IFtoIF(x, DDec, Fct_Fast_Dec);
}

WamWord FC
Fct_Add(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Add, Fct_Fast_Add);
}

WamWord FC
Fct_Sub(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Sub, Fct_Fast_Sub);
}

WamWord FC
Fct_Mul(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Mul, Fct_Fast_Mul);
}

WamWord FC
Fct_Div(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Div);
}

WamWord FC
Fct_Float_Div(WamWord x, WamWord y)
{
  IFxIFtoF(x, y, C_Div);
}

WamWord FC
Fct_Rem(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Rem);
}

WamWord FC
Fct_Mod(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Mod);
}

WamWord FC
Fct_And(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_And);
}

WamWord FC
Fct_Or(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Or);
}

WamWord FC
Fct_Xor(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Xor);
}

WamWord FC
Fct_Not(WamWord x)
{
  ItoI(x, Fct_Fast_Not);
}

WamWord FC
Fct_Shl(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Shl);
}

WamWord FC
Fct_Shr(WamWord x, WamWord y)
{
  IxItoI(x, y, Fct_Fast_Shr);
}

WamWord FC
Fct_Abs(WamWord x)
{
  IFtoIF(x, fabs, Fct_Fast_Abs);
}

WamWord FC
Fct_Sign(WamWord x)
{
  IFtoIF(x, DSign, Fct_Fast_Sign);
}

WamWord FC
Fct_Min(WamWord x, WamWord y)
{
  double dx = To_Double(x);
  double dy = To_Double(y);

  if (dx < dy)
    return x;

  if (dx > dy)
    return y;

  return Tag_Is_INT(x) ? x : y;
}

WamWord FC
Fct_Max(WamWord x, WamWord y)
{
  double dx = To_Double(x);
  double dy = To_Double(y);

  if (dx > dy)
    return x;

  if (dx < dy)
    return y;

  return Tag_Is_INT(x) ? x : y;
}

WamWord FC
Fct_Pow(WamWord x, WamWord y)
{
  IFxIFtoF(x, y, pow);
}

WamWord FC
Fct_Sqrt(WamWord x)
{
  IFtoF(x, sqrt);
}

WamWord FC
Fct_Atan(WamWord x)
{
  IFtoF(x, atan);
}

WamWord FC
Fct_Cos(WamWord x)
{
  IFtoF(x, cos);
}

WamWord FC
Fct_Acos(WamWord x)
{
  IFtoF(x, acos);
}

WamWord FC
Fct_Sin(WamWord x)
{
  IFtoF(x, sin);
}

WamWord FC
Fct_Asin(WamWord x)
{
  IFtoF(x, asin);
}

WamWord FC
Fct_Exp(WamWord x)
{
  IFtoF(x, exp);
}

WamWord FC
Fct_Log(WamWord x)
{
  IFtoF(x, log);
}

WamWord FC
Fct_Float(WamWord x)
{
  IFtoF(x, Identity);
}

WamWord FC
Fct_Ceiling(WamWord x)
{
  FtoI(x, ceil);
}

WamWord FC
Fct_Floor(WamWord x)
{
  FtoI(x, floor);
}

WamWord FC
Fct_Round(WamWord x)
{
  FtoI(x, rint);
}

WamWord FC
Fct_Truncate(WamWord x)
{
  FtoI(x, Identity);
}

WamWord FC
Fct_Float_Fract_Part(WamWord x)
{
  FtoF(x, DFract);
}

WamWord FC
Fct_Float_Integ_Part(WamWord x)
{
  FtoF(x, DInteg);
}

WamWord FC
Fct_Identity(WamWord x)
{
  return x;
}				/* for meta-call */


	  /* Mathematic Comparisons */

#define Cmp_IFxIF(x, y, c_op, fast_op)     \
  return (X_and_Y_are_INT(x, y))           \
    ? fast_op(x, y)                        \
    : (To_Double(x) c_op To_Double(y))


	  /* fast-math version */

Bool FC
Blt_Fast_Eq(WamWord x, WamWord y)
{
  return x == y;
}

Bool FC
Blt_Fast_Neq(WamWord x, WamWord y)
{
  return x != y;
}

Bool FC
Blt_Fast_Lt(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return vx < vy;
}

Bool FC
Blt_Fast_Lte(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return vx <= vy;
}

Bool FC
Blt_Fast_Gt(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return vx > vy;
}

Bool FC
Blt_Fast_Gte(WamWord x, WamWord y)
{
  long vx = UnTag_INT(x);
  long vy = UnTag_INT(y);
  return vx >= vy;
}


	  /* standard version */

Bool FC
Blt_Eq(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, ==, Blt_Fast_Eq);
}
Bool FC
Blt_Neq(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, !=, Blt_Fast_Neq);
}
Bool FC
Blt_Lt(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, <, Blt_Fast_Lt);
}
Bool FC
Blt_Lte(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, <=, Blt_Fast_Lte);
}
Bool FC
Blt_Gt(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, >, Blt_Fast_Gt);
}
Bool FC
Blt_Gte(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, >=, Blt_Fast_Gte);
}
