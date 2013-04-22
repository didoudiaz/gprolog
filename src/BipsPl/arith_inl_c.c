/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : arith_inl_c.c                                                   *
 * Descr.: arithmetic (inline) management - C part                         *
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


#include <stdlib.h>
#include <string.h>
#include <math.h>

#define OBJ_INIT Arith_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#ifdef _MSC_VER
#define rint(x)  (floor((x) + (double) 0.5))
#endif


/* PI */

#ifndef M_PI
#define M_PI 3.1415926535897932384
#endif


#ifndef M_E
#define M_E  2.7182818284590452354
#endif

/* Difference between 1.0 and the minimum double greater than 1.0 */
#ifndef DBL_EPSILON

#ifdef __DBL_EPSILON__
#define DBL_EPSILON __DBL_EPSILON__
#else
#define DBL_EPSILON 2.2204460492503131e-16  /* C double (64 bits IEEE encoding) */
#endif

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

static int atom_pi;
static int atom_e;
static int atom_epsilon;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static WamWord Make_Tagged_Float(double d);

static double To_Double(WamWord x);

static WamWord Load_Math_Expression(WamWord exp);



#define ADD_ARITH_OPER(atom_str, arity, f)                         \
  arith_info.f_n = Functor_Arity(Pl_Create_Atom(atom_str), arity); \
  arith_info.fct = f;                                              \
  Pl_Hash_Insert(arith_tbl, (char *) &arith_info, FALSE)




/*-------------------------------------------------------------------------*
 * ARITH_INITIALIZER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Arith_Initializer(void)
{
  ArithInf arith_info;

  arith_tbl = Pl_Hash_Alloc_Table(START_ARITH_TBL_SIZE, sizeof(ArithInf));

  ADD_ARITH_OPER("+", 1, Pl_Fct_Identity);

  ADD_ARITH_OPER("-", 1, Pl_Fct_Neg);
  ADD_ARITH_OPER("inc", 1, Pl_Fct_Inc);
  ADD_ARITH_OPER("dec", 1, Pl_Fct_Dec);
  ADD_ARITH_OPER("+", 2, Pl_Fct_Add);
  ADD_ARITH_OPER("-", 2, Pl_Fct_Sub);
  ADD_ARITH_OPER("*", 2, Pl_Fct_Mul);
  ADD_ARITH_OPER("//", 2, Pl_Fct_Div);
  ADD_ARITH_OPER("/", 2, Pl_Fct_Float_Div);
  ADD_ARITH_OPER("rem", 2, Pl_Fct_Rem);
  ADD_ARITH_OPER("mod", 2, Pl_Fct_Mod);
  ADD_ARITH_OPER("div", 2, Pl_Fct_Div2);
  ADD_ARITH_OPER("/\\", 2, Pl_Fct_And);
  ADD_ARITH_OPER("\\/", 2, Pl_Fct_Or);
  ADD_ARITH_OPER("xor", 2, Pl_Fct_Xor);
  ADD_ARITH_OPER("\\", 1, Pl_Fct_Not);
  ADD_ARITH_OPER("<<", 2, Pl_Fct_Shl);
  ADD_ARITH_OPER(">>", 2, Pl_Fct_Shr);
  ADD_ARITH_OPER("lsb", 1, Pl_Fct_LSB);
  ADD_ARITH_OPER("msb", 1, Pl_Fct_MSB);
  ADD_ARITH_OPER("popcount", 1, Pl_Fct_Popcount);
  ADD_ARITH_OPER("abs", 1, Pl_Fct_Abs);
  ADD_ARITH_OPER("sign", 1, Pl_Fct_Sign);

  ADD_ARITH_OPER("gcd", 2, Pl_Fct_GCD);
  ADD_ARITH_OPER("min", 2, Pl_Fct_Min);
  ADD_ARITH_OPER("max", 2, Pl_Fct_Max);
  ADD_ARITH_OPER("^", 2, Pl_Fct_Integer_Pow);
  ADD_ARITH_OPER("**", 2, Pl_Fct_Pow);
  ADD_ARITH_OPER("sqrt", 1, Pl_Fct_Sqrt);
  ADD_ARITH_OPER("tan", 1, Pl_Fct_Tan);
  ADD_ARITH_OPER("atan", 1, Pl_Fct_Atan);
  ADD_ARITH_OPER("atan2", 2, Pl_Fct_Atan2);
  ADD_ARITH_OPER("cos", 1, Pl_Fct_Cos);
  ADD_ARITH_OPER("acos", 1, Pl_Fct_Acos);
  ADD_ARITH_OPER("sin", 1, Pl_Fct_Sin);
  ADD_ARITH_OPER("asin", 1, Pl_Fct_Asin);
  ADD_ARITH_OPER("tanh", 1, Pl_Fct_Tanh);
  ADD_ARITH_OPER("atanh", 1, Pl_Fct_Atanh);
  ADD_ARITH_OPER("cosh", 1, Pl_Fct_Cosh);
  ADD_ARITH_OPER("acosh", 1, Pl_Fct_Acosh);
  ADD_ARITH_OPER("sinh", 1, Pl_Fct_Sinh);
  ADD_ARITH_OPER("asinh", 1, Pl_Fct_Asinh);
  ADD_ARITH_OPER("exp", 1, Pl_Fct_Exp);
  ADD_ARITH_OPER("log", 1, Pl_Fct_Log);
  ADD_ARITH_OPER("log10", 1, Pl_Fct_Log10);
  ADD_ARITH_OPER("log", 2, Pl_Fct_Log_Radix);
  ADD_ARITH_OPER("float", 1, Pl_Fct_Float);
  ADD_ARITH_OPER("ceiling", 1, Pl_Fct_Ceiling);
  ADD_ARITH_OPER("floor", 1, Pl_Fct_Floor);
  ADD_ARITH_OPER("round", 1, Pl_Fct_Round);
  ADD_ARITH_OPER("truncate", 1, Pl_Fct_Truncate);
  ADD_ARITH_OPER("float_fractional_part", 1, Pl_Fct_Float_Fract_Part);
  ADD_ARITH_OPER("float_integer_part", 1, Pl_Fct_Float_Integ_Part);

  atom_pi = Pl_Create_Atom("pi");
  atom_e = Pl_Create_Atom("e");
  atom_epsilon = Pl_Create_Atom("epsilon");
}




/*-------------------------------------------------------------------------*
 * PL_DEFINE_MATH_BIP_2                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Define_Math_Bip_2(WamWord func_word, WamWord arity_word)
{
  char *cur_bip_func;
  int cur_bip_arity;

  cur_bip_func = Pl_Rd_String_Check(func_word);
  cur_bip_arity = Pl_Rd_Integer_Check(arity_word);
  Pl_Set_C_Bip_Name(cur_bip_func, cur_bip_arity);
}




/*-------------------------------------------------------------------------*
 * PL_MATH_LOAD_VALUE                                                      *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Math_Load_Value(WamWord start_word, WamWord *word_adr)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  if (tag_mask != TAG_INT_MASK && tag_mask != TAG_FLT_MASK)
    word = Load_Math_Expression(word);

  *word_adr = word;
}




/*-------------------------------------------------------------------------*
 * PL_MATH_FAST_LOAD_VALUE                                                 *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr)
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

  Pl_Global_Push_Float(d);

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
    Pl_Obtain_Float(UnTag_FLT(x));
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
  int atom;

  DEREF(exp, word, tag_mask);

  if (tag_mask == TAG_INT_MASK || tag_mask == TAG_FLT_MASK)
    return word;

  if (tag_mask == TAG_LST_MASK)
    {
      lst_adr = UnTag_LST(word);
      DEREF(Cdr(lst_adr), word, tag_mask);
      if (word != NIL_WORD)
	{
	  word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
	  Pl_Unify_Atom(ATOM_CHAR('.'));
	  Pl_Unify_Integer(2);
	  Pl_Err_Type(pl_type_evaluable, word);
	}
      DEREF(Car(lst_adr), word, tag_mask);
      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (tag_mask != TAG_INT_MASK)
	{
	  Pl_Err_Type(pl_type_integer, word);
	}
      return word;
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);

      arith = (ArithInf *) Pl_Hash_Find(arith_tbl, Functor_And_Arity(adr));
      if (arith == NULL)
	{
	  word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
	  Pl_Unify_Atom(Functor(adr));
	  Pl_Unify_Integer(Arity(adr));
	  Pl_Err_Type(pl_type_evaluable, word);
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
      atom = UnTag_ATM(word);
      if (atom == atom_pi)
	return Pl_Fct_PI();

      if (atom == atom_e)
	return Pl_Fct_E();

      if (atom == atom_epsilon)
	return Pl_Fct_Epsilon();

      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Value(exp);
      Pl_Unify_Integer(0);		/* then type_error */
    }

  Pl_Err_Type(pl_type_evaluable, word);
  return word;
}




/*-------------------------------------------------------------------------*
 * PL_ARITH_EVAL_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Arith_Eval_2(WamWord exp_word, WamWord x_word)
{
  return Pl_Unify(Load_Math_Expression(exp_word), x_word);
}


/*-------------------------------------------------------------------------*
 * PL_SUCC_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Succ_2(WamWord x_word, WamWord y_word)
{
  WamWord word, tag_mask;
  PlLong x;

  DEREF(x_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Un_Positive_Check(Pl_Rd_Positive_Check(word) + 1, y_word);

  Pl_Check_For_Un_Positive(word);
  x = Pl_Rd_Positive_Check(y_word) - 1;
  return x >= 0 && Pl_Get_Integer(x, word);
}




	  /* Mathematic Operations */

#define C_Neg(x)     (- (x))

#define C_Add(x, y)  ((x) + (y))

#define C_Sub(x, y)  ((x) - (y))

#define C_Mul(x, y)  ((x) * (y))

#define C_Div(x, y)  ((y) != 0 ? (x) / (y) : (Pl_Err_Evaluation(pl_evluation_zero_divisor), 0))

#define Identity(x)  (x)

#define DInc(x)      ((x) + 1)

#define DDec(x)      ((x) - 1)

#define DSign(x)     ((x) < 0.0 ? -1.0 : (x) > 0.0 ? 1.0 : 0.0)

#define DInteg(x)    (((x) > 0) ? floor(x) : ceil(x))

#define DFract(x)    ((x) - DInteg(x))

#define Log_Radix(b, x)  (log(x) / log(b))


#define X_and_Y_are_INT(x, y)  Tag_Is_INT(x & y)


#define IFxIFtoIF(x, y, c_op, fast_op)                     \
  return (X_and_Y_are_INT(x, y))                           \
    ? fast_op(x, y)                                        \
    : Make_Tagged_Float(c_op(To_Double(x), To_Double(y)))



#define IFxIFtoF(x, y, c_op)                                \
  return Make_Tagged_Float(c_op(To_Double(x), To_Double(y)))



#define IxItoI(x, y, fast_op)                    \
  if (Tag_Is_FLT(x))		/* error case */ \
    Pl_Err_Type(pl_type_integer, x);             \
  if (Tag_Is_FLT(y))		/* error case */ \
    Pl_Err_Type(pl_type_integer, y);             \
  return fast_op(x, y)



#define IFtoIF(x, c_op, fast_op)                 \
  return (Tag_Is_INT(x)) ? fast_op(x) :          \
    Make_Tagged_Float(c_op(To_Double(x)))



#define ItoI(x, fast_op)                         \
  if (Tag_Is_FLT(x))            /* error case */ \
    Pl_Err_Type(pl_type_integer, x);             \
  return fast_op(x)



#define IFtoF(x, c_op)                            \
  return Make_Tagged_Float(c_op(To_Double(x)))



       /* FtoI is ONLY used for rounding functions */
#define FtoI(x, c_op)                            \
  double d;                                      \
  if (Tag_Is_INT(x))            /* error case */ \
    {                                            \
      if (Flag_Value(strict_iso))           \
         Pl_Err_Type(pl_type_float, x);          \
      else                                       \
         return x;                               \
    }						 \
  else                                           \
    d = Pl_Obtain_Float(UnTag_FLT(x));           \
  return Tag_INT((PlLong) c_op(d))



#define FtoF(x, c_op)                            \
  double d;                                      \
  if (Tag_Is_INT(x))            /* error case */ \
    Pl_Err_Type(pl_type_float, x);               \
  else                                           \
    d = Pl_Obtain_Float(UnTag_FLT(x));           \
  return Make_Tagged_Float(c_op(d))



	  /* fast-math version */

WamWord FC
Pl_Fct_Fast_Neg(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT(-vx);
}

WamWord FC
Pl_Fct_Fast_Inc(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT(vx + 1);
}

WamWord FC
Pl_Fct_Fast_Dec(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT(vx - 1);
}

WamWord FC
Pl_Fct_Fast_Add(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return Tag_INT(vx + vy);
}

WamWord FC
Pl_Fct_Fast_Sub(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return Tag_INT(vx - vy);
}

WamWord FC
Pl_Fct_Fast_Mul(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return Tag_INT(vx * vy);
}

WamWord FC
Pl_Fct_Fast_Div(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);

  if (vy == 0)
    Pl_Err_Evaluation(pl_evluation_zero_divisor);

  return Tag_INT(vx / vy);
}

WamWord FC
Pl_Fct_Fast_Rem(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);

  if (vy == 0)
    Pl_Err_Evaluation(pl_evluation_zero_divisor);

  return Tag_INT(vx % vy);
}

WamWord FC
Pl_Fct_Fast_Mod(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  PlLong m;

  if (vy == 0)
    Pl_Err_Evaluation(pl_evluation_zero_divisor);

  m = vx % vy;

  if (m != 0 && (m ^ vy) < 0)	/* have m and vy different signs ? */
    m += vy;

  return Tag_INT(m);
}

WamWord FC
Pl_Fct_Fast_Div2(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  PlLong m;

  if (vy == 0)
    Pl_Err_Evaluation(pl_evluation_zero_divisor);

  m = vx % vy;

  if (m != 0 && (m ^ vy) < 0)	/* have m and vy different signs ? */
    m += vy;

  m = (vx - m) / vy;

  return Tag_INT(m);
}

WamWord FC
Pl_Fct_Fast_And(WamWord x, WamWord y)
{
  return x & y;
}

WamWord FC
Pl_Fct_Fast_Or(WamWord x, WamWord y)
{
  return x | y;
}

WamWord FC
Pl_Fct_Fast_Xor(WamWord x, WamWord y)
{
  return (x ^ y) | TAG_INT_MASK;
}

WamWord FC
Pl_Fct_Fast_Not(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT(~vx);
}


WamWord FC
Pl_Fct_Fast_Shl(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return Tag_INT(vx << vy);
}

WamWord FC
Pl_Fct_Fast_Shr(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return Tag_INT(vx >> vy);
}

WamWord FC
Pl_Fct_Fast_LSB(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT((vx == 0) ? -1 : Pl_Least_Significant_Bit(vx));
}

WamWord FC
Pl_Fct_Fast_MSB(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT((vx == 0) ? -1 : Pl_Most_Significant_Bit(vx));
}

WamWord FC
Pl_Fct_Fast_Popcount(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return Tag_INT(Pl_Count_Set_Bits(vx));
}

WamWord FC
Pl_Fct_Fast_Abs(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return (vx < 0) ? Tag_INT(-vx) : x;
}

WamWord FC
Pl_Fct_Fast_Sign(WamWord x)
{
  PlLong vx = UnTag_INT(x);
  return (vx < 0) ? Tag_INT(-1) : (vx == 0) ? Tag_INT(0) : Tag_INT(1);
}

WamWord FC
Pl_Fct_Fast_GCD(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);

  if (vx < 0)
    vx = -vx;

  if (vy < 0)
    vy = -vy;

  while(vy != 0)
    {
      PlLong r = vx % vy;
      vx = vy;
      vy = r;
    }
  return Tag_INT(vx);
}




WamWord FC
Pl_Fct_Fast_Integer_Pow(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  PlLong p = (PlLong) pow(vx, vy);
  return Tag_INT(p);
}



	  /* standard version */

WamWord FC
Pl_Fct_Neg(WamWord x)
{
  IFtoIF(x, C_Neg, Pl_Fct_Fast_Neg);
}

WamWord FC
Pl_Fct_Inc(WamWord x)
{
  IFtoIF(x, DInc, Pl_Fct_Fast_Inc);
}

WamWord FC
Pl_Fct_Dec(WamWord x)
{
  IFtoIF(x, DDec, Pl_Fct_Fast_Dec);
}

WamWord FC
Pl_Fct_Add(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Add, Pl_Fct_Fast_Add);
}

WamWord FC
Pl_Fct_Sub(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Sub, Pl_Fct_Fast_Sub);
}

WamWord FC
Pl_Fct_Mul(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, C_Mul, Pl_Fct_Fast_Mul);
}

WamWord FC
Pl_Fct_Div(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Div);
}

WamWord FC
Pl_Fct_Float_Div(WamWord x, WamWord y)
{
  IFxIFtoF(x, y, C_Div);
}

WamWord FC
Pl_Fct_Rem(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Rem);
}

WamWord FC
Pl_Fct_Mod(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Mod);
}

WamWord FC
Pl_Fct_Div2(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Div2);
}

WamWord FC
Pl_Fct_And(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_And);
}

WamWord FC
Pl_Fct_Or(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Or);
}

WamWord FC
Pl_Fct_Xor(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Xor);
}

WamWord FC
Pl_Fct_Not(WamWord x)
{
  ItoI(x, Pl_Fct_Fast_Not);
}

WamWord FC
Pl_Fct_Shl(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Shl);
}

WamWord FC
Pl_Fct_Shr(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_Shr);
}

WamWord FC
Pl_Fct_LSB(WamWord x)
{
  ItoI(x, Pl_Fct_Fast_LSB);
}

WamWord FC
Pl_Fct_MSB(WamWord x)
{
  ItoI(x, Pl_Fct_Fast_MSB);
}

WamWord FC
Pl_Fct_Popcount(WamWord x)
{
  ItoI(x, Pl_Fct_Fast_Popcount);
}

WamWord FC
Pl_Fct_Abs(WamWord x)
{
  IFtoIF(x, fabs, Pl_Fct_Fast_Abs);
}

WamWord FC
Pl_Fct_Sign(WamWord x)
{
  IFtoIF(x, DSign, Pl_Fct_Fast_Sign);
}


WamWord FC
Pl_Fct_GCD(WamWord x, WamWord y)
{
  IxItoI(x, y, Pl_Fct_Fast_GCD);
}



WamWord FC
Pl_Fct_Min(WamWord x, WamWord y)
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
Pl_Fct_Max(WamWord x, WamWord y)
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
Pl_Fct_Integer_Pow(WamWord x, WamWord y)
{
  IFxIFtoIF(x, y, pow, Pl_Fct_Fast_Integer_Pow);
}



WamWord FC
Pl_Fct_Pow(WamWord x, WamWord y)
{
  IFxIFtoF(x, y, pow);
}

WamWord FC
Pl_Fct_Sqrt(WamWord x)
{
  IFtoF(x, sqrt);
}

WamWord FC
Pl_Fct_Tan(WamWord x)
{
  IFtoF(x, tan);
}

WamWord FC
Pl_Fct_Atan(WamWord x)
{
  IFtoF(x, atan);
}

WamWord FC
Pl_Fct_Atan2(WamWord x, WamWord y)
{
  IFxIFtoF(x, y, atan2);
}

WamWord FC
Pl_Fct_Cos(WamWord x)
{
  IFtoF(x, cos);
}

WamWord FC
Pl_Fct_Acos(WamWord x)
{
  IFtoF(x, acos);
}

WamWord FC
Pl_Fct_Sin(WamWord x)
{
  IFtoF(x, sin);
}

WamWord FC
Pl_Fct_Asin(WamWord x)
{
  IFtoF(x, asin);
}

WamWord FC
Pl_Fct_Tanh(WamWord x)
{
  IFtoF(x, tanh);
}

WamWord FC
Pl_Fct_Atanh(WamWord x)
{
#ifdef HAVE_ATANH
  IFtoF(x, atanh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable function"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Cosh(WamWord x)
{
  IFtoF(x, cosh);
}

WamWord FC
Pl_Fct_Acosh(WamWord x)
{
#ifdef HAVE_ACOSH
  IFtoF(x, acosh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable function"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Sinh(WamWord x)
{
  IFtoF(x, sinh);
}

WamWord FC
Pl_Fct_Asinh(WamWord x)
{
#ifdef HAVE_ASINH
  IFtoF(x, asinh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable function"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Exp(WamWord x)
{
  IFtoF(x, exp);
}

WamWord FC
Pl_Fct_Log(WamWord x)
{
  IFtoF(x, log);
}

WamWord FC
Pl_Fct_Log10(WamWord x)
{
  IFtoF(x, log10);
}

WamWord FC
Pl_Fct_Log_Radix(WamWord b, WamWord x)
{
  IFxIFtoF(b, x, Log_Radix);
}

WamWord FC
Pl_Fct_Float(WamWord x)
{
  IFtoF(x, Identity);
}

WamWord FC
Pl_Fct_Ceiling(WamWord x)
{
  FtoI(x, ceil);
}

WamWord FC
Pl_Fct_Floor(WamWord x)
{
  FtoI(x, floor);
}

WamWord FC
Pl_Fct_Round(WamWord x)
{
  FtoI(x, rint);
}

WamWord FC
Pl_Fct_Truncate(WamWord x)
{
  FtoI(x, Identity);
}

WamWord FC
Pl_Fct_Float_Fract_Part(WamWord x)
{
  FtoF(x, DFract);
}

WamWord FC
Pl_Fct_Float_Integ_Part(WamWord x)
{
  FtoF(x, DInteg);
}

WamWord FC
Pl_Fct_PI(void)
{
  return Make_Tagged_Float(M_PI);
}


WamWord FC
Pl_Fct_E(void)
{
  return Make_Tagged_Float(M_E);
}


WamWord FC
Pl_Fct_Epsilon(void)
{
  return Make_Tagged_Float(DBL_EPSILON);
}


WamWord FC
Pl_Fct_Identity(WamWord x)
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
Pl_Blt_Fast_Eq(WamWord x, WamWord y)
{
  return x == y;
}

Bool FC
Pl_Blt_Fast_Neq(WamWord x, WamWord y)
{
  return x != y;
}

Bool FC
Pl_Blt_Fast_Lt(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return vx < vy;
}

Bool FC
Pl_Blt_Fast_Lte(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return vx <= vy;
}

Bool FC
Pl_Blt_Fast_Gt(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return vx > vy;
}

Bool FC
Pl_Blt_Fast_Gte(WamWord x, WamWord y)
{
  PlLong vx = UnTag_INT(x);
  PlLong vy = UnTag_INT(y);
  return vx >= vy;
}


	  /* standard version */

Bool FC
Pl_Blt_Eq(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, ==, Pl_Blt_Fast_Eq);
}
Bool FC
Pl_Blt_Neq(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, !=, Pl_Blt_Fast_Neq);
}
Bool FC
Pl_Blt_Lt(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, <, Pl_Blt_Fast_Lt);
}
Bool FC
Pl_Blt_Lte(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, <=, Pl_Blt_Fast_Lte);
}
Bool FC
Pl_Blt_Gt(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, >, Pl_Blt_Fast_Gt);
}
Bool FC
Pl_Blt_Gte(WamWord x, WamWord y)
{
  Cmp_IFxIF(x, y, >=, Pl_Blt_Fast_Gte);
}
