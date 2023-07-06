/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : arith_inl_c.c                                                   *
 * Descr.: arithmetic (inline) management - C part                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

/* ISO round/1 (neither lrint nor lround) - see Core 1, section 9.1.6.1 */
#define pl_round(x)  (floor((x) + (double) 0.5))


/* PI and E */

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


#ifndef HAVE_ISNAN		/* isnan is C99 */
#define isnan(x) ((x) != (x))
#endif
#ifndef HAVE_ISINF		/* isinf is C99 */
#define isinf(x) FALSE
#endif


#ifndef DBL_MANT_DIG
#define DBL_MANT_DIG 53
#endif

  /* Largest int64_t value that can be stored in a double without.
   * Using C99 hexadecimal floating point literal notation = 0x1.0p53 
   */
#define MAX_INT_OK_IN_DBL   ((double) ((PlLong) 1 << DBL_MANT_DIG))


#ifndef LDBL_MANT_DIG
#define LDBL_MANT_DIG DBL_MANT_DIG
#endif

#if DBL_MANT_DIG < LDBL_MANT_DIG
#define HAS_AN_EFFECTIVE_LONG_DOUBLE
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define START_EVALUABLE_TBL_SIZE    64




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  PlLong f_n;			/* key is <functor_atom,arity>    */
  int signat_atom;		/* signature (e.g. IFxIFtoIF)     */
  Bool is_iso;			/* normalized in ISO ?            */
  WamWord (FC *fct) ();		/* pointer to associated function */
}
EvaluableInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char *evaluable_tbl;

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static WamWord Make_Tagged_Float(double d);

static double To_Double(WamWord x_word);

static WamWord Load_Math_Expression(WamWord exp_word);




#define Pl_Lookup_Evaluable(func, arity) \
  (EvaluableInf *) Pl_Hash_Find(evaluable_tbl, Functor_Arity(func, arity))




#define ADD_EVALUABLE(atom_str, arity, signat_str, iso, f)             \
  evaluable_info.f_n = Functor_Arity(Pl_Create_Atom(atom_str), arity); \
  evaluable_info.signat_atom = Pl_Create_Atom(signat_str);             \
  evaluable_info.is_iso = iso;             			       \
  evaluable_info.fct = f;                                              \
  Pl_Hash_Insert(evaluable_tbl, (char *) &evaluable_info, FALSE)



#define TEST_INT_OVERFLOW(x) (((x) < INT_LOWEST_VALUE) || ((x) > INT_GREATEST_VALUE))




/*-------------------------------------------------------------------------*
 * ARITH_INITIALIZER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Arith_Initializer(void)
{
  EvaluableInf evaluable_info;

  evaluable_tbl = Pl_Hash_Alloc_Table(START_EVALUABLE_TBL_SIZE, sizeof(EvaluableInf));

  ADD_EVALUABLE("pi",                    0, "=F",       TRUE,  Pl_Fct_PI);
  ADD_EVALUABLE("e",                     0, "=F",       FALSE, Pl_Fct_E);
  ADD_EVALUABLE("epsilon",               0, "=F",       FALSE, Pl_Fct_Epsilon);
  ADD_EVALUABLE("+",                     1, "IF=IF",    TRUE,  Pl_Fct_Identity);
  ADD_EVALUABLE("-",                     1, "IF=IF",    TRUE,  Pl_Fct_Neg);
  ADD_EVALUABLE("inc",                   1, "IF=IF",    FALSE, Pl_Fct_Inc);
  ADD_EVALUABLE("dec",                   1, "IF=IF",    FALSE, Pl_Fct_Dec);
  ADD_EVALUABLE("+",                     2, "IF,IF=IF", TRUE,  Pl_Fct_Add);
  ADD_EVALUABLE("-",                     2, "IF,IF=IF", TRUE,  Pl_Fct_Sub);
  ADD_EVALUABLE("*",                     2, "IF,IF=IF", TRUE,  Pl_Fct_Mul);
  ADD_EVALUABLE("/",                     2, "IF,IF=F",  TRUE,  Pl_Fct_Float_Div);
  ADD_EVALUABLE("//",                    2, "I,I=I",    TRUE,  Pl_Fct_Integer_Div);
  ADD_EVALUABLE("div",                   2, "I,I=I",    TRUE,  Pl_Fct_Integer_Div2);
  ADD_EVALUABLE("rem",                   2, "I,I=I",    TRUE,  Pl_Fct_Rem);
  ADD_EVALUABLE("mod",                   2, "I,I=I",    TRUE,  Pl_Fct_Mod);
  ADD_EVALUABLE("/\\",                   2, "I,I=I",    TRUE,  Pl_Fct_And);
  ADD_EVALUABLE("\\/",                   2, "I,I=I",    TRUE,  Pl_Fct_Or);
  ADD_EVALUABLE("xor",                   2, "I,I=I",    TRUE,  Pl_Fct_Xor);
  ADD_EVALUABLE("\\",                    1, "I=I",      TRUE,  Pl_Fct_Not);
  ADD_EVALUABLE("<<",                    2, "I,I=I",    TRUE,  Pl_Fct_Shl);
  ADD_EVALUABLE(">>",                    2, "I,I=I",    TRUE,  Pl_Fct_Shr);
  ADD_EVALUABLE("lsb",                   1, "I=I",      FALSE, Pl_Fct_LSB);
  ADD_EVALUABLE("msb",                   1, "I=I",      FALSE, Pl_Fct_MSB);
  ADD_EVALUABLE("popcount",              1, "I=I",      FALSE, Pl_Fct_Popcount);
  ADD_EVALUABLE("abs",                   1, "IF=IF",    TRUE,  Pl_Fct_Abs);
  ADD_EVALUABLE("sign",                  1, "IF=IF",    TRUE,  Pl_Fct_Sign);
  ADD_EVALUABLE("min",                   2, "IF,IF=?",  TRUE,  Pl_Fct_Min);
  ADD_EVALUABLE("max",                   2, "IF,IF=?",  TRUE,  Pl_Fct_Max);
  ADD_EVALUABLE("gcd",                   2, "I,I=I",    FALSE, Pl_Fct_GCD);
  ADD_EVALUABLE("^",                     2, "IF,IF=IF", TRUE,  Pl_Fct_IPow);
  ADD_EVALUABLE("**",                    2, "IF,IF=F",  TRUE,  Pl_Fct_Pow);
  ADD_EVALUABLE("sqrt",                  1, "IF=F",     TRUE,  Pl_Fct_Sqrt);
  ADD_EVALUABLE("tan",                   1, "IF=F",     TRUE,  Pl_Fct_Tan);
  ADD_EVALUABLE("atan",                  1, "IF=F",     TRUE,  Pl_Fct_Atan);
  ADD_EVALUABLE("atan2",                 2, "IF,IF=F",  TRUE,  Pl_Fct_Atan2);
  ADD_EVALUABLE("cos",                   1, "IF=F",     TRUE,  Pl_Fct_Cos);
  ADD_EVALUABLE("acos",                  1, "IF=F",     TRUE,  Pl_Fct_Acos);
  ADD_EVALUABLE("sin",                   1, "IF=F",     TRUE,  Pl_Fct_Sin);
  ADD_EVALUABLE("asin",                  1, "IF=F",     TRUE,  Pl_Fct_Asin);
  ADD_EVALUABLE("tanh",                  1, "IF=F",     TRUE,  Pl_Fct_Tanh);
  ADD_EVALUABLE("atanh",                 1, "IF=F",     FALSE, Pl_Fct_Atanh);
  ADD_EVALUABLE("cosh",                  1, "IF=F",     FALSE, Pl_Fct_Cosh);
  ADD_EVALUABLE("acosh",                 1, "IF=F",     FALSE, Pl_Fct_Acosh);
  ADD_EVALUABLE("sinh",                  1, "IF=F",     FALSE, Pl_Fct_Sinh);
  ADD_EVALUABLE("asinh",                 1, "IF=F",     FALSE, Pl_Fct_Asinh);
  ADD_EVALUABLE("exp",                   1, "IF=F",     TRUE,  Pl_Fct_Exp);
  ADD_EVALUABLE("log",                   1, "IF=F",     TRUE,  Pl_Fct_Log);
  ADD_EVALUABLE("log10",                 1, "IF=F",     FALSE, Pl_Fct_Log10);
  ADD_EVALUABLE("log",                   2, "IF,IF=F",  FALSE, Pl_Fct_Log_Radix);
  ADD_EVALUABLE("float",                 1, "IF=F",     TRUE,  Pl_Fct_Float);
  ADD_EVALUABLE("ceiling",               1, "F=I",      TRUE,  Pl_Fct_Ceiling);
  ADD_EVALUABLE("floor",                 1, "F=I",      TRUE,  Pl_Fct_Floor);
  ADD_EVALUABLE("round",                 1, "F=I",      TRUE,  Pl_Fct_Round);
  ADD_EVALUABLE("truncate",              1, "F=I",      TRUE,  Pl_Fct_Truncate);
  ADD_EVALUABLE("float_fractional_part", 1, "F=F",      TRUE,  Pl_Fct_Float_Fract_Part);
  ADD_EVALUABLE("float_integer_part",    1, "F=F",      TRUE,  Pl_Fct_Float_Integer_Part);
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
  cur_bip_arity = (int) Pl_Rd_Integer_Check(arity_word);
  Pl_Set_C_Bip_Name(cur_bip_func, cur_bip_arity);
}




/*-------------------------------------------------------------------------*
 * PL_CLASSIFY_DOUBLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Classify_Double(double x)
{
#ifdef fpclassify
  switch(fpclassify(x))
    {
    case FP_NAN:
      return PL_FP_NAN;
    case FP_INFINITE:
      return PL_FP_INFINITE;
    case FP_SUBNORMAL:
      return PL_FP_SUBNORMAL;
    }
#elif defined(HAVE_FPCLASS)
  switch(fpclass(x))
    {
    case FP_SNAN:
    case FP_QNAN:
      return PL_FP_NAN;
    case FP_NINF:
    case FP_PINF:
      return PL_FP_INFINITE;
    case FP_NDENORM:
    case FP_PDENORM:
      return PL_FP_SUBNORMAL;
    }
#elif defined(HAVE__FPCLASS)
  switch(_fpclass(x))
    {
    case _FPCLASS_SNAN:
    case _FPCLASS_QNAN:
      return PL_FP_NAN;
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      return PL_FP_INFINITE;
    case _FPCLASS_ND:
    case _FPCLASS_PD:
      return PL_FP_SUBNORMAL;
    }
#else
  if (isnan(x))
    return PL_FP_NAN;
  if (isinf(x))
    return PL_FP_INFINITE;
#endif
  return PL_FP_NORMAL;
}




/*-------------------------------------------------------------------------*
 * CHECK_DOUBLE_ERRORS                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Check_Double_Errors(double x, Bool for_int)
{
  switch(Pl_Classify_Double(x))
    {
    case PL_FP_NAN:
      Pl_Err_Evaluation(pl_evaluation_undefined);

    case PL_FP_INFINITE:
      if (for_int)
	Pl_Err_Evaluation(pl_evaluation_int_overflow);
      else
	Pl_Err_Evaluation(pl_evaluation_float_overflow);

#if 0  /* ignored for the moment, maybe add prolog_flags to control this, see swi */
    case PL_FP_SUBNORMAL:
      Pl_Err_Evaluation(pl_evaluation_underflow);
#endif
    }
}




/*-------------------------------------------------------------------------*
 * PL_NAN                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Pl_NaN(void)
{
#ifdef NAN
  return NAN;
#else
  return 0.0 / 0.0;
#endif
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
  WamWord x_word;

  Check_Double_Errors(d, FALSE);

  x_word = Tag_FLT(H);

  Pl_Global_Push_Float(d);

  return x_word;
}




/*-------------------------------------------------------------------------*
 * TO_DOUBLE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static double
To_Double(WamWord x_word)
{
  return (Tag_Is_INT(x_word)) ? (double) (UnTag_INT(x_word)) :
    Pl_Obtain_Float(UnTag_FLT(x_word));
}




/*-------------------------------------------------------------------------*
 * LOAD_MATH_EXPRESSION                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Load_Math_Expression(WamWord exp_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *lst_adr;
  WamWord func, arity;
  EvaluableInf *evaluable;

  DEREF(exp_word, word, tag_mask);

  if (tag_mask == TAG_INT_MASK || tag_mask == TAG_FLT_MASK)
    return word;

  func = arity = 0;		/* init for the compiler */

  if (tag_mask == TAG_STC_MASK)	/* most common case first for efficiency */
    {
      adr = UnTag_STC(word);
      func = Functor(adr);
      arity = Arity(adr);
    }
  else if (tag_mask == TAG_ATM_MASK)
    {
      func = UnTag_ATM(word);
      arity = 0;
    }
  else if (tag_mask == TAG_LST_MASK)
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
  else if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();
  else
    Pl_Err_Type(pl_type_evaluable, word);

  /* here it is an evaluable (callable) func/arity */

  evaluable = Pl_Lookup_Evaluable(func, arity);
  if (evaluable == NULL)
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Type(pl_type_evaluable, word);
    }

#define CALL_MATH_FCT_0(x)    (* (WamWord (*)())                 (evaluable->fct)) ()
#define CALL_MATH_FCT_1(x)    (* (WamWord (*)(WamWord))          (evaluable->fct)) (x)
#define CALL_MATH_FCT_2(x, y) (* (WamWord (*)(WamWord, WamWord)) (evaluable->fct)) (x, y)

  if (arity == 2)
    return CALL_MATH_FCT_2(Load_Math_Expression(Arg(adr, 0)), Load_Math_Expression(Arg(adr, 1)));

  if (arity == 1)
    return CALL_MATH_FCT_1(Load_Math_Expression(Arg(adr, 0)));

  return CALL_MATH_FCT_0(Load_Math_Expression());
}




/*-------------------------------------------------------------------------*
 * PL_ARITH_EVALUATE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Arith_Evaluate_2(WamWord x_word, WamWord exp_word)
{
  return Pl_Unify(x_word, Load_Math_Expression(exp_word));
}


/*-------------------------------------------------------------------------*
 * PL_SUCC_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Succ_2(WamWord x_word, WamWord y_word)
{
  WamWord word, tag_mask;
  PlLong x, y;

  DEREF(x_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      x = Pl_Rd_Positive_Check(x_word);
      
      DEREF(y_word, word, tag_mask);
      if (tag_mask == TAG_INT_MASK)
	{
	  y = UnTag_INT(y_word);
	  if (y < 0)
	    Pl_Err_Domain(pl_domain_not_less_than_zero, y_word);
	}
      else if (tag_mask == TAG_REF_MASK)
	{
	  if (x == INT_GREATEST_VALUE)
	    Pl_Err_Evaluation(pl_evaluation_int_overflow); /* only if y is a var (else fail) */
	}
      else
	Pl_Err_Type(pl_type_integer, y_word);

      return Pl_Get_Integer(x + 1, y_word);
    }

  Pl_Check_For_Un_Positive(word);
  x = Pl_Rd_Positive_Check(y_word) - 1;
  return x >= 0 && Pl_Get_Integer(x, word);
}




	  /* Mathematic Operations */

	/* --- Helper functions for int_overflow detection --- */

/* Similar to __builtin_xxx_overflow:
 * signature: operands, pointer to result -> Bool 
 * return true iff overflow
 *
 * Tests overflow wrt. PlLong (PlLong could be replaced by int64_t).
 * It accepts ALL representable integers in the range of a PlLong.
 * So, it DOES NOT test if the PlLong is a representable Prolog integer.
 */
static inline Bool
mul_overflow(PlLong a, PlLong b, PlLong *r)
{
  PlLong res = a * b;
  *r = res;
    
  return (a != 0 && res / a != b);/* this also detects the case INT64_MIN * -1 */
}

/* Similar to mul_overflow() but for integer power
 * Returns an int: 
 *     0 = OK, 1 = overflow, 
 *     2 = undefined (division by 0)
 *     3 = not an integer (result would be OK as a float)
 */
static inline int
pow_overflow(PlLong a, PlLong b, PlLong *r)
{
  if (a == 1)
    {
      *r = 1;
      return 0;
    }
        
  if (a == -1)
    {
      *r = 1 - ((b & 1) << 1); /* 1 if b is even, -1 else */
      return 0;
    }

  if (a == 0)
    {
      if (b >= 0)
	{
	  *r = (b == 0);
	  return 0;
	}
      return 2;		/* undefined if div by 0, e.g. 0^-3 */
    }

  /* here abs(a) > 1 */
  if (b < 0)
    return 3;	    /* not an integer (result would be a float) */

  /* here 0 < b. b sould be < 64, else overflow occurs even on 64 bits, 
   * this could exploited to unroll the loop 6 times (b has at most 6 bits)
   */
  *r = 1;
  for(;;)			/* inifinte loop: don't test b twice */
    {
      if ((b & 1) && mul_overflow(*r, a, r)) /* r = r * a */
	return 1;
      b >>= 1;
      if (!b)
	break;
      if (mul_overflow(a, a, &a))
	return 1;
    }

  return 0;
}

/* On 64bits, some PlLong are not exactly represented as double.
 * Comparing a PlLong with a double (used by min/2, max/2) requires
 * more work than a simple cast to double 
 */

#define CMP_SAME_TYPE(a, b) ((a) < (b) ? -1 : (a) != (b))

static inline int
compare_with_double(PlLong a, double b)
{
#if WORD_SIZE == 64 && defined(HAS_AN_EFFECTIVE_LONG_DOUBLE)

  long double lda = (long double) a;
  long double ldb = (long double) b;
  return CMP_SAME_TYPE(lda, ldb);
  
#elif WORD_SIZE == 64

  if (isnan(b) || isinf(b))
    return FALSE;

  if (b < -0x1.0p63)		/* b is less than any PlLong */
    return 1;

  if (b >= 0x1.0p63)		/* b is greated than any PlLong */
    return -1;

  if (b < -MAX_INT_OK_IN_DBL || b > MAX_INT_OK_IN_DBL) 	/* |b| >= largest representable */
    {
      PlLong lb = (PlLong) b;
      return CMP_SAME_TYPE(a, lb);
    }

#endif

  double da = (double) a;
  return CMP_SAME_TYPE(da, b);
}


	/* --- End helper function for int_overflow detection --- */

#define C_Neg(x)     (- (x))

#define C_Add(x, y)  ((x) + (y))

#define C_Sub(x, y)  ((x) - (y))

#define C_Mul(x, y)  ((x) * (y))

#define C_Div(x, y)  ((y) != 0 ? (x) / (y) : (Pl_Err_Evaluation(pl_evaluation_zero_divisor), 0))

#define Identity(x)  (x)

#define DInc(x)      ((x) + 1)

#define DDec(x)      ((x) - 1)

#define DSign(x)     ((x) < 0.0 ? -1.0 : (x) > 0.0 ? 1.0 : 0.0)

#define DInteg(x)    (((x) > 0) ? floor(x) : ceil(x))

#define DFract(x)    ((x) - DInteg(x))

#define Log_Radix(b, x)  (log(x) / log(b))


#define X_and_Y_are_INT(x_word, y_word)  Tag_Is_INT(x_word & y_word)


#define IFxIFtoIF(x_word, y_word, c_op, fast_op)                     \
  return (X_and_Y_are_INT(x_word, y_word))                           \
    ? fast_op(x_word, y_word)                                        \
    : Make_Tagged_Float(c_op(To_Double(x_word), To_Double(y_word)))



#define IFxIFtoF(x_word, y_word, c_op)                                \
  return Make_Tagged_Float(c_op(To_Double(x_word), To_Double(y_word)))



#define IxItoI(x_word, y_word, fast_op)				\
  if (Tag_Is_FLT(x_word))		/* error case */	\
    Pl_Err_Type(pl_type_integer, x_word);			\
  if (Tag_Is_FLT(y_word))		/* error case */	\
    Pl_Err_Type(pl_type_integer, y_word);			\
  return fast_op(x_word, y_word)



#define IFtoIF(x_word, c_op, fast_op)			\
  return (Tag_Is_INT(x_word)) ? fast_op(x_word) :	\
    Make_Tagged_Float(c_op(To_Double(x_word)))



#define ItoI(x_word, fast_op)                         \
  if (Tag_Is_FLT(x_word))            /* error case */ \
    Pl_Err_Type(pl_type_integer, x_word);             \
  return fast_op(x_word)



#define IFtoF(x_word, c_op)                            \
  return Make_Tagged_Float(c_op(To_Double(x_word)))



       /* FtoI is ONLY used for rounding evaluables */
#define FtoI(x_word, c_op)				\
  double d;						\
  if (Tag_Is_INT(x_word))            /* error case */	\
    {							\
      if (Flag_Value(strict_iso))			\
         Pl_Err_Type(pl_type_float, x_word);		\
							\
      return x_word;					\
    }							\
  else							\
    d = Pl_Obtain_Float(UnTag_FLT(x_word));		\
  return Tag_INT((PlLong) c_op(d))



#define FtoF(x_word, c_op)				\
  double d;						\
  if (Tag_Is_INT(x_word))            /* error case */	\
    {							\
      Pl_Err_Type(pl_type_float, x_word);		\
							\
      return x_word; /* for clang (avoid d uninit) */	\
    }							\
  else							\
    d = Pl_Obtain_Float(UnTag_FLT(x_word));		\
  return Make_Tagged_Float(c_op(d))



	  /* fast-math version */

WamWord FC
Pl_Fct_Fast_Neg(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  if (x == INT_LOWEST_VALUE)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(-x);
}

WamWord FC
Pl_Fct_Fast_Inc(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  if (x == INT_GREATEST_VALUE)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(x + 1);
}

WamWord FC
Pl_Fct_Fast_Dec(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  if (x == INT_LOWEST_VALUE)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(x - 1);
}

WamWord FC
Pl_Fct_Fast_Add(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z = x + y;
  if (TEST_INT_OVERFLOW(z))
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(z);
}

WamWord FC
Pl_Fct_Fast_Sub(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z = x - y;
  if (TEST_INT_OVERFLOW(z))
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(z);
}

WamWord FC
Pl_Fct_Fast_Mul(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z;
  if (mul_overflow(x, y, &z) || TEST_INT_OVERFLOW(z))
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return Tag_INT(z);
}

WamWord FC
Pl_Fct_Fast_Integer_Div(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);

  if (y == 0)
    Pl_Err_Evaluation(pl_evaluation_zero_divisor);

  if (x == INT_LOWEST_VALUE && y == -1)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);

  return Tag_INT(x / y);
}

WamWord FC
Pl_Fct_Fast_Integer_Div2(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong m;

  if (y == 0)
    Pl_Err_Evaluation(pl_evaluation_zero_divisor);

  if (x == INT_LOWEST_VALUE && y == -1)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);

  m = x % y;

  if (m != 0 && (m ^ y) < 0)	/* have m and y different signs ? */
    m += y;

  m = (x - m) / y;

  return Tag_INT(m);
}

WamWord FC
Pl_Fct_Fast_Rem(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);

  if (y == 0)
    Pl_Err_Evaluation(pl_evaluation_zero_divisor);

  return Tag_INT(x % y);
}

WamWord FC
Pl_Fct_Fast_Mod(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong m;

  if (y == 0)
    Pl_Err_Evaluation(pl_evaluation_zero_divisor);

  m = x % y;

  if (m != 0 && (m ^ y) < 0)	/* have m and y different signs ? */
    m += y;

  return Tag_INT(m);
}

WamWord FC
Pl_Fct_Fast_And(WamWord x_word, WamWord y_word)
{
  return x_word & y_word;
}

WamWord FC
Pl_Fct_Fast_Or(WamWord x_word, WamWord y_word)
{
  return x_word | y_word;
}

WamWord FC
Pl_Fct_Fast_Xor(WamWord x_word, WamWord y_word)
{
  return (x_word ^ y_word) | TAG_INT_MASK;
}

WamWord FC
Pl_Fct_Fast_Not(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  return Tag_INT(~x);
}


WamWord FC
Pl_Fct_Fast_Shl(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z;

  if (y >= WORD_SIZE)
    {
      if (x)
	Pl_Err_Evaluation(pl_evaluation_int_overflow);
      z = 0;
    }
  else
    {
      if (x > (INT_GREATEST_VALUE >> y))
	Pl_Err_Evaluation(pl_evaluation_int_overflow);
      z = x << y;
    }

  return Tag_INT(z);
}

WamWord FC
Pl_Fct_Fast_Shr(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z;

  z = (y >= WORD_SIZE) ? 0 : (x >> y);

  return Tag_INT(z);
}

WamWord FC
Pl_Fct_Fast_LSB(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  return Tag_INT((x == 0) ? -1 : Pl_Least_Significant_Bit(x));
}

WamWord FC
Pl_Fct_Fast_MSB(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  return Tag_INT((x == 0) ? -1 : Pl_Most_Significant_Bit(x));
}

WamWord FC
Pl_Fct_Fast_Popcount(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  return Tag_INT(Pl_Count_Set_Bits(x));
}

WamWord FC
Pl_Fct_Fast_Abs(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  if (x == INT_LOWEST_VALUE)
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  return (x < 0) ? Tag_INT(-x) : x_word;
}

WamWord FC
Pl_Fct_Fast_Sign(WamWord x_word)
{
  PlLong x = UnTag_INT(x_word);
  return (x < 0) ? Tag_INT(-1) : (x == 0) ? Tag_INT(0) : Tag_INT(1);
}

WamWord FC
Pl_Fct_Fast_GCD(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);

  if ((x == INT_LOWEST_VALUE && y == 0) ||
      (x == 0 && y == INT_LOWEST_VALUE))
    Pl_Err_Evaluation(pl_evaluation_int_overflow);

  if (x < 0)
    x = -x;

  if (y < 0)
    y = -y;

  while(y != 0)
    {
      PlLong r = x % y;
      x = y;
      y = r;
    }
  return Tag_INT(x);
}



/* see ISO TC#2-9.3.10.3 (corr. TC#3)- special error cases for integer power (^)/2 */
WamWord FC
Pl_Fct_Fast_IPow(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  PlLong z;
  int s = pow_overflow(x, y, &z);

  if (s == 2)
    Pl_Err_Evaluation(pl_evaluation_undefined);

  if (s == 3)
    Pl_Err_Type(pl_type_float, x_word);

  if (s || TEST_INT_OVERFLOW(z))
    Pl_Err_Evaluation(pl_evaluation_int_overflow);
  
  return Tag_INT(z);
}



	  /* standard version */

WamWord FC
Pl_Fct_Neg(WamWord x_word)
{
  IFtoIF(x_word, C_Neg, Pl_Fct_Fast_Neg);
}

WamWord FC
Pl_Fct_Inc(WamWord x_word)
{
  IFtoIF(x_word, DInc, Pl_Fct_Fast_Inc);
}

WamWord FC
Pl_Fct_Dec(WamWord x_word)
{
  IFtoIF(x_word, DDec, Pl_Fct_Fast_Dec);
}

WamWord FC
Pl_Fct_Add(WamWord x_word, WamWord y_word)
{
  IFxIFtoIF(x_word, y_word, C_Add, Pl_Fct_Fast_Add);
}

WamWord FC
Pl_Fct_Sub(WamWord x_word, WamWord y_word)
{
  IFxIFtoIF(x_word, y_word, C_Sub, Pl_Fct_Fast_Sub);
}

WamWord FC
Pl_Fct_Mul(WamWord x_word, WamWord y_word)
{
  IFxIFtoIF(x_word, y_word, C_Mul, Pl_Fct_Fast_Mul);
}

WamWord FC
Pl_Fct_Float_Div(WamWord x_word, WamWord y_word)
{
  IFxIFtoF(x_word, y_word, C_Div);
}

WamWord FC
Pl_Fct_Integer_Div(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Integer_Div);
}

WamWord FC
Pl_Fct_Integer_Div2(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Integer_Div2);
}


WamWord FC
Pl_Fct_Rem(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Rem);
}

WamWord FC
Pl_Fct_Mod(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Mod);
}
WamWord FC
Pl_Fct_And(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_And);
}

WamWord FC
Pl_Fct_Or(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Or);
}

WamWord FC
Pl_Fct_Xor(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Xor);
}

WamWord FC
Pl_Fct_Not(WamWord x_word)
{
  ItoI(x_word, Pl_Fct_Fast_Not);
}

WamWord FC
Pl_Fct_Shl(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Shl);
}

WamWord FC
Pl_Fct_Shr(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_Shr);
}

WamWord FC
Pl_Fct_LSB(WamWord x_word)
{
  ItoI(x_word, Pl_Fct_Fast_LSB);
}

WamWord FC
Pl_Fct_MSB(WamWord x_word)
{
  ItoI(x_word, Pl_Fct_Fast_MSB);
}

WamWord FC
Pl_Fct_Popcount(WamWord x_word)
{
  ItoI(x_word, Pl_Fct_Fast_Popcount);
}

WamWord FC
Pl_Fct_Abs(WamWord x_word)
{
  IFtoIF(x_word, fabs, Pl_Fct_Fast_Abs);
}

WamWord FC
Pl_Fct_Sign(WamWord x_word)
{
  IFtoIF(x_word, DSign, Pl_Fct_Fast_Sign);
}

/* issue #48: large 64 bits integers cannot be represented 
 * exactly as a double without a loss.
 * Do no convert long to double for min/2 or max/2.
 * use special function compare_with_double
 */



static int
Compare_Numbers(WamWord x_word, WamWord y_word)
{
  Bool x_is_int, y_is_int;
  PlLong x_int, y_int;
  double x_flt, y_flt;
  
  if ((x_is_int = Tag_Is_INT(x_word)))
    x_int = UnTag_INT(x_word);
  else
    x_flt = Pl_Obtain_Float(UnTag_FLT(x_word));
  
  if ((y_is_int = Tag_Is_INT(y_word)))
    y_int = UnTag_INT(y_word);
  else
    y_flt = Pl_Obtain_Float(UnTag_FLT(y_word));

  if (x_is_int)
    {
      if (y_is_int)
	return CMP_SAME_TYPE(x_int, y_int);
      else
	return compare_with_double(x_int, y_flt);
    }
  else if (y_is_int)
    return -compare_with_double(y_int, x_flt);
  else
    return CMP_SAME_TYPE(x_flt, y_flt);
}

WamWord FC
Pl_Fct_Min(WamWord x_word, WamWord y_word)
{
  int cmp = Compare_Numbers(x_word, y_word);

  if (cmp < 0)
    return x_word;

  if (cmp > 0)
    return y_word;

  return Tag_Is_FLT(x_word) ? x_word : y_word;
}

WamWord FC
Pl_Fct_Max(WamWord x_word, WamWord y_word)
{
  int cmp = Compare_Numbers(x_word, y_word);

  if (cmp > 0)
    return x_word;

  if (cmp < 0)
    return y_word;

  return Tag_Is_FLT(x_word) ? x_word : y_word;
}

WamWord FC
Pl_Fct_GCD(WamWord x_word, WamWord y_word)
{
  IxItoI(x_word, y_word, Pl_Fct_Fast_GCD);
}


/* see ISO TC#2-9.3.10.3 (corr. TC#3)- special error cases for integer power (^)/2 */
static inline double
IPow(double x, double y)
{
  if (x == 0.0 && y < 0)
    return Pl_NaN();

  if (x == 1.0)
    return x;

  return pow(x, y);
}


WamWord FC
Pl_Fct_IPow(WamWord x_word, WamWord y_word)
{
  IFxIFtoIF(x_word, y_word, IPow, Pl_Fct_Fast_IPow);
}


WamWord FC
Pl_Fct_Pow(WamWord x_word, WamWord y_word)
{
  IFxIFtoF(x_word, y_word, pow);
}

WamWord FC
Pl_Fct_Sqrt(WamWord x_word)
{
  IFtoF(x_word, sqrt);
}

WamWord FC
Pl_Fct_Tan(WamWord x_word)
{
  IFtoF(x_word, tan);
}

WamWord FC
Pl_Fct_Atan(WamWord x_word)
{
  IFtoF(x_word, atan);
}

WamWord FC
Pl_Fct_Atan2(WamWord x_word, WamWord y_word)
{
  IFxIFtoF(x_word, y_word, atan2);
}

WamWord FC
Pl_Fct_Cos(WamWord x_word)
{
  IFtoF(x_word, cos);
}

WamWord FC
Pl_Fct_Acos(WamWord x_word)
{
  IFtoF(x_word, acos);
}

WamWord FC
Pl_Fct_Sin(WamWord x_word)
{
  IFtoF(x_word, sin);
}

WamWord FC
Pl_Fct_Asin(WamWord x_word)
{
  IFtoF(x_word, asin);
}

WamWord FC
Pl_Fct_Tanh(WamWord x_word)
{
  IFtoF(x_word, tanh);
}

WamWord FC
Pl_Fct_Atanh(WamWord x_word)
{
#ifdef HAVE_ATANH
  IFtoF(x_word, atanh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable evaluable"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Cosh(WamWord x_word)
{
  IFtoF(x_word, cosh);
}

WamWord FC
Pl_Fct_Acosh(WamWord x_word)
{
#ifdef HAVE_ACOSH
  IFtoF(x_word, acosh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable evaluable"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Sinh(WamWord x_word)
{
  IFtoF(x_word, sinh);
}

WamWord FC
Pl_Fct_Asinh(WamWord x_word)
{
#ifdef HAVE_ASINH
  IFtoF(x_word, asinh);
#else
  Pl_Err_Resource(Pl_Create_Atom("unavailable evaluable"));
  return 0;			/* anything for the compiler */
#endif
}

WamWord FC
Pl_Fct_Exp(WamWord x_word)
{
  IFtoF(x_word, exp);
}

WamWord FC
Pl_Fct_Log(WamWord x_word)
{
  IFtoF(x_word, log);
}

WamWord FC
Pl_Fct_Log10(WamWord x_word)
{
  IFtoF(x_word, log10);
}

WamWord FC
Pl_Fct_Log_Radix(WamWord b_word, WamWord x_word)
{
  IFxIFtoF(b_word, x_word, Log_Radix);
}

WamWord FC
Pl_Fct_Float(WamWord x_word)
{
  IFtoF(x_word, Identity);
}

WamWord FC
Pl_Fct_Ceiling(WamWord x_word)
{
  FtoI(x_word, ceil);
}

WamWord FC
Pl_Fct_Floor(WamWord x_word)
{
  FtoI(x_word, floor);
}

WamWord FC
Pl_Fct_Round(WamWord x_word)
{
  FtoI(x_word, pl_round);
}

WamWord FC
Pl_Fct_Truncate(WamWord x_word)
{
  FtoI(x_word, Identity);
}

WamWord FC
Pl_Fct_Float_Fract_Part(WamWord x_word)
{
  FtoF(x_word, DFract);
}

WamWord FC
Pl_Fct_Float_Integer_Part(WamWord x_word)
{
  FtoF(x_word, DInteg);
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
Pl_Fct_Identity(WamWord x_word)
{
  return x_word;
}				/* for meta-call */





	  /* Mathematic Comparisons */

#define Cmp_IFxIF(x_word, y_word, c_op, fast_op)     \
  return (X_and_Y_are_INT(x_word, y_word))           \
    ? fast_op(x_word, y_word)                        \
    : (To_Double(x_word) c_op To_Double(y_word))


	  /* fast-math version */

Bool FC
Pl_Blt_Fast_Eq(WamWord x_word, WamWord y_word)
{
  return x_word == y_word;
}

Bool FC
Pl_Blt_Fast_Neq(WamWord x_word, WamWord y_word)
{
  return x_word != y_word;
}

Bool FC
Pl_Blt_Fast_Lt(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  return x < y;
}

Bool FC
Pl_Blt_Fast_Lte(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  return x <= y;
}

Bool FC
Pl_Blt_Fast_Gt(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  return x > y;
}

Bool FC
Pl_Blt_Fast_Gte(WamWord x_word, WamWord y_word)
{
  PlLong x = UnTag_INT(x_word);
  PlLong y = UnTag_INT(y_word);
  return x >= y;
}


	  /* standard version */

Bool FC
Pl_Blt_Eq(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, ==, Pl_Blt_Fast_Eq);
}
Bool FC
Pl_Blt_Neq(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, !=, Pl_Blt_Fast_Neq);
}
Bool FC
Pl_Blt_Lt(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, <, Pl_Blt_Fast_Lt);
}
Bool FC
Pl_Blt_Lte(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, <=, Pl_Blt_Fast_Lte);
}
Bool FC
Pl_Blt_Gt(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, >, Pl_Blt_Fast_Gt);
}
Bool FC
Pl_Blt_Gte(WamWord x_word, WamWord y_word)
{
  Cmp_IFxIF(x_word, y_word, >=, Pl_Blt_Fast_Gte);
}




#define CURRENT_EVALUABLE_ALT   X1_2463757272656E745F6576616C7561626C655F616C74

Prolog_Prototype(CURRENT_EVALUABLE_ALT, 0);




/*-------------------------------------------------------------------------*
 * PL_CURRENT_EVALUABLE_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Evaluable_1(WamWord func_indic_word)
{
  WamWord name_word, arity_word;
  HashScan scan;
  EvaluableInf *evaluable;
  int func, arity;
  int func1, arity1;
  Bool all;

  func = Pl_Get_Pred_Indicator(func_indic_word, FALSE, &arity);
  name_word = pl_pi_name_word;
  arity_word = pl_pi_arity_word;

  if (func >= 0 && arity >= 0)
    {
      evaluable = Pl_Lookup_Evaluable(func, arity);
      return evaluable != NULL;
    }

				/* here func or arity == -1 (or both) */
  all = (func == -1 && arity == -1);

  evaluable = (EvaluableInf *) Pl_Hash_First(evaluable_tbl, &scan);
  for (;;)
    {
      if (evaluable == NULL)
	return FALSE;

      func1 = Functor_Of(evaluable->f_n);
      arity1 = Arity_Of(evaluable->f_n);

      if (all || func == func1 || arity == arity1)
	break;

      evaluable = (EvaluableInf *) Pl_Hash_Next(&scan);
    }

				/* non deterministic case */
  A(0) = name_word;
  A(1) = arity_word;
  A(2) = (WamWord) scan.endt;
  A(3) = (WamWord) scan.cur_t;
  A(4) = (WamWord) scan.cur_p;
  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_EVALUABLE_ALT, 0), 5);

  return Pl_Get_Atom(Functor_Of(evaluable->f_n), name_word) &&
    Pl_Get_Integer(Arity_Of(evaluable->f_n), arity_word);
  /*
  return Pl_Un_Atom_Check(Functor_Of(evaluable->f_n), name_word) &&
    Pl_Un_Integer_Check(Arity_Of(evaluable->f_n), arity_word);
  */
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_EVALUABLE_ALT_0                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Evaluable_Alt_0(void)
{
  WamWord name_word, arity_word;
  HashScan scan;
  EvaluableInf *evaluable;
  int func, arity;
  int func1, arity1;
  Bool all;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_EVALUABLE_ALT, 0), 0);

  name_word = AB(B, 0);
  arity_word = AB(B, 1);
  scan.endt = (char *) AB(B, 2);
  scan.cur_t = (char *) AB(B, 3);
  scan.cur_p = (char *) AB(B, 4);

  func = Tag_Mask_Of(name_word) == TAG_REF_MASK ? -1 : UnTag_ATM(name_word);
  arity = Tag_Mask_Of(arity_word) == TAG_REF_MASK ? -1 : (int) UnTag_INT(arity_word);

				/* here func or arity == -1 (or both) */
  all = (func == -1 && arity == -1);

  for (;;)
    {
      evaluable = (EvaluableInf *) Pl_Hash_Next(&scan);
      if (evaluable == NULL)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}

      func1 = Functor_Of(evaluable->f_n);
      arity1 = Arity_Of(evaluable->f_n);

      if (all || func == func1 || arity == arity1)
	break;
    }

				/* non deterministic case */

#if 0				/* the following data is unchanged */
  AB(B, 0) = name_word;
  AB(B, 1) = arity_word;
  AB(B, 2) = (WamWord) scan.endt;
#endif
  AB(B, 3) = (WamWord) scan.cur_t;
  AB(B, 4) = (WamWord) scan.cur_p;

  return Pl_Get_Atom(Functor_Of(evaluable->f_n), name_word) &&
    Pl_Get_Integer(Arity_Of(evaluable->f_n), arity_word);
}




/*-------------------------------------------------------------------------*
 * EVALUABLE_PROP_BUILT_IN_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Evaluable_Prop_Built_In_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_C_Int(arity_word);
  EvaluableInf *evaluable = Pl_Lookup_Evaluable(func, arity);

  return evaluable != NULL;
}




/*-------------------------------------------------------------------------*
 * EVALUABLE_PROP_STATIC_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Evaluable_Prop_Static_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_C_Int(arity_word);
  EvaluableInf *evaluable = Pl_Lookup_Evaluable(func, arity);

  return evaluable != NULL;
}




/*-------------------------------------------------------------------------*
 * EVALUABLE_PROP_ISO_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Evaluable_Prop_ISO_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_C_Int(arity_word);
  EvaluableInf *evaluable = Pl_Lookup_Evaluable(func, arity);

  return evaluable != NULL && evaluable->is_iso;
}




/*-------------------------------------------------------------------------*
 * EVALUABLE_PROP_SIGNATURE_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Evaluable_Prop_Signature_3(WamWord func_word, WamWord arity_word,
			      WamWord signature_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_C_Int(arity_word);
  EvaluableInf *evaluable = Pl_Lookup_Evaluable(func, arity);

  return evaluable != NULL && Pl_Get_Atom(evaluable->signat_atom, signature_word);
}
