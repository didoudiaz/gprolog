/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : arith_inl_c.c                                                   *
 * Descr.: arithmetic (inline) management - C part                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define OBJ_INIT Arith_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#ifdef M_ix86_win32
#define rint(x)  (floor((x)+(double) 0.5))
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
  WamWord (*fct) ();
}
ArithInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char *arith_tbl;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Error_Zero_Divisor(void);

static WamWord Tagged_Mod(WamWord x, WamWord y);

static WamWord Make_Tagged_Float(double d);

static double To_Double(WamWord x);

static WamWord Load_Math_Expression(WamWord exp);



	  /* Mathematic Operations */

#define C_Neg(x)     (- (x))

#define C_Add(x,y)   ((x) + (y))

#define C_Sub(x,y)   ((x) - (y))

#define C_Mul(x,y)   ((x) * (y))

#define C_Div(x,y)   ((y)!=0 ? (x) / (y) : Error_Zero_Divisor())

#define Identity(x)  (x)

#define DInc(x)      ((x)+1)

#define DDec(x)      ((x)-1)

#define DSign(x)     ((x) < 0.0 ? -1.0 : ((x) > 0.0 ? 1.0 : 0.0))

#define DInteg(x)    ((double) ((long) (x)))

#define DFract(x)    ((double) ((x) - (long) (x)))



#define IFxIFtoIF(x,y,c_op,int0_op)                                         \
 return (Tag_Of(x)==INT && Tag_Of(y)==INT)                                  \
                ? int0_op(x,y)                                              \
                : Make_Tagged_Float(c_op(To_Double(x),To_Double(y)));



#define IFxIFtoF(x,y,c_op)                                                  \
 return Make_Tagged_Float(c_op(To_Double(x),To_Double(y)));



#define IxItoI(x,y,int0_op)                                                 \
 if (Tag_Of(x)!=INT)                                       /* error case */ \
     Pl_Err_Type(type_integer,x);                                           \
 if (Tag_Of(y)!=INT)                                       /* error case */ \
     Pl_Err_Type(type_integer,y);                                           \
 return int0_op(x,y);



#define IFtoIF(x,c_op,int0_op)                                              \
 return (Tag_Of(x)==INT) ? int0_op(x)                                       \
                         : Make_Tagged_Float(c_op(To_Double(x)));



#define ItoI(x,int0_op)                                                     \
 if (Tag_Of(x)!=INT)                                       /* error case */ \
     Pl_Err_Type(type_integer,x);                                           \
 return int0_op(x);



#define IFtoF(x,c_op)                                                       \
 return Make_Tagged_Float(c_op(To_Double(x)));



#define FtoI(x,c_op)                                                        \
 double d;                                                                  \
 if (Tag_Of(x)==INT)                                       /* error case */ \
     Pl_Err_Type(type_float,x);                                             \
  else                                                                      \
     d=Obtain_Float(UnTag_FLT(x));                                          \
 return Tag_Value(INT,(long) c_op(d));



#define FtoF(x,c_op)                                                        \
 double d;                                                                  \
 if (Tag_Of(x)==INT)                                       /* error case */ \
     Pl_Err_Type(type_float,x);                                             \
  else                                                                      \
     d=Obtain_Float(UnTag_FLT(x));                                          \
 return Make_Tagged_Float(c_op(d));



#define Tag0_Neg(x)     (-x)

#define Tag0_Inc(x)     (x+Tag_Value(INT,1))

#define Tag0_Dec(x)     (x-Tag_Value(INT,1))

#define Tag0_Add(x,y)   (x+y)

#define Tag0_Sub(x,y)   (x-y)

#define Tag0_Mul(x,y)   (x*(y>>TAG_SIZE))

#define Tag0_Div(x,y)   (y!=0 ? (x/y)<<TAG_SIZE : Error_Zero_Divisor())

#define Tag0_Rem(x,y)   (y!=0 ? x%y             : Error_Zero_Divisor())

#define Tag0_Mod(x,y)   (y!=0 ? Tagged_Mod(x,y) : Error_Zero_Divisor())

#define Tag0_And(x,y)   (x&y)

#define Tag0_Or(x,y)    (x|y)

#define Tag0_Xor(x,y)   (Tag_Value(INT,UnTag_INT(x)^UnTag_INT(y)))

#define Tag0_Not(x)     ((~x)-((1<<TAG_SIZE)-1))

#define Tag0_Shl(x,y)   (x<<UnTag_INT(y))

#define Tag0_Shr(x,y)   (Tag_Value(INT,UnTag_INT(x)>>UnTag_INT(y)))

#define Tag0_Abs(x)     ((x) < 0 ? -x : x)

#define Tag0_Sign(x)    ((x) < 0 ? Tag_INT(-1) \
                                 : ((x) > 0.0 ? Tag_INT(1) : Tag_INT(0)))



	  /* fast-math version */

WamWord
Fct_Fast_Neg(int x)
{
  return Tag0_Neg(x);
}

WamWord
Fct_Fast_Inc(int x)
{
  return Tag0_Inc(x);
}

WamWord
Fct_Fast_Dec(int x)
{
  return Tag0_Dec(x);
}

WamWord
Fct_Fast_Add(int x, int y)
{
  return Tag0_Add(x, y);
}

WamWord
Fct_Fast_Sub(int x, int y)
{
  return Tag0_Sub(x, y);
}

WamWord
Fct_Fast_Mul(int x, int y)
{
  return Tag0_Mul(x, y);
}

WamWord
Fct_Fast_Div(int x, int y)
{
  return Tag0_Div(x, y);
}

WamWord
Fct_Fast_Rem(int x, int y)
{
  return Tag0_Rem(x, y);
}

WamWord
Fct_Fast_Mod(int x, int y)
{
  return Tag0_Mod(x, y);
}

WamWord
Fct_Fast_And(int x, int y)
{
  return Tag0_And(x, y);
}

WamWord
Fct_Fast_Or(int x, int y)
{
  return Tag0_Or(x, y);
}

WamWord
Fct_Fast_Xor(int x, int y)
{
  return Tag0_Xor(x, y);
}

WamWord
Fct_Fast_Not(int x)
{
  return Tag0_Not(x);
}

WamWord
Fct_Fast_Shl(int x, int y)
{
  return Tag0_Shl(x, y);
}

WamWord
Fct_Fast_Shr(int x, int y)
{
  return Tag0_Shr(x, y);
}

WamWord
Fct_Fast_Abs(int x)
{
  return Tag0_Abs(x);
}

WamWord
Fct_Fast_Sign(int x)
{
  return Tag0_Sign(x);
}



	  /* standard version */

WamWord
Fct_Neg(WamWord x)
{
IFtoIF(x, C_Neg, Tag0_Neg)}
WamWord
Fct_Inc(WamWord x)
{
IFtoIF(x, DInc, Tag0_Inc)}
WamWord
Fct_Dec(WamWord x)
{
IFtoIF(x, DDec, Tag0_Dec)}
WamWord
Fct_Add(WamWord x, WamWord y)
{
IFxIFtoIF(x, y, C_Add, Tag0_Add)}
WamWord
Fct_Sub(WamWord x, WamWord y)
{
IFxIFtoIF(x, y, C_Sub, Tag0_Sub)}
WamWord
Fct_Mul(WamWord x, WamWord y)
{
IFxIFtoIF(x, y, C_Mul, Tag0_Mul)}
WamWord
Fct_Div(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Div)}
WamWord
Fct_Float_Div(WamWord x, WamWord y)
{
IFxIFtoF(x, y, C_Div)}
WamWord
Fct_Rem(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Rem)}
WamWord
Fct_Mod(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Mod)}
WamWord
Fct_And(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_And)}
WamWord
Fct_Or(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Or)}
WamWord
Fct_Xor(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Xor)}
WamWord
Fct_Not(WamWord x)
{
ItoI(x, Tag0_Not)}
WamWord
Fct_Shl(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Shl)}
WamWord
Fct_Shr(WamWord x, WamWord y)
{
IxItoI(x, y, Tag0_Shr)}
WamWord
Fct_Abs(WamWord x)
{
IFtoIF(x, fabs, Tag0_Abs)}
WamWord
Fct_Sign(WamWord x)
{
IFtoIF(x, DSign, Tag0_Sign)}
WamWord
Fct_Pow(WamWord x, WamWord y)
{
IFxIFtoF(x, y, pow)}
WamWord
Fct_Sqrt(WamWord x)
{
IFtoF(x, sqrt)}
WamWord
Fct_Atan(WamWord x)
{
IFtoF(x, atan)}
WamWord
Fct_Cos(WamWord x)
{
IFtoF(x, cos)}
WamWord
Fct_Acos(WamWord x)
{
IFtoF(x, acos)}
WamWord
Fct_Sin(WamWord x)
{
IFtoF(x, sin)}
WamWord
Fct_Asin(WamWord x)
{
IFtoF(x, asin)}
WamWord
Fct_Exp(WamWord x)
{
IFtoF(x, exp)}
WamWord
Fct_Log(WamWord x)
{
IFtoF(x, log)}
WamWord
Fct_Float(WamWord x)
{
IFtoF(x, Identity)}
WamWord
Fct_Ceiling(WamWord x)
{
FtoI(x, ceil)}
WamWord
Fct_Floor(WamWord x)
{
FtoI(x, floor)}
WamWord
Fct_Round(WamWord x)
{
FtoI(x, rint)}
WamWord
Fct_Truncate(WamWord x)
{
FtoI(x, Identity)}
WamWord
Fct_Float_Fract_Part(WamWord x)
{
FtoF(x, DFract)}
WamWord
Fct_Float_Integ_Part(WamWord x)
{
FtoF(x, DInteg)}
WamWord
Fct_Identity(WamWord x)
{
  return x;
}				/* for meta-call */



	  /* Mathematic Comparisons */

#define Cmp_IFxIF(x,y,op)                                                   \
 return (Tag_Of(x)==INT && Tag_Of(y)==INT)                                  \
                ? ((long) (x) op (long) (y))                                \
                : (To_Double(x) op To_Double(y));



#define ADD_ARITH_OPER(atom_str,arity,f)                                    \
     arith_info.f_n=Functor_Arity(Create_Atom(atom_str),arity);             \
     arith_info.fct=f;                                                      \
     Hash_Insert(arith_tbl,(char *) &arith_info,FALSE)




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
 * ERROR_ZERO_DIVISOR                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Error_Zero_Divisor(void)
{
  Pl_Err_Evaluation(evluation_zero_divisor);

  return 0;
}




/*-------------------------------------------------------------------------*
 * TAGGED_MOD                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Tagged_Mod(WamWord x, WamWord y)
{
  WamWord m = Tag0_Rem(x, y);

  if ((long) (((long) m ^ (long) y)) < 0)	/* have m and y different signs ? */
    m = Tag0_Add(m, y);

  return m;
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
void
Math_Load_Value(WamWord start_word, WamWord *word_adr)
{
  WamWord word, tag, *adr;

  Deref(start_word, word, tag, adr);

  if (tag != INT && tag != FLT)
    word = Load_Math_Expression(word);

  *word_adr = word;
}




/*-------------------------------------------------------------------------*
 * MATH_FAST_LOAD_VALUE                                                    *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr)
{
  WamWord word, tag, *adr;

  Deref(start_word, word, tag, adr);
  *word_adr = word;
}




	  /* fast-math version */

Bool
Blt_Fast_Eq(WamWord x, WamWord y)
{
  return x == y;
}

Bool
Blt_Fast_Neq(WamWord x, WamWord y)
{
  return x != y;
}

Bool
Blt_Fast_Lt(WamWord x, WamWord y)
{
  return x < y;
}

Bool
Blt_Fast_Lte(WamWord x, WamWord y)
{
  return x <= y;
}

Bool
Blt_Fast_Gt(WamWord x, WamWord y)
{
  return x > y;
}

Bool
Blt_Fast_Gte(WamWord x, WamWord y)
{
  return x >= y;
}


	  /* standard version */

Bool
Blt_Eq(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, ==)}
Bool
Blt_Neq(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, !=)}
Bool
Blt_Lt(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, <)}
Bool
Blt_Lte(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, <=)}
Bool
Blt_Gt(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, >)}
Bool
Blt_Gte(WamWord x, WamWord y)
{
Cmp_IFxIF(x, y, >=)}



/*-------------------------------------------------------------------------*
 * MAKE_TAGGED_FLOAT                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Make_Tagged_Float(double d)
{
  WamWord x = Tag_Value(FLT, H);

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
  return (Tag_Of(x) == INT) ? (double) (UnTag_INT(x)) :
    Obtain_Float(UnTag_FLT(x));
}




/*-------------------------------------------------------------------------*
 * LOAD_MATH_EXPRESSION                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Load_Math_Expression(WamWord exp)
{
  WamWord word, tag, *adr;
  WamWord *lst_adr;
  ArithInf *arith;

  Deref(exp, word, tag, adr);
  switch (tag)
    {
    case REF:
      Pl_Err_Instantiation();
      break;

    case INT:
    case FLT:
      return word;

    case ATM:
      word = Put_Structure(ATOM_CHAR('/'), 2);
      Unify_Value(exp);
      Unify_Integer(0);
      break;			/* then type_error */

    case LST:
      lst_adr = UnTag_LST(word);
      Deref(Cdr(lst_adr), word, tag, adr);
      if (word != NIL_WORD)
	{
	  word = Put_Structure(ATOM_CHAR('/'), 2);
	  Unify_Atom(ATOM_CHAR('.'));
	  Unify_Integer(2);
	  Pl_Err_Type(type_evaluable, word);
	}
      return Load_Math_Expression(Car(lst_adr));

    case STC:
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
      else
	return (*(arith->fct)) (Load_Math_Expression(Arg(adr, 0)),
				Load_Math_Expression(Arg(adr, 1)));
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
