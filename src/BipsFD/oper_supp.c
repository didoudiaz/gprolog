/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : oper_supp.c                                                     *
 * Descr.: FD Operation support                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static unsigned Find_Expon_General(unsigned x, unsigned y, unsigned *pxn);




/*-------------------------------------------------------------------------*
 * POWER                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Power(unsigned x, unsigned n)
{
  unsigned xn, xp;

  if (n == 0 || x == 1)
    return 1;

  if (x == 0)
    return 0;

  if (n >= sizeof(unsigned) * 8)
    return INTERVAL_MAX_INTEGER;

  xn = 1;
  xp = x;

  while (n)
    {
      if (n & 1)
	xn *= xp;
      xp *= xp;
      n >>= 1;
    }

  return ((long) xn > 0 && (long) xn <= INTERVAL_MAX_INTEGER)
    ? xn : INTERVAL_MAX_INTEGER;
}




/*-------------------------------------------------------------------------*
 * NTH_ROOT_DN                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Nth_Root_Dn(unsigned y, unsigned n)
{
  unsigned old, new;
  unsigned n1 = n - 1;
  unsigned oldn1;
  int bit, nb;

  if (y == 0)
    return 0;

  if (n == 0)
    return INTERVAL_MAX_INTEGER;

  if (n >= sizeof(unsigned) * 8)
    return 1;

  bit = Most_Significant_Bit(y);

  if ((unsigned) (bit + 1) < n)
    return 1;

  nb = bit / n;
  new = 1 << nb;

  old = new;
  oldn1 = Power(old, n1);
  new = (n1 * old + y / oldn1) / n;

  do
    {
      old = new;
      oldn1 = Power(old, n1);
      new = (n1 * old + y / oldn1) / n;
    }
  while (new < old);

  return old;
}



/*-------------------------------------------------------------------------*
 * NTH_ROOT_UP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Nth_Root_Up(unsigned y, unsigned n)
{
  unsigned x;

  if (y == 0)
    return 0;

  if (n == 0)
    return 0;

  x = Nth_Root_Dn(y, n);
  if (Power(x, n) != y)
    x++;

  return x;
}




/*-------------------------------------------------------------------------*
 * NTH_ROOT_EXACT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Nth_Root_Exact(unsigned y, unsigned n)
{
  unsigned x;

  if (y == 0)
    return 0;

  x = Nth_Root_Dn(y, n);
  if (Power(x, n) != y)
    return (unsigned) -1;

  return x;
}




/*-------------------------------------------------------------------------*
 * SQRT_DN                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Sqrt_Dn(unsigned y)
{
  unsigned old, new;

  if (y == 0)
    return 0;

  new = y;
  do
    {
      old = new;
      new = (old + y / old) >> 1;
    }
  while (new < old);

  return old;
}




/*-------------------------------------------------------------------------*
 * SQRT_UP                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Sqrt_Up(unsigned y)
{
  unsigned x;

  x = Sqrt_Dn(y);
  if (x * x != y)
    x++;

  return x;
}




/*-------------------------------------------------------------------------*
 * SQRT_EXACT                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Sqrt_Exact(unsigned y)
{
  unsigned x;

  x = Sqrt_Dn(y);
  if (x * x != y)
    return (unsigned) -1;

  return x;
}




/*-------------------------------------------------------------------------*
 * FIND_EXPON_DN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Find_Expon_Dn(unsigned x, unsigned y)
{
  unsigned n;
  unsigned xn;

  if (x <= 1 || y == 0)
    return INTERVAL_MAX_INTEGER;


  n = Find_Expon_General(x, y, &xn);

  return n;
}




/*-------------------------------------------------------------------------*
 * FIND_EXPON_UP                                                           *
 *                                                                         *
 * X must be > 1 and Y must be > 0                                         *
 *-------------------------------------------------------------------------*/
unsigned
Find_Expon_Up(unsigned x, unsigned y)
{
  unsigned n;
  unsigned xn;

  if (x <= 1 || y == 0)
    return INTERVAL_MAX_INTEGER;


  n = Find_Expon_General(x, y, &xn);

  return n + (y != xn);
}




/*-------------------------------------------------------------------------*
 * FIND_EXPON_EXACT                                                        *
 *                                                                         *
 * X must be > 1 and Y must be > 0                                         *
 *-------------------------------------------------------------------------*/
unsigned
Find_Expon_Exact(unsigned x, unsigned y)
{
  unsigned n;
  unsigned xn;

  if (x <= 1 || y == 0)
    return INTERVAL_MAX_INTEGER;


  n = Find_Expon_General(x, y, &xn);

  if (y != xn)
    return (unsigned) -1;

  return n;
}




/*-------------------------------------------------------------------------*
 * FIND_EXPON_GENERAL                                                      *
 *                                                                         *
 * X must be >1 and Y must be >0                                           *
 *-------------------------------------------------------------------------*/
static unsigned
Find_Expon_General(unsigned x, unsigned y, unsigned *pxn)
{
  static unsigned txp[sizeof(unsigned) * 8];
  unsigned *p = txp;
  unsigned xp;
  unsigned prod;
  unsigned n;
  unsigned xn;

  p = txp;
  xp = x;
  prod = 1;
  while (prod < y && (long) xp > 0)
    {
      *p++ = xp;
      prod *= xp;
      xp *= xp;
    }

  n = 0;
  xn = 1;

  while (--p >= txp)
    {
      xp = *p;
      n <<= 1;
      if (y >= xp)
	{
	  y /= xp;
	  xn *= xp;
	  n |= 1;
	}
    }

  *pxn = xn;
  return n;
}




/*-------------------------------------------------------------------------*
 * FULL_COEFF_POWER_VAR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Coeff_Power_Var(Range *y, int a, Range *n)
{
  unsigned an, an0;
  int i, vec_elem;

  an = Power(a, n->min);

  Vector_Allocate(y->vec);
  if (an > (unsigned) vec_max_integer)
    {
      y->extra_cstr = TRUE;
      Set_To_Empty(y);
      return;
    }

  Vector_Empty(y->vec);
  y->extra_cstr = FALSE;
  y->min = an;

  if (Is_Interval(n))		/* N is Interval */
    {
      an0 = an;
      for (i = n->min; i <= n->max; i++)
	{
	  if (an0 > (unsigned) vec_max_integer)
	    goto end_loop;

	  an = an0;
	  Vector_Set_Value(y->vec, an);
	  an0 *= a;
	}
    }
  else				/* N is Sparse */
    {
      y->extra_cstr = n->extra_cstr;

      VECTOR_BEGIN_ENUM(n->vec, vec_elem);

      an = Power(a, vec_elem);
      if (an > (unsigned) vec_max_integer)
	goto end_loop;

      Vector_Set_Value(y->vec, an);

      VECTOR_END_ENUM;
    }
end_loop:

  y->max = an;
}




/*-------------------------------------------------------------------------*
 * FULL_FIND_EXPON                                                         *
 *                                                                         *
 * Here A>=2 then Y>=1                                                     *
 *-------------------------------------------------------------------------*/
void
Full_Find_Expon(Range *n, int a, Range *y)
{
  int e, min;
  int i, vec_elem;

  Vector_Allocate(n->vec);
  Vector_Empty(n->vec);
  n->extra_cstr = y->extra_cstr;

  min = -1;


  if (Is_Interval(y))		/* Y is Interval */
    {
      for (i = y->min; i <= y->max; i++)
	{
	  e = Find_Expon_Exact(a, i);

	  if (e >= 0)
	    {
	      if (min < 0)
		min = e;

	      Vector_Set_Value(n->vec, e);
	    }
	}
    }
  else				/* Y is Sparse */
    {
      VECTOR_BEGIN_ENUM(y->vec, vec_elem);

      e = Find_Expon_Exact(a, vec_elem);

      if (e >= 0)
	{
	  if (min < 0)
	    min = e;

	  Vector_Set_Value(n->vec, e);
	}

      VECTOR_END_ENUM;
    }

  n->min = min;
  n->max = e;
}




/*-------------------------------------------------------------------------*
 * FULL_VAR_POWER_COEFF                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Var_Power_Coeff(Range *y, Range *x, int a)
{
  unsigned xa;
  int i, vec_elem;

  xa = Power(x->min, a);

  Vector_Allocate(y->vec);
  if (xa > (unsigned) vec_max_integer)
    {
      y->extra_cstr = TRUE;
      Set_To_Empty(y);
      return;
    }

  Vector_Empty(y->vec);
  y->extra_cstr = FALSE;
  y->min = xa;

  if (Is_Interval(x))		/* X is Interval */
    {
      for (i = x->min; i <= x->max; i++)
	{
	  xa = Power(i, a);
	  if (xa > (unsigned) vec_max_integer)
	    goto end_loop;

	  Vector_Set_Value(y->vec, xa);
	}
    }
  else				/* X is Sparse */
    {
      y->extra_cstr = x->extra_cstr;

      VECTOR_BEGIN_ENUM(x->vec, vec_elem);

      xa = Power(vec_elem, a);
      if (xa > (unsigned) vec_max_integer)
	goto end_loop;

      Vector_Set_Value(y->vec, xa);

      VECTOR_END_ENUM;
    }
end_loop:

  y->max = xa;
}




/*-------------------------------------------------------------------------*
 * FULL_NTH_ROOT                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Nth_Root(Range *x, Range *y, int a)
{
  int e, min;
  int i, vec_elem;

  Vector_Allocate(x->vec);
  Vector_Empty(x->vec);
  x->extra_cstr = y->extra_cstr;

  min = -1;


  if (Is_Interval(y))		/* Y is Interval */
    {
      for (i = y->min; i <= y->max; i++)
	{
	  e = Nth_Root_Exact(i, a);

	  if (e >= 0)
	    {
	      if (min < 0)
		min = e;

	      Vector_Set_Value(x->vec, e);
	    }
	}
    }
  else				/* Y is Sparse */
    {
      VECTOR_BEGIN_ENUM(y->vec, vec_elem);

      e = Nth_Root_Exact(vec_elem, a);

      if (e >= 0)
	{
	  if (min < 0)
	    min = e;

	  Vector_Set_Value(x->vec, e);
	}

      VECTOR_END_ENUM;
    }

  x->min = min;
  x->max = e;
}




/*-------------------------------------------------------------------------*
 * FULL_VAR_POWER_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Var_Power_2(Range *y, Range *x)
{
  unsigned x2;
  int i, vec_elem;

  x2 = x->min * x->min;

  Vector_Allocate(y->vec);
  if (x2 > (unsigned) vec_max_integer)
    {
      y->extra_cstr = TRUE;
      Set_To_Empty(y);
      return;
    }

  Vector_Empty(y->vec);
  y->extra_cstr = FALSE;
  y->min = x2;

  if (Is_Interval(x))		/* X is Interval */
    {
      for (i = x->min; i <= x->max; i++)
	{
	  x2 = i * i;
	  if (x2 > (unsigned) vec_max_integer)
	    goto end_loop;

	  Vector_Set_Value(y->vec, x2);
	}
    }
  else				/* X is Sparse */
    {
      y->extra_cstr = x->extra_cstr;

      VECTOR_BEGIN_ENUM(x->vec, vec_elem);

      x2 = vec_elem * vec_elem;
      if (x2 > (unsigned) vec_max_integer)
	goto end_loop;

      Vector_Set_Value(y->vec, x2);

      VECTOR_END_ENUM;
    }
end_loop:

  y->max = x2;
}




/*-------------------------------------------------------------------------*
 * FULL_SQRT_VAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Sqrt_Var(Range *x, Range *y)
{
  int e, min;
  int i, vec_elem;

  Vector_Allocate(x->vec);
  Vector_Empty(x->vec);
  x->extra_cstr = y->extra_cstr;

  min = -1;


  if (Is_Interval(y))		/* Y is Interval */
    {
      for (i = y->min; i <= y->max; i++)
	{
	  e = Sqrt_Exact(i);

	  if (e >= 0)
	    {
	      if (min < 0)
		min = e;

	      Vector_Set_Value(x->vec, e);
	    }
	}
    }
  else				/* Y is Sparse */
    {
      VECTOR_BEGIN_ENUM(y->vec, vec_elem);

      e = Sqrt_Exact(vec_elem);

      if (e >= 0)
	{
	  if (min < 0)
	    min = e;

	  Vector_Set_Value(x->vec, e);
	}

      VECTOR_END_ENUM;
    }

  x->min = min;
  x->max = e;
}




/*-------------------------------------------------------------------------*
 * FULL_VAR_DIV_VAR                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Full_Var_Div_Var(Range *x, Range *z, Range *y)
{
  if (y->min == 0)
    {
      Range_Init_Interval(x, 0, INTERVAL_MAX_INTEGER);
      return;
    }

  Range_Copy(x, z);
  Range_Div_Range(x, y);
}
