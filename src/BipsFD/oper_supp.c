/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : oper_supp.c                                                     *
 * Descr.: FD Operation support                                            *
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
 * PL_POWER                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Power(int x, unsigned n)
{
  PlLong xn, xp;

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

  if (xn < INTERVAL_MIN_INTEGER)
    return INTERVAL_MIN_INTEGER;

  if (xn <= INTERVAL_MAX_INTEGER)
    return (int) xn;

  return INTERVAL_MAX_INTEGER;
}




/*-------------------------------------------------------------------------*
 * PL_NTH_ROOT_DN                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Nth_Root_Dn(unsigned y, unsigned n)
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

  bit = Pl_Most_Significant_Bit(y);

  if ((unsigned) (bit + 1) < n)
    return 1;

  nb = bit / n;
  new = 1 << nb;

  old = new;
  oldn1 = Pl_Power(old, n1);
  new = (n1 * old + y / oldn1) / n;

  do
    {
      old = new;
      oldn1 = Pl_Power(old, (unsigned) n1);
      new = (n1 * old + y / oldn1) / n;
    }
  while (new < old);

  return old;
}



/*-------------------------------------------------------------------------*
 * PL_NTH_ROOT_UP                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Nth_Root_Up(unsigned y, unsigned n)
{
  unsigned x;

  if (y == 0)
    return 0;

  if (n == 0)
    return 0;

  x = Pl_Nth_Root_Dn(y, n);
  if (Pl_Power(x, n) != y)
    x++;

  return x;
}




/*-------------------------------------------------------------------------*
 * PL_NTH_ROOT_EXACT                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Nth_Root_Exact(unsigned y, unsigned n)
{
  unsigned x;

  if (y == 0)
    return 0;

  x = Pl_Nth_Root_Dn(y, n);
  if (Pl_Power(x, n) != y)
    return (unsigned) -1;

  return x;
}




/*-------------------------------------------------------------------------*
 * PL_SQRT_DN                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Sqrt_Dn(unsigned y)
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
 * PL_SQRT_UP                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Sqrt_Up(unsigned y)
{
  unsigned x;

  x = Pl_Sqrt_Dn(y);
  if (x * x != y)
    x++;

  return x;
}




/*-------------------------------------------------------------------------*
 * PL_SQRT_EXACT                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Sqrt_Exact(unsigned y)
{
  unsigned x;

  x = Pl_Sqrt_Dn(y);
  if (x * x != y)
    return (unsigned) -1;

  return x;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_EXPON_DN                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Find_Expon_Dn(unsigned x, unsigned y)
{
  unsigned n;
  unsigned xn;

  if (x <= 1 || y == 0)
    return INTERVAL_MAX_INTEGER;

  n = Find_Expon_General(x, y, &xn);

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_EXPON_UP                                                        *
 *                                                                         *
 * X must be > 1 and Y must be > 0                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Find_Expon_Up(unsigned x, unsigned y)
{
  unsigned n;
  unsigned xn;

  if (x <= 1 || y == 0)
    return INTERVAL_MAX_INTEGER;

  n = Find_Expon_General(x, y, &xn);

  return n + (y != xn);
}




/*-------------------------------------------------------------------------*
 * PL_FIND_EXPON_EXACT                                                     *
 *                                                                         *
 * X must be > 1 and Y must be > 0                                         *
 *-------------------------------------------------------------------------*/
unsigned
Pl_Find_Expon_Exact(unsigned x, unsigned y)
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
 * X must be > 1 and Y must be > 0                                         *
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
  while (prod < y && (PlLong) xp > 0)
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
 * PL_FULL_COEFF_POWER_VAR                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Coeff_Power_Var(Range *y, int a, Range *n)
{
  int an, an0, min_n;
  int i;

  //Set_To_Empty(y);
  if (a == 0) {
    // 0 or 1
    y->min = (n->min==0 && n->max==0) ? 1 : 0;
    if (Pl_Range_Test_Value(n,0)) y->max = 1;
    else y->max = 0;
    return;
  }
  // a != 0
  if (Is_Interval(n)) {			/* N is Interval */
    min_n = math_max(n->min, 0); // only positive N
    an = Pl_Power(a, min_n);
    an0 = an;
    for (i = min_n; i <= n->max; i++) {
      if (an0 >= INTERVAL_MAX_INTEGER || an0 <= INTERVAL_MIN_INTEGER)
      	return;
      an = an0;
      Pl_Range_Set_Value(y,an);
      an0 *= a;
    }
  }
  else {				/* N is Sparse */
    Chunk *chunk = n->first;
    while (chunk!=NULL) {
      min_n = math_max(chunk->min, 0);
      an = Pl_Power(a, min_n);
      an0 = an;
      for (i = min_n; i <= chunk->max; i++) {
	if (an0 >= INTERVAL_MAX_INTEGER || an0 <= INTERVAL_MIN_INTEGER)
	  return;
	an = an0;
	Pl_Range_Set_Value(y, an);
	an0 *= a;
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_FIND_EXPON                                                      *
 *                                                                         *
 * Here A>=2 then Y>=1                                                     *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Find_Expon(Range *n, int a, Range *y)
{
  int e;
  int i;

  //Set_To_Empty(y);
  if (Is_Interval(y)) {		/* Y is Interval */
    for (i = y->min; i <= y->max; i++) {
      e = Pl_Find_Expon_Exact(a, i);
      if (e >= 0) 
	Pl_Range_Set_Value(n, e);
    }
  }
  else {			/* Y is Sparse */
    Chunk *chunk = y->first;
    while (chunk!=NULL) {
      for (i = chunk->min; i <= chunk->max; i++) {
        e = Pl_Find_Expon_Exact(a, i);
        if (e >= 0)
          Pl_Range_Set_Value(n, e);
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_VAR_POWER_COEFF                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Var_Power_Coeff(Range *y, Range *x, int a)
{
  int xa;
  int i;

  // base cases (not sure if needed)
  if (a < 0) {
    Set_To_Empty(y);
    return;
  } else if (a==0) {
    Set_To_Empty(y);
    y->min = 1; 
    y->max = 1;
    return;
  } else if (a==1) {
    return;
  }

  
  if (Is_Interval(x)) {		/* X is Interval */
    for (i = x->min; i <= x->max; i++) {
      xa = Pl_Power(i, a);
      if (xa >= INTERVAL_MAX_INTEGER || xa <= INTERVAL_MIN_INTEGER)
      	continue;
      Pl_Range_Set_Value(y, xa);
    }
  }
  else {			/* X is Sparse */
    Chunk *chunk = x->first;
    while (chunk!=NULL) {
      for (i = chunk->min; i <= chunk->max; i++) {
	xa = Pl_Power(i, a);
	if (xa >= INTERVAL_MAX_INTEGER || xa <= INTERVAL_MIN_INTEGER)
	  continue;
	Pl_Range_Set_Value(y, xa);
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_NTH_ROOT                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Nth_Root(Range *x, Range *y, int a)
{
  int e;
  int i;

  if (Is_Interval(y)) {		/* Y is Interval */
    for (i = y->min; i <= y->max; i++) {
      e = Pl_Nth_Root_Exact(i, a);
      if (e >= 0)
        Pl_Range_Set_Value(x, e);
    }
  }
  else {			/* Y is Sparse */
    Chunk *chunk = y->first;
    while (chunk!=NULL) {
      for (i = chunk->min; i <= chunk->max; i++) {
	e = Pl_Nth_Root_Exact(i, a);
	if (e >= 0)
	  Pl_Range_Set_Value(x, e);
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_VAR_POWER_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Var_Power_2(Range *y, Range *x)
{
  int i;
  int min_x, max_x;
  // max_val is for preventing overflows
  int max_val = Pl_Sqrt_Dn(INTERVAL_MAX_INTEGER);

  Set_To_Empty(y);
  if (Is_Interval(x)) {
    // -x^2 == x^2 (so make everything positive)
    min_x = math_min(abs(closest_to_zero(x->min, x->max)), max_val);
    max_x = math_min(math_max(abs(x->min), abs(x->max)), max_val);
    for (i = min_x; i <= max_x; i++) {
      // i*i should be < MAX
      Pl_Range_Set_Value(y,i*i);
    }
  }
  else {
    Chunk *chunk = x->first;
    while (chunk!=NULL) {
      // -x^2 == x^2 (so make everything positive)
      min_x = math_min(abs(closest_to_zero(chunk->min, chunk->max)), max_val);
      max_x = math_min(math_max(abs(chunk->min), abs(chunk->max)), max_val);
      for (i = min_x; i <= max_x; i++) {
	// i*i should be < MAX
	Pl_Range_Set_Value(y,i*i);
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_SQRT_VAR                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Sqrt_Var(Range *x, Range *y)
{
  int e;
  int i;

  // take only positive y
  // don't take values > (x->max ** 2)
  int max_val = (x->max < (int) Pl_Sqrt_Up(INTERVAL_MAX_INTEGER)) ? (x->max * x->max) : INTERVAL_MAX_INTEGER;
  Set_To_Empty(x);

  if (Is_Interval(y)) {		/* Y is Interval */
    for (i = math_max(0,y->min); i <= math_max(0,math_min(max_val,y->max)); i++) {
      e = Pl_Sqrt_Exact(i);
      if (e >= 0) {
        Pl_Range_Set_Value(x, -e);
        Pl_Range_Set_Value(x, e);
      }
    }
  }
  else {			/* Y is Sparse */
    Chunk *chunk = y->first;
    while (chunk!=NULL) {
      for (i = math_max(0,chunk->min); i <= math_max(0,math_min(max_val,chunk->max)); i++) {
	e = Pl_Sqrt_Exact(i);
	if (e >= 0) {
	  Pl_Range_Set_Value(x, -e);
	  Pl_Range_Set_Value(x, e);
	}
      }
      chunk = chunk->next;
    }
  }
}




/*-------------------------------------------------------------------------*
 * PL_FULL_VAR_DIV_VAR                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Full_Var_Div_Var(Range *x, Range *z, Range *y)
{
  if (y->min == 0)
    {
      Range_Init_Interval(x, INTERVAL_MIN_INTEGER, INTERVAL_MAX_INTEGER);
      return;
    }

  Pl_Range_Copy(x, z);
  Pl_Range_Div_Range(x, y);
}
