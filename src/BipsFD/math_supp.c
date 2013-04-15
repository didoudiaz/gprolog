/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : math_supp.c                                                     *
 * Descr.: mathematical support                                            *
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
#include <stdarg.h>
#include <string.h>

#define OBJ_INIT Math_Supp_Initializer

#define MATH_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"



#if 1
#define DEVELOP_TIMES_2
#endif

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DELAY_CSTR_STACK_SIZE      1000
#define VARS_STACK_SIZE            100000

#define MAX_MONOMS                 2000

#define MAX_COEF_FOR_SORT          100


#define PLUS_1                     0
#define PLUS_2                     1
#define MINUS_1                    2
#define MINUS_2                    3
#define TIMES_2                    4
#define DIV_2                      5
#define POWER_2                    6
#define MIN_2                      7
#define MAX_2                      8
#define DIST_2                     9
#define QUOT_2                     10
#define REM_2                      11
#define QUOT_REM_3                 12
#define NB_OF_OP                   13




#define DC_X2_EQ_Y                 0
#define DC_XY_EQ_Z                 1
#define DC_DIV_A_Y_EQ_Z            2
#define DC_DIV_X_A_EQ_Z            3
#define DC_DIV_X_Y_EQ_Z            4
#define DC_ZERO_POWER_N_EQ_Y       5
#define DC_A_POWER_N_EQ_Y          6
#define DC_X_POWER_A_EQ_Y          7
#define DC_MIN_X_A_EQ_Z            8
#define DC_MIN_X_Y_EQ_Z            9
#define DC_MAX_X_A_EQ_Z            10
#define DC_MAX_X_Y_EQ_Z            11
#define DC_ABS_X_MINUS_A_EQ_Z      12
#define DC_ABS_X_MINUS_Y_EQ_Z      13
#define DC_QUOT_REM_A_Y_R_EQ_Z     14
#define DC_QUOT_REM_X_A_R_EQ_Z     15
#define DC_QUOT_REM_X_Y_R_EQ_Z     16




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Monomial term information      */
{				/* ------------------------------ */
  PlLong a;			/* coefficient                    */
  WamWord x_word;		/* variable a tagged <REF,adr>    */
}
Monom;




typedef struct			/* Polynomial term information    */
{				/* ------------------------------ */
  PlLong c;			/* the constant                   */
  int nb_monom;			/* nb of monomial terms           */
  Monom m[MAX_MONOMS];		/* table of monomial terms        */
}
Poly;



typedef struct			/* Non linear constr information  */
{				/* ------------------------------ */
  int cstr;			/* DC_X2_EQ_Y, DC_XY_EQ_Z,...     */
  WamWord a1, a2, a3;		/* arguments (input)              */
  WamWord res;			/* argument  (result)             */
}
NonLin;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord arith_tbl[NB_OF_OP];


static NonLin delay_cstr_stack[DELAY_CSTR_STACK_SIZE];
static NonLin *delay_sp;


static WamWord vars_tbl[VARS_STACK_SIZE];
static WamWord *vars_sp;


static Bool sort;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Load_Left_Right_Rec(Bool optim_eq,
				WamWord le_word, WamWord re_word,
				int *mask, WamWord *c_word,
				WamWord *l_word, WamWord *r_word);

static int Compar_Monom(Monom *m1, Monom *m2);

static Bool Load_Term_Into_Word(WamWord e_word, WamWord *load_word);

static WamWord Push_Delayed_Cstr(int cstr, WamWord a1, WamWord a2,
				 WamWord a3);

static void Add_Monom(Poly *p, int sign, PlLong a, WamWord x_word);

#ifdef DEVELOP_TIMES_2
static Bool Add_Multiply_Monom(Poly *p, int sign, Monom *m1, Monom *m2);
#endif

static Bool Normalize(WamWord e_word, int sign, Poly *p);

static Bool Load_Poly(int nb_monom, Monom *m, WamWord pref_load_word,
		      WamWord *load_word);

static Bool Load_Poly_Rec(int nb_monom, Monom *m, WamWord load_word);

static Bool Load_Delay_Cstr_Part(void);



#ifdef DEBUG

void Pl_Write_1(WamWord term_word);

#endif

#define New_Tagged_Fd_Variable  (Tag_REF(Pl_Fd_New_Variable()))

#define New_Poly(p)             ((p).c = (p).nb_monom = 0)

#define Add_Cst_To_Poly(p, s, w)  (p->c += s * w)





/*-------------------------------------------------------------------------*
 * MATH_SUPP_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Math_Supp_Initializer(void)
{
  arith_tbl[PLUS_1] = Functor_Arity(ATOM_CHAR('+'), 1);
  arith_tbl[PLUS_2] = Functor_Arity(ATOM_CHAR('+'), 2);
  arith_tbl[MINUS_1] = Functor_Arity(ATOM_CHAR('-'), 1);
  arith_tbl[MINUS_2] = Functor_Arity(ATOM_CHAR('-'), 2);
  arith_tbl[TIMES_2] = Functor_Arity(ATOM_CHAR('*'), 2);
  arith_tbl[POWER_2] = Functor_Arity(Pl_Create_Atom("**"), 2);
  arith_tbl[DIV_2] = Functor_Arity(ATOM_CHAR('/'), 2);
  arith_tbl[MIN_2] = Functor_Arity(Pl_Create_Atom("min"), 2);
  arith_tbl[MAX_2] = Functor_Arity(Pl_Create_Atom("max"), 2);
  arith_tbl[DIST_2] = Functor_Arity(Pl_Create_Atom("dist"), 2);
  arith_tbl[QUOT_2] = Functor_Arity(Pl_Create_Atom("//"), 2);
  arith_tbl[REM_2] = Functor_Arity(Pl_Create_Atom("rem"), 2);
  arith_tbl[QUOT_REM_3] = Functor_Arity(Pl_Create_Atom("quot_rem"), 3);
}




/*-------------------------------------------------------------------------*
 * PL_LOAD_LEFT_RIGHT                                                      *
 *                                                                         *
 * This function loads the left and right term of a constraint into (new)  *
 * variables.                                                              *
 * Input:                                                                  *
 *    optim_eq: is used to optimize loadings of a term1 #= term2 constraint*
 *              when the constant is zero.                                 *
 *    le_word : left  term of the constraint                               *
 *    re_word : right term of the constraint                               *
 *                                                                         *
 * Output:                                                                 *
 *   mask     : indicates if l_word and r_word are used (see MASK_... cst) *
 *   c        : the general (signed) constant                              *
 *   l_word   : the variable containing the left  part  (tagged <REF,adr>) *
 *   r_word   : the variable containing the right part  (tagged <REF,adr>) *
 *-------------------------------------------------------------------------*/
Bool
Pl_Load_Left_Right(Bool optim_eq, WamWord le_word, WamWord re_word,
		int *mask, PlLong *c, WamWord *l_word, WamWord *r_word)
{
#ifdef DEBUG
  DBGPRINTF("\n*** Math constraint : ");
  Pl_Write_1(le_word);
  DBGPRINTF(" %s ", cur_op);
  Pl_Write_1(re_word);
  DBGPRINTF("\n");
#endif

  delay_sp = delay_cstr_stack;
  vars_sp = vars_tbl;

  return Load_Left_Right_Rec(optim_eq, le_word, re_word, mask, c,
			     l_word, r_word);
}




/*-------------------------------------------------------------------------*
 * PL_TERM_MATH_LOADING                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Term_Math_Loading(WamWord l_word, WamWord r_word)
{
  WamWord word, tag_mask;
  WamWord *adr, *fdv_adr;

  if (delay_sp != delay_cstr_stack)
    {
#ifdef DEBUG
      DBGPRINTF("\nnon Linear part\n");
#endif
      if (!Load_Delay_Cstr_Part())
	return FALSE;
    }

  while (--vars_sp >= vars_tbl)
    {
      DEREF(*vars_sp, word, tag_mask);
      if (tag_mask == TAG_REF_MASK && word != l_word && word != r_word)
	{
	  adr = UnTag_REF(word);
	  fdv_adr = Pl_Fd_New_Variable();
	  Bind_UV(adr, Tag_REF(fdv_adr));
	}
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * LOAD_LEFT_RIGHT_REC                                                     *
 *                                                                         *
 * This function can be called with re_word == NOT_A_WAM_WORD by the fct   *
 * Load_Term_Into_Word(). In that case, re_word is simply ignored.         *
 *-------------------------------------------------------------------------*/
static Bool
Load_Left_Right_Rec(Bool optim_eq, WamWord le_word, WamWord re_word,
		    int *mask, PlLong *c, WamWord *l_word, WamWord *r_word)
{
  Poly p;
  Monom *l_m, *r_m;
  Monom *cur, *pos, *neg, *end;
  int l_nb_monom, r_nb_monom;
  WamWord pref_load_word;	/* to optimize equalities (#=) */
  int i;

  sort = FALSE;
  New_Poly(p);

  if (!Normalize(le_word, +1, &p))
    return FALSE;

  if (re_word != NOT_A_WAM_WORD && !Normalize(re_word, -1, &p))
    return FALSE;

  if (sort || p.nb_monom > MAX_MONOMS / 2)
    {
      qsort(p.m, p.nb_monom, sizeof(Monom),
	    (int (*)(const void *, const void *)) Compar_Monom);

      for (i = 0; i < p.nb_monom; i++)	/* find left monomial terms */
	if (p.m[i].a <= 0)
	  break;

      l_m = p.m;
      l_nb_monom = i;

      for (; i < p.nb_monom; i++)	/* find right monomial terms */
	if (p.m[i].a >= 0)
	  break;
	else
	  p.m[i].a = -p.m[i].a;	        /* only positive coefs now */

      r_m = l_m + l_nb_monom;
      r_nb_monom = i - l_nb_monom;
    }
  else
    {
      pos = p.m;
      end = pos + p.nb_monom;
      neg = end;

      for (cur = pos; cur < end; cur++)
	{
	  if (cur->a < 0)
	    {
	      neg->a = -cur->a;
	      neg->x_word = cur->x_word;
	      neg++;
	      continue;
	    }

	  if (cur->a > 0)
	    {
	      if (cur != pos)
		*pos = *cur;
	      pos++;
	    }
	}

      l_m = p.m;
      l_nb_monom = pos - l_m;
      r_m = end;
      r_nb_monom = neg - r_m;

#ifdef DEBUG
      DBGPRINTF("l_nb_monom:%d   r_nb_monom:%d\n", l_nb_monom, r_nb_monom);
#endif
    }


#ifdef DEBUG
  DBGPRINTF("normalization: ");
  for (i = 0; i < l_nb_monom; i++)
    {
      DBGPRINTF("%" PL_FMT_d "*", l_m[i].a);
      Pl_Write_1(l_m[i].x_word);
      DBGPRINTF(" + ");
    }

  if (p.c > 0)
    DBGPRINTF("%" PL_FMT_d " + ", p.c);
  else if (l_nb_monom == 0)
    DBGPRINTF("0 + ");

  DBGPRINTF("\b\b%s ", (re_word != NOT_A_WAM_WORD) ? cur_op : "=");

  for (i = 0; i < r_nb_monom; i++)
    {
      DBGPRINTF("%" PL_FMT_d "*", r_m[i].a);
      Pl_Write_1(r_m[i].x_word);
      DBGPRINTF(" + ");
    }

  if (p.c < 0)
    DBGPRINTF("%" PL_FMT_d " + ", -p.c);
  else if (r_nb_monom == 0 && re_word != NOT_A_WAM_WORD)
    DBGPRINTF("0 + ");

  if (re_word == NOT_A_WAM_WORD)
    DBGPRINTF("loaded + ");

  DBGPRINTF("\b\b \n\n");
#endif


  pref_load_word = NOT_A_WAM_WORD;

  *mask = MASK_EMPTY;
  if (l_nb_monom)
    {
      *mask |= MASK_LEFT;
      if (optim_eq && p.c == 0 && r_nb_monom == 1 && r_m[0].a == 1)
	pref_load_word = r_m[0].x_word;

      if (!Load_Poly(l_nb_monom, l_m, pref_load_word, l_word))
	return FALSE;
    }

  if (r_nb_monom)
    {
      *mask |= MASK_RIGHT;
      if (pref_load_word == NOT_A_WAM_WORD)
	{
	  if (optim_eq && p.c == 0 && l_nb_monom)
	    pref_load_word = *l_word;

	  if (!Load_Poly(r_nb_monom, r_m, pref_load_word, r_word))
	    return FALSE;
	}
    }

  *c = p.c;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * LOAD_TERM_INTO_WORD                                                     *
 *                                                                         *
 * This function loads a term into a (tagged) word.                        *
 * Input:                                                                  *
 *    e_word  : term to load                                               *
 *                                                                         *
 * Output:                                                                 *
 *   load_word: the tagged word containing the loading of the term:        *
 *              can be a <INT,val> if there is no variable or a <REF,adr>) *
 *                                                                         *
 * This functions acts like T #= NewVar. However, if T is just an integer  *
 * it avoids the creation of a useless FD NewVar.                          *
 *-------------------------------------------------------------------------*/
static Bool
Load_Term_Into_Word(WamWord e_word, WamWord *load_word)
{
  int mask;
  WamWord l_word, r_word, word;
  PlLong c;


  if (!Load_Left_Right_Rec(FALSE, e_word, NOT_A_WAM_WORD, &mask, &c,
			   &l_word, &r_word))
    return FALSE;

  if (mask == MASK_EMPTY)
    {
      if (c < 0)
	return FALSE;

      *load_word = Tag_INT(c);
      return TRUE;
    }

  if (mask == MASK_LEFT && c == 0)
    {
      *load_word = l_word;
      return TRUE;
    }

  *load_word = New_Tagged_Fd_Variable;

  switch (mask)
    {
    case MASK_LEFT:		/* here c != 0 */
      if (c > 0)
	MATH_CSTR_3(pl_x_plus_c_eq_y, l_word, Tag_INT(c), *load_word);
      else
	MATH_CSTR_3(pl_x_plus_c_eq_y, *load_word, Tag_INT(-c), l_word);
      return TRUE;

    case MASK_RIGHT:
      if (c < 0)
	return FALSE;

      word = New_Tagged_Fd_Variable;
      MATH_CSTR_3(pl_x_plus_y_eq_z, r_word, *load_word, word);
      PRIM_CSTR_2(pl_x_eq_c, word, Tag_INT(c));
      return TRUE;
    }

  if (c == 0)
    {
      MATH_CSTR_3(pl_x_plus_y_eq_z, r_word, *load_word, l_word);
      return TRUE;
    }

  word = New_Tagged_Fd_Variable;
  MATH_CSTR_3(pl_x_plus_y_eq_z, r_word, *load_word, word);

  if (c > 0)
    MATH_CSTR_3(pl_x_plus_c_eq_y, l_word, Tag_INT(c), word);
  else
    MATH_CSTR_3(pl_x_plus_c_eq_y, word, Tag_INT(-c), l_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * COMPAR_MONOM                                                            *
 *                                                                         *
 * This function is called by qsort to order a polynomial term. It compares*
 * 2 monomial terms according to the following sequence:                   *
 *                                                                         *
 * positive coefficients (from greatest to smallest)                       *
 * negative coefficients (from smallest to greatest)                       *
 *                       (ie. from |greatest| to |smallest|)               *
 * null coefficients                                                       *
 *-------------------------------------------------------------------------*/
static int
Compar_Monom(Monom *m1, Monom *m2)
{
  PlLong cmp;

  if (m1->a > 0)
    cmp = (m2->a > 0) ? m2->a - m1->a : -1;
  else
    cmp = (m2->a > 0) ? +1 : m1->a - m2->a;

  return (cmp > 0) ? 1 : (cmp == 0) ? 0 : -1;
}




/*-------------------------------------------------------------------------*
 * PUSH_DELAYED_CSTR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Push_Delayed_Cstr(int cstr, WamWord a1, WamWord a2, WamWord a3)
{
  WamWord res_word;

  res_word = Make_Self_Ref(H);
  Global_Push(res_word);

  if (delay_sp - delay_cstr_stack >= DELAY_CSTR_STACK_SIZE)
    Pl_Err_Resource(pl_resource_too_big_fd_constraint);

  delay_sp->cstr = cstr;
  delay_sp->a1 = a1;
  delay_sp->a2 = a2;
  delay_sp->a3 = a3;
  delay_sp->res = res_word;

  delay_sp++;

  return res_word;
}




/*-------------------------------------------------------------------------*
 * ADD_MONOM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Add_Monom(Poly *p, int sign, PlLong a, WamWord x_word)
{
  int i;

  if (a == 0)
    return;

  if (sign < 0)
    a = -a;

  for (i = 0; i < p->nb_monom; i++)
    if (p->m[i].x_word == x_word)
      {
	p->m[i].a += a;
	return;
      }

  if (p->nb_monom >= MAX_MONOMS)
    Pl_Err_Resource(pl_resource_too_big_fd_constraint);

  p->m[p->nb_monom].a = a;
  p->m[p->nb_monom].x_word = x_word;
  p->nb_monom++;
}




#ifdef DEVELOP_TIMES_2
/*-------------------------------------------------------------------------*
 * ADD_MULTIPLY_MONOM                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Add_Multiply_Monom(Poly *p, int sign, Monom *m1, Monom *m2)
{
  PlLong a;
  WamWord x_word;

  a = m1->a * m2->a;

  if (a == 0)
    return TRUE;

  x_word = (m1->x_word == m2->x_word)
    ? Push_Delayed_Cstr(DC_X2_EQ_Y, m1->x_word, 0, 0)
    : Push_Delayed_Cstr(DC_XY_EQ_Z, m1->x_word, m2->x_word, 0);

  Add_Monom(p, sign, a, x_word);
  return TRUE;
}
#endif




/*-------------------------------------------------------------------------*
 * NORMALIZE                                                               *
 *                                                                         *
 * This functions normalizes a term.                                       *
 * Input:                                                                  *
 *    e_word: term to normalize                                            *
 *    sign  : current sign of the term (-1 or +1)                          *
 *                                                                         *
 * Output:                                                                 *
 *    p     : the associated polynomial term                               *
 *                                                                         *
 * Normalizes the term and loads it into p.                                *
 * Non-Linear operations are simplified and loaded into a stack to be      *
 * executed later.                                                         *
 *                                                                         *
 * T1*T2 : T1 and T2 are normalized to give the polynomials p1 and p2, with*
 *         p1 = c1 + a1X1 + a2X2 + ... + anXn                              *
 *         p2 = c2 + b1X1 + b2X2 + ... + bmXm                              *
 *         and replaced by c1*c2 +                                         *
 *                         a1X1 * c2 + a1X1 * b1X1 + ... + a1X1 * bmXm     *
 *                         ...                                             *
 *                         anX1 * c2 + anXn * b1X1 + ... + anXn * bmXm     *
 *                                                                         *
 * T1**T2: T1 and T2 are loaded into 2 new words word1 and word2 that can  *
 *         be integers or variables (tagged words). The code emitted       *
 *         depends on 3 possibilities (var**var is not allowed)            *
 *         (+ optim 1**T2, 0**T2, T1**0, T1**1), NB 0**0=1                 *
 *-------------------------------------------------------------------------*/
static Bool
Normalize(WamWord e_word, int sign, Poly *p)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *fdv_adr;
  WamWord word1, word2, word3;
  WamWord f_n, le_word, re_word;
  int i;
  PlLong n1, n2, n3;

 terminal_rec:

  DEREF(e_word, word, tag_mask);

  if (tag_mask == TAG_FDV_MASK)
    {
      fdv_adr = UnTag_FDV(word);
      Add_Monom(p, sign, 1, Tag_REF(fdv_adr));
      return TRUE;
    }

  if (tag_mask == TAG_INT_MASK)
    {
      n1 = UnTag_INT(word);
      if (n1 > MAX_COEF_FOR_SORT)
	sort = TRUE;

      Add_Cst_To_Poly(p, sign, n1);
      return TRUE;
    }

  if (tag_mask == TAG_REF_MASK)
    {
      if (vars_sp - vars_tbl >= VARS_STACK_SIZE)
	Pl_Err_Resource(pl_resource_too_big_fd_constraint);

      *vars_sp++ = word;
      Add_Monom(p, sign, 1, word);
      return TRUE;
    }

  if (tag_mask == TAG_ATM_MASK)
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Value(e_word);
      Pl_Unify_Integer(0);
    type_error:
      Pl_Err_Type(pl_type_fd_evaluable, word);
    }

  if (tag_mask != TAG_STC_MASK)
    goto type_error;


  adr = UnTag_STC(word);

  f_n = Functor_And_Arity(adr);
  for (i = 0; i < NB_OF_OP; i++)
    if (arith_tbl[i] == f_n)
      break;

  le_word = Arg(adr, 0);
  re_word = Arg(adr, 1);

  switch (i)
    {
    case PLUS_1:
      e_word = le_word;
      goto terminal_rec;

    case PLUS_2:
      if (!Pl_Blt_Compound(le_word)) /* try to avoid C stack overflow */
	{
	  if (!Normalize(le_word, sign, p))
	    return FALSE;
	  e_word = re_word;
	}
      else
	{
	  if (!Normalize(re_word, sign, p))
	    return FALSE;
	  e_word = le_word;
	}
      goto terminal_rec;

    case MINUS_2:
      if (!Pl_Blt_Compound(le_word)) /* try to avoid C stack overflow */
	{
	  if (!Normalize(le_word, sign, p))
	    return FALSE;
	  e_word = re_word;
	  sign = -sign;
	}
      else
	{
	  if (!Normalize(re_word, -sign, p))
	    return FALSE;
	  e_word = le_word;
	}
      goto terminal_rec;

    case MINUS_1:
      e_word = le_word;
      sign = -sign;
      goto terminal_rec;

    case TIMES_2:
#ifdef DEVELOP_TIMES_2
#if 1				/* optimize frequent use: INT*VAR */
      DEREF(le_word, word, tag_mask);
      if (tag_mask != TAG_INT_MASK)
	goto any;

      n1 = UnTag_INT(word);

      if (n1 > MAX_COEF_FOR_SORT)
	sort = TRUE;

      DEREF(re_word, word, tag_mask);
      if (tag_mask != TAG_REF_MASK)
	{
	  if (tag_mask != TAG_FDV_MASK)
	    goto any;
	  else
	    {
	      fdv_adr = UnTag_FDV(word);
	      word = Tag_REF(fdv_adr);
	    }
	}
      Add_Monom(p, sign, n1, word);
      return TRUE;
    any:
#endif
      {
	Poly p1, p2;
	int i1, i2;

	New_Poly(p1);
	New_Poly(p2);

	if (!Normalize(le_word, 1, &p1) || !Normalize(re_word, 1, &p2))
	  return FALSE;

	Add_Cst_To_Poly(p, sign, p1.c * p2.c);

	for (i1 = 0; i1 < p1.nb_monom; i1++)
	  {
	    Add_Monom(p, sign, p1.m[i1].a * p2.c, p1.m[i1].x_word);
	    for (i2 = 0; i2 < p2.nb_monom; i2++)
	      if (!Add_Multiply_Monom(p, sign, p1.m + i1, p2.m + i2))
		return FALSE;
	  }

	for (i2 = 0; i2 < p2.nb_monom; i2++)
	  Add_Monom(p, sign, p2.m[i2].a * p1.c, p2.m[i2].x_word);

	return TRUE;
      }
#else
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      n1 = n1 * n2;
	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  Add_Monom(p, sign, n1, word2);
	  return TRUE;
	}

      if (Tag_Is_INT(word2))
	{
	  n2 = UnTag_INT(word2);
	  Add_Monom(p, sign, n2, word1);
	  return TRUE;
	}


      word1 = (word1 == word2)
	? Push_Delayed_Cstr(DC_X2_EQ_Y, word1, 0, 0)
	: Push_Delayed_Cstr(DC_XY_EQ_Z, word1, word2, 0);

      Add_Monom(p, sign, 1, word1);
      return TRUE;
#endif

    case POWER_2:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      if ((n1 = Pl_Power(n1, n2)) < 0)
		return FALSE;

	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  if (n1 == 1)
	    {
	      Add_Cst_To_Poly(p, sign, 1);
	      return TRUE;
	    }

	  word = (n1 == 0)
	    ? Push_Delayed_Cstr(DC_ZERO_POWER_N_EQ_Y, word2, 0, 0)
	    : Push_Delayed_Cstr(DC_A_POWER_N_EQ_Y, word1, word2, 0);
	  goto end_power;
	}

      if (Tag_Mask_Of(word2) != TAG_INT_MASK)
	Pl_Err_Instantiation();
      else
	{
	  n2 = UnTag_INT(word2);
	  if (n2 == 0)
	    {
	      Add_Cst_To_Poly(p, sign, 1);
	      return TRUE;
	    }

	  word = (n2 == 1)
	    ? word1
	    : (n2 == 2)
	    ? Push_Delayed_Cstr(DC_X2_EQ_Y, word1, 0, 0)
	    : Push_Delayed_Cstr(DC_X_POWER_A_EQ_Y, word1, word2, 0);
	}
    end_power:
      Add_Monom(p, sign, 1, word);
      return TRUE;

    case MIN_2:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      n1 = math_min(n1, n2);
	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  word = Push_Delayed_Cstr(DC_MIN_X_A_EQ_Z, word2, word1, 0);
	  goto end_min;
	}

      if (Tag_Is_INT(word2))
	word = Push_Delayed_Cstr(DC_MIN_X_A_EQ_Z, word1, word2, 0);
      else
	word = Push_Delayed_Cstr(DC_MIN_X_Y_EQ_Z, word1, word2, 0);

    end_min:
      Add_Monom(p, sign, 1, word);
      return TRUE;

    case MAX_2:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      n1 = math_max(n1, n2);
	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  word = Push_Delayed_Cstr(DC_MAX_X_A_EQ_Z, word2, word1, 0);
	  goto end_max;
	}

      if (Tag_Is_INT(word2))
	word = Push_Delayed_Cstr(DC_MAX_X_A_EQ_Z, word1, word2, 0);
      else
	word = Push_Delayed_Cstr(DC_MAX_X_Y_EQ_Z, word1, word2, 0);

    end_max:
      Add_Monom(p, sign, 1, word);
      return TRUE;

    case DIST_2:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      n1 = (n1 >= n2) ? n1 - n2 : n2 - n1;
	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  word = Push_Delayed_Cstr(DC_ABS_X_MINUS_A_EQ_Z, word2, word1, 0);
	  goto end_dist;
	}

      if (Tag_Is_INT(word2))
	word = Push_Delayed_Cstr(DC_ABS_X_MINUS_A_EQ_Z, word1, word2, 0);
      else
	word = Push_Delayed_Cstr(DC_ABS_X_MINUS_Y_EQ_Z, word1, word2, 0);

    end_dist:
      Add_Monom(p, sign, 1, word);
      return TRUE;

    case QUOT_2:
      word3 = Make_Self_Ref(H);	/* word3 = remainder */
      Global_Push(word3);
      goto quot_rem;

    case REM_2:
      word3 = Make_Self_Ref(H);	/* word3 = remainder */
      Global_Push(word3);
      goto quot_rem;

    case QUOT_REM_3:
    quot_rem:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2) ||
	  (i == QUOT_REM_3 && !Load_Term_Into_Word(Arg(adr, 2), &word3)))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      if (n2 == 0)
		return FALSE;
	      n3 = n1 % n2;

	      if (i == QUOT_2 || i == QUOT_REM_3)
		{
		  if (i == QUOT_REM_3)
		    PRIM_CSTR_2(pl_x_eq_c, word3, word);
		  else
		    H--;	/* recover word3 space */
		  n3 = n1 / n2;
		}

	      Add_Cst_To_Poly(p, sign, n3);
	      return TRUE;
	    }

	  word = Push_Delayed_Cstr(DC_QUOT_REM_A_Y_R_EQ_Z, word1, word2, word3);
	  goto end_quot_rem;
	}

      if (Tag_Is_INT(word2))
	word = Push_Delayed_Cstr(DC_QUOT_REM_X_A_R_EQ_Z, word1, word2, word3);
      else
	word = Push_Delayed_Cstr(DC_QUOT_REM_X_Y_R_EQ_Z, word1, word2, word3);

    end_quot_rem:
      Add_Monom(p, sign, 1, (i == REM_2) ? word3 : word);
      return TRUE;

    case DIV_2:
      if (!Load_Term_Into_Word(le_word, &word1) ||
	  !Load_Term_Into_Word(re_word, &word2))
	return FALSE;

      if (Tag_Is_INT(word1))
	{
	  n1 = UnTag_INT(word1);
	  if (Tag_Is_INT(word2))
	    {
	      n2 = UnTag_INT(word2);
	      if (n2 == 0 || n1 % n2 != 0)
		return FALSE;
	      n1 /= n2;
	      Add_Cst_To_Poly(p, sign, n1);
	      return TRUE;
	    }

	  word = Push_Delayed_Cstr(DC_DIV_A_Y_EQ_Z, word1, word2, 0);
	  goto end_div;
	}

      if (Tag_Is_INT(word2))
	word = Push_Delayed_Cstr(DC_DIV_X_A_EQ_Z, word1, word2, 0);
      else
	word = Push_Delayed_Cstr(DC_DIV_X_Y_EQ_Z, word1, word2, 0);

    end_div:
      Add_Monom(p, sign, 1, word);
      return TRUE;

    default:
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(Functor(adr));
      Pl_Unify_Integer(Arity(adr));
      goto type_error;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * LOAD_POLY                                                               *
 *                                                                         *
 * This function loads a polynomial term (without constant) into a word.   *
 * Input:                                                                  *
 *    nb_monom      : nb of monomial terms (nb_monom > 0)                  *
 *    m             : array of monomial terms                              *
 *    pref_load_word: wanted load_word (or NOT_A_WAM_WORD)                 *
 *                                                                         *
 * Output:                                                                 *
 *   load_word      : the word containing the loading ie.: <REF,adr>       *
 *                                                                         *
 * This functions does not take into account constants.                    *
 *-------------------------------------------------------------------------*/
static Bool
Load_Poly(int nb_monom, Monom *m, WamWord pref_load_word,
	  WamWord *load_word)
{
  if (nb_monom == 1 && m[0].a == 1)
    {
      if (pref_load_word != NOT_A_WAM_WORD)
	{
	  if (!Pl_Fd_Math_Unify_X_Y(m[0].x_word, pref_load_word))
	    return FALSE;
	  *load_word = pref_load_word;
	  return TRUE;
	}

      *load_word = m[0].x_word;
      return TRUE;
    }

  if (pref_load_word != NOT_A_WAM_WORD)
    *load_word = pref_load_word;
  else
    *load_word = New_Tagged_Fd_Variable;

  return Load_Poly_Rec(nb_monom, m, *load_word);
}




/*-------------------------------------------------------------------------*
 * LOAD_POLY_REC                                                           *
 *                                                                         *
 * This function recursively loads a polynomial term into a word.          *
 * Input:                                                                  *
 *    nb_monom      : nb of monomial terms (nb_monom > 0)                  *
 *    m             : array of monomial terms                              *
 *    load_word      : the word where the term must be loaded              *
 *                                                                         *
 * At the entry, if nb_monom==1 then the coefficient of the monomial term  *
 * is > 1 (see call from Load_Poly() and recursive call).                  *
 *-------------------------------------------------------------------------*/
static Bool
Load_Poly_Rec(int nb_monom, Monom *m, WamWord load_word)
{
  WamWord load_word1;

  if (nb_monom == 1)
    {				/* here m[0].a != 1 */
      MATH_CSTR_3(pl_ax_eq_y, Tag_INT(m[0].a), m[0].x_word, load_word);

      return TRUE;
    }


  if (nb_monom == 2)
    {
      if (m[0].a == 1)
	{
	  if (m[1].a == 1)
	    MATH_CSTR_3(pl_x_plus_y_eq_z, m[0].x_word, m[1].x_word, load_word);
	  else
	    MATH_CSTR_4(pl_ax_plus_y_eq_z, Tag_INT(m[1].a), m[1].x_word,
			m[0].x_word, load_word);
	}
      else if (m[1].a == 1)
	MATH_CSTR_4(pl_ax_plus_y_eq_z, Tag_INT(m[0].a), m[0].x_word, m[1].x_word,
		    load_word);
      else
	MATH_CSTR_5(pl_ax_plus_by_eq_z, Tag_INT(m[0].a), m[0].x_word,
		    Tag_INT(m[1].a), m[1].x_word, load_word);

      return TRUE;
    }

  if (nb_monom == 3 && m[2].a == 1)
    load_word1 = m[2].x_word;
  else
    load_word1 = New_Tagged_Fd_Variable;

  if (m[0].a == 1)
    {
      if (m[1].a == 1)
	MATH_CSTR_4(pl_x_plus_y_plus_z_eq_t, m[0].x_word, m[1].x_word,
		    load_word1, load_word);
      else
	MATH_CSTR_5(pl_ax_plus_y_plus_z_eq_t, Tag_INT(m[1].a), m[1].x_word,
		    m[0].x_word, load_word1, load_word);
    }
  else if (m[1].a == 1)
    MATH_CSTR_5(pl_ax_plus_y_plus_z_eq_t, Tag_INT(m[0].a), m[0].x_word,
		m[1].x_word, load_word1, load_word);
  else
    PRIM_CSTR_6(pl_ax_plus_by_plus_z_eq_t, Tag_INT(m[0].a), m[0].x_word,
		Tag_INT(m[1].a), m[1].x_word, load_word1, load_word);

  if (!(nb_monom == 3 && m[2].a == 1))
    return Load_Poly_Rec(nb_monom - 2, m + 2, load_word1);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * LOAD_DELAY_CSTR_PART                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Load_Delay_Cstr_Part(void)
{
  NonLin *i;

  for (i = delay_cstr_stack; i < delay_sp; i++)
    {
      switch (i->cstr)
	{
	case DC_X2_EQ_Y:
	  MATH_CSTR_2(pl_x2_eq_y, i->a1, i->res);
	  break;

	case DC_XY_EQ_Z:
	  MATH_CSTR_3(pl_xy_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_DIV_A_Y_EQ_Z:
	  PRIM_CSTR_2(pl_x_gte_c, i->a2, Tag_INT(1));
	  MATH_CSTR_3(pl_xy_eq_z, i->res, i->a2, i->a1);
	  break;

	case DC_DIV_X_A_EQ_Z:
	  MATH_CSTR_3(pl_ax_eq_y, i->a2, i->res, i->a1);
	  break;

	case DC_DIV_X_Y_EQ_Z:
	  PRIM_CSTR_2(pl_x_gte_c, i->a2, Tag_INT(1));
	  MATH_CSTR_3(pl_xy_eq_z, i->res, i->a2, i->a1);
	  break;

	case DC_ZERO_POWER_N_EQ_Y:
	  PRIM_CSTR_2(pl_zero_power_n_eq_y, i->a1, i->res);
	  break;

	case DC_A_POWER_N_EQ_Y:
	  MATH_CSTR_3(pl_a_power_n_eq_y, i->a1, i->a2, i->res);
	  break;

	case DC_X_POWER_A_EQ_Y:
	  MATH_CSTR_3(pl_x_power_a_eq_y, i->a1, i->a2, i->res);
	  break;

	case DC_MIN_X_A_EQ_Z:
	  MATH_CSTR_3(pl_min_x_a_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_MIN_X_Y_EQ_Z:
	  MATH_CSTR_3(pl_min_x_y_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_MAX_X_A_EQ_Z:
	  MATH_CSTR_3(pl_max_x_a_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_MAX_X_Y_EQ_Z:
	  MATH_CSTR_3(pl_max_x_y_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_ABS_X_MINUS_A_EQ_Z:
	  MATH_CSTR_3(pl_abs_x_minus_a_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_ABS_X_MINUS_Y_EQ_Z:
	  MATH_CSTR_3(pl_abs_x_minus_y_eq_z, i->a1, i->a2, i->res);
	  break;

	case DC_QUOT_REM_A_Y_R_EQ_Z:
	  MATH_CSTR_4(pl_quot_rem_a_y_r_eq_z, i->a1, i->a2, i->a3, i->res);
	  break;

	case DC_QUOT_REM_X_A_R_EQ_Z:
	  MATH_CSTR_4(pl_quot_rem_x_a_r_eq_z, i->a1, i->a2, i->a3, i->res);
	  break;

	case DC_QUOT_REM_X_Y_R_EQ_Z:
	  MATH_CSTR_4(pl_quot_rem_x_y_r_eq_z, i->a1, i->a2, i->a3, i->res);
	  break;
	}
    }

  delay_sp = delay_cstr_stack;
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_MATH_UNIFY_X_Y                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Math_Unify_X_Y(WamWord x, WamWord y)
{
  WamWord x_word, x_tag;
  WamWord y_word, y_tag;

  DEREF(x, x_word, x_tag);
  DEREF(y, y_word, y_tag);

  if (x_tag == TAG_FDV_MASK && y_tag == TAG_FDV_MASK)
    {
      MATH_CSTR_2(pl_x_eq_y, x, y);
      return TRUE;
    }

#ifdef DEBUG
  DBGPRINTF("Prolog Unif: ");
  Pl_Write_1(x_word);
  DBGPRINTF(" = ");
  Pl_Write_1(y_word);
  DBGPRINTF("\n");
#endif

  return Pl_Unify(x_word, y_word);
}





/*-------------------------------------------------------------------------*
 * X_EQ_C                                                                  *
 *                                                                         *
 * Defined here instead in fd_math_fd.fd to avoid A frame creation.        *
 *-------------------------------------------------------------------------*/
Bool
pl_x_eq_c(WamWord x, WamWord c)
{
  return Pl_Get_Integer_Tagged(c, x);
}





#ifdef DEBUG

/*-------------------------------------------------------------------------*
 * DEBUG_DISPLAY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Debug_Display(char *fct, int n, ...)
{
  va_list arg_ptr;
  WamWord word;
  int i;
  char *s1[] = { "plus", "eq", "neq", "lte", "lt", "gte", "gt", NULL };
  char *s2[] = { "+", "=", "\\=", "<=", "<", ">=", ">" };
  char **s;
  char *p;

  va_start(arg_ptr, n);

  DBGPRINTF("'");

  for (p = fct; *p; p++)
    {
      if (*p == '_')
	{
	  for (s = s1; *s; s++)
	    {
	      i = strlen(*s);
	      if (strncmp(*s, p + 1, i) == 0)
		break;
	    }

	  if (*s && p[1 + i] == '_')
	    {
	      p += 1 + i;
	      DBGPRINTF("%s", s2[s - s1]);
	      continue;
	    }
	}

      DBGPRINTF("%c", *p);
    }


  DBGPRINTF("'(");
  for (i = 0; i < n; i++)
    {
      word = va_arg(arg_ptr, WamWord);

      Pl_Write_1(word);
      DBGPRINTF("%c", (i < n - 1) ? ',' : ')');
    }

  va_end(arg_ptr);
  DBGPRINTF("\n");
}


#endif
