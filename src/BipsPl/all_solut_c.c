/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : all_solut_c.c                                                   *
 * Descr.: all solution collector management - C part                      *
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

#include <sys/types.h>

#define OBJ_INIT All_Solut_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#ifndef _WIN32
#include <unistd.h>
#include <sys/wait.h>
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct onesol *OneSolP;

typedef struct onesol
{
  OneSolP prev;
  int sol_no;
  int term_size;
  WamWord term_word;
}
OneSol;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord exist_2;

static WamWord new_gen_word;



static long *bound_var_ptr;
static WamWord *free_var_base;



static OneSol dummy = { NULL, 0, 0 };
static OneSol *sol = &dummy;

static long *key_var_ptr;
static long *save_key_var_ptr;
static long *next_key_var_ptr;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Bound_Var(WamWord *adr);

static WamWord Existential_Variables(WamWord start_word);

static void Free_Var(WamWord *adr);



static void Handle_Key_Variables(WamWord start_word);

static void Link_Key_Var(WamWord *adr);



static WamWord Group(WamWord all_sol_word, WamWord gl_key_word,
		     WamWord *key_adr);



#define GROUP_SOLUTIONS_ALT       X2467726F75705F736F6C7574696F6E735F616C74

Prolog_Prototype(GROUP_SOLUTIONS_ALT, 0);




/*-------------------------------------------------------------------------*
 * ALL_SOLUT_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
All_Solut_Initializer(void)
{
  exist_2 = Functor_Arity(ATOM_CHAR('^'), 2);
}




/*-------------------------------------------------------------------------*
 * This part collects all free variables, i.e. variables appearing in the  *
 * generator but neither in the template nor in the set of existentially   *
 * qualified variables.                                                    *
 *   - collect variables of the template (bound variables)                 *
 *   - collect existentially qualified variables of the generator          *
 *     btw: compute the unqualified generator                              *
 *   - make in the heap the array of free variables of the unqualified     *
 *     generator                                                           *
 *   - create a Prolog term (gl_key) with the free variables.              *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * FREE_VARIABLES_4                                                        *
 *                                                                         *
 * Fail if no free variables.                                              *
 *-------------------------------------------------------------------------*/
Bool
Free_Variables_4(WamWord templ_word, WamWord gen_word, WamWord gen1_word,
		 WamWord key_word)
{
  WamWord gl_key_word;
  WamWord *save_H, *arg;
  int nb_free_var = 0;

  bound_var_ptr = glob_dico_var;	/* glob_dico_var: stores bound vars */

  Treat_Vars_Of_Term(templ_word, TRUE, Bound_Var);

  new_gen_word = Existential_Variables(gen_word);

  save_H = H++;			/* one more word for f/n is possible */

  arg = free_var_base = H;	/* array is in the heap */
  Treat_Vars_Of_Term(new_gen_word, TRUE, Free_Var);
  nb_free_var = H - arg;

  if (nb_free_var == 0)
    return FALSE;

  if (nb_free_var <= MAX_ARITY)
    {
      *save_H = Functor_Arity(ATOM_CHAR('.'), nb_free_var);
      gl_key_word = Tag_STC(save_H);
    }
  else
    {
      H = free_var_base;
      gl_key_word = Mk_Proper_List(nb_free_var, arg);
    }

  Unify(new_gen_word, gen1_word);
  return Unify(gl_key_word, key_word);
}




/*-------------------------------------------------------------------------*
 * RECOVER_GENERATOR_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Recover_Generator_1(WamWord gen1_word)
{
  Unify(new_gen_word, gen1_word);
}




/*-------------------------------------------------------------------------*
 * BOUND_VAR                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Bound_Var(WamWord *adr)
{
  long *p;

  for (p = glob_dico_var; p < bound_var_ptr; p++)
    if (*p == (long) adr)
      return;

  if (bound_var_ptr - glob_dico_var >= MAX_VAR_IN_TERM)
    Pl_Err_Representation(representation_too_many_variables);

  *bound_var_ptr++ = (long) adr;
}




/*-------------------------------------------------------------------------*
 * EXISTENTIAL_VARIABLES                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Existential_Variables(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);
  
  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      if (Functor_And_Arity(adr) == exist_2)
	{
	  Treat_Vars_Of_Term(Arg(adr, 0), TRUE, Bound_Var);
	  word = Existential_Variables(Arg(adr, 1));
	}
    }

  return word;
}




/*-------------------------------------------------------------------------*
 * FREE_VAR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Free_Var(WamWord *adr)
{
  long *p;
  WamWord word;

  for (p = glob_dico_var; p < bound_var_ptr; p++)
    if (*p == (long) adr)
      return;

  word = Tag_REF(adr);	/* if an FDV for a Dont_Separate_Tag */

  for (p = free_var_base; p < H; p++)
    if (*p == word)
      return;

  *H++ = word;
}




/*-------------------------------------------------------------------------*
 * This part saves and restores all solutions found. A stack of solutions  *
 * is used (each solution is copied to a Mallocated memory area). To handle*
 * nested findall and al, each solution receive a sequential number. At the*
 * start, the number of the solution on the top of the stack serves as a   *
 * stop mark.                                                              *
 *                                                                         *
 * Recovering the solutions: a space for the list of (nb_sol) solutions is *
 * reserved on the top of the heap (nb_sol*2 WamWords) then each term is   *
 * poped from the stack and copied to the heap (the list of solutions is   *
 * constructed from last to first (since we handle a stack).               *
 *                                                                         *
 * There is a special treatment for bagof/3. Each solution is a term of    *
 * the form Key-Value. In order to group solutions by Key we use a keysort *
 * (done in Prolog) + Group_Solutions_3 (written in C). However, keysort/2 *
 * tests a term equality (==) while a structural equality is needed.       *
 *                                                                         *
 * Structural equality: T1 and T2 are structurally equal if their tree     *
 * representation is equivalent (we say that T1 and T2 are variant). Namely*
 * there is a a bijection f from the variable of T1 to the variables of T2 *
 * such that T2 == f(T1).                                                  *
 * For instance f(A,g(B),A) is a variant of f(C,g(D),C) but f(A,B) is not a*
 * variant of f(C,D).                                                      *
 *                                                                         *
 * Since we use keysort we have to first transform two keys K1 and K2 that *
 * are variants to a same term K. This can be done by unifying (linking)   *
 * each (unbound) variable of K1 with a unique term, similarly for K2.     *
 * The unique term used is a free variable (in fact a variable of K1).     *
 * The function Handle_Key_Variables performs that. A set of used variables*
 * is maintained (a stack). Treatment of a key:                            *
 *                                                                         *
 *   - save_key_var_ptr=key_var_ptr (save current top of stack)            *
 *   - next_key_var_ptr=base of the stack                                  *
 *   - for each variable V of Key:                                         *
 *       - if V is in the stack do nothing                                 *
 *       - if next_key_var_ptr<save_key_var_ptr (can reuse a variable)     *
 *         then Unify(V,*next_key_var_ptr++)                               *
 *       - otherwise push V (*key_var_ptr++=V)                             *
 *                                                                         *
 * E.g. the keys [A,B,A], [f(C),D,E,F], [G,H,G] and [f(C),D,E,F] become:   *
 * [X1,X2,X1], [f(X1),X2,X3,X4], [X1,X2,X1] and [f(X1),X2,X3,X4]           *
 * The fact that a same variable (e.g. X1) appears in 2 keys that are not  *
 * variants is not a problem since they will not be unified at the same    *
 * moment. Indeed, they corresponds to 2 different group of solutions which*
 * are not yielded in the same calculus (a backtracking will occurs between*
 * each solution, c.f. Group_Solutions_3).                                 *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * STOP_MARK_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Stop_Mark_1(WamWord stop_word)
{
  Get_Integer(sol->sol_no, stop_word);
}




/*-------------------------------------------------------------------------*
 * STORE_SOLUTION_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Store_Solution_1(WamWord term_word)
{
  OneSol *s;
  int size;
/* fix_bug is because when gcc sees &xxx where xxx is a fct argument variable
 * it allocates a frame even with -fomit-frame-pointer.
 * This corrupts ebp on ix86 */
  static WamWord fix_bug;

  size = Term_Size(term_word);

  s = (OneSol *) Malloc(sizeof(OneSol) - sizeof(WamWord) +
			size * sizeof(WamWord));
  s->prev = sol;
  s->sol_no = sol->sol_no + 1;
  s->term_size = size;
  fix_bug = term_word;	
  Copy_Term(&s->term_word, &fix_bug);
  sol = s;
}




/*-------------------------------------------------------------------------*
 * RECOVER_SOLUTIONS_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Recover_Solutions_2(WamWord stop_word, WamWord handle_key_word,
		    WamWord list_word)
{
  int stop;
  int nb_sol;
  WamWord *p, *q;
  OneSol *s;
  Bool handle_key;

  stop = Rd_Integer(stop_word);
  nb_sol = sol->sol_no - stop;

  if (nb_sol == 0)
    return Get_Nil(list_word);

  handle_key = Rd_Integer(handle_key_word);
  key_var_ptr = glob_dico_var;	/* glob_dico_var: key vars */


  H += 2 * nb_sol;
  p = q = H;

  while (nb_sol--)
    {
      p--;
      *p = Tag_LST(p + 1);
      *--p = Tag_REF(H);
      Copy_Contiguous_Term(H, &sol->term_word);

      if (handle_key)
	Handle_Key_Variables(*H);

      H += sol->term_size;
      s = sol;
      sol = sol->prev;
      Free(s);
    }

  q[-1] = NIL_WORD;
  return Unify(Tag_LST(p), list_word);
}




/*-------------------------------------------------------------------------*
 * HANDLE_KEY_VARIABLES                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Handle_Key_Variables(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;

  save_key_var_ptr = key_var_ptr;
  next_key_var_ptr = glob_dico_var;

  DEREF(start_word, word, tag_mask);
  adr = UnTag_STC(word);
  Treat_Vars_Of_Term(Arg(adr, 0), TRUE, Link_Key_Var);
}




/*-------------------------------------------------------------------------*
 * LINK_KEY_VAR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Link_Key_Var(WamWord *adr)
{
  long *p;

  for (p = glob_dico_var; p < key_var_ptr; p++)
    if (*p == (long) adr)
      return;

  if (next_key_var_ptr < save_key_var_ptr)
    {		      /* same as Unify(Tag_REF(adr), *next_key_var_ptr++) */
      *adr = *(WamWord *) (*next_key_var_ptr);
      next_key_var_ptr++;
      return;
    }

  if (key_var_ptr - glob_dico_var >= MAX_VAR_IN_TERM)
    Pl_Err_Representation(representation_too_many_variables);

  *key_var_ptr++ = (long) adr;
}




/*-------------------------------------------------------------------------*
 * This part goups the solutions according to their Key. The list of all   *
 * solutions is keysorted (form [Key-Val,...]). This function could be     *
 * written in Prolog but we perform an update-in-place of the list saving  *
 * thus memory space.                                                      *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * GROUP_SOLUTIONS_3                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Group_Solutions_3(WamWord all_sol_word, WamWord gl_key_word,
		  WamWord sol_word)
{
  WamWord word, tag_mask;
  WamWord key_word;

  DEREF(all_sol_word, word, tag_mask);
  if (word == NIL_WORD)
    return FALSE;
  
  word = Group(all_sol_word, gl_key_word, &key_word);
  if (word != NOT_A_WAM_WORD)
    {
      A(0) = word;
      A(1) = gl_key_word;
      A(2) = sol_word;
      Create_Choice_Point((CodePtr)
			  Prolog_Predicate(GROUP_SOLUTIONS_ALT, 0), 3);
    }

  Unify(key_word, gl_key_word);
  return Unify(sol_word, all_sol_word);
}




/*-------------------------------------------------------------------------*
 * GROUP_SOLUTIONS_ALT_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Group_Solutions_Alt_0(void)
{
  WamWord all_sol_word, gl_key_word, sol_word;
  WamWord word;
  WamWord key_word;

  Update_Choice_Point((CodePtr) Prolog_Predicate(GROUP_SOLUTIONS_ALT, 0),
		      0);

  all_sol_word = AB(B, 0);
  gl_key_word = AB(B, 1);
  sol_word = AB(B, 2);

  word = Group(all_sol_word, gl_key_word, &key_word);
  if (word == NOT_A_WAM_WORD)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
      AB(B, 0) = word;
#if 0 /* the following data is unchanged */
      AB(B, 1) = gl_key_word;
      AB(B, 2) = sol_word;
#endif
    }

  Unify(key_word, gl_key_word);
  return Unify(sol_word, all_sol_word);
}




/*-------------------------------------------------------------------------*
 * GROUP                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Group(WamWord all_sol_word, WamWord gl_key_word, WamWord *key_adr)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *lst_adr, *prev_lst_adr;
  WamWord key_word, key_word1;

  DEREF(all_sol_word, word, tag_mask);

  lst_adr = UnTag_LST(word);
  DEREF(Car(lst_adr), word, tag_mask);	/* term of the form Key-Value */
  adr = UnTag_STC(word);
  *key_adr = key_word = Arg(adr, 0);

  for (;;)
    {				/* Arg(adr,1) cannot be a Dont_Separate_Tag */
      Car(lst_adr) = Arg(adr, 1);

      prev_lst_adr = lst_adr;
      DEREF(Cdr(lst_adr), word, tag_mask);
      if (word == NIL_WORD)
	return NOT_A_WAM_WORD;

      prev_lst_adr = lst_adr;
      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask); /* term of the form Key-Value */
      adr = UnTag_STC(word);
      key_word1 = Arg(adr, 0);

      if (Term_Compare(key_word, key_word1) != 0)
	break;
    }

  all_sol_word = Cdr(prev_lst_adr);
  Cdr(prev_lst_adr) = NIL_WORD;

  return all_sol_word;
}
