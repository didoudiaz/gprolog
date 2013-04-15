/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : g_var_inl_c.c                                                   *
 * Descr.: global variable (inline) management - C part                    *
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

#define OBJ_INIT G_Var_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define G_INITIAL_VALUE            Tag_INT(0)


#define G_ARRAY                    0
#define G_ARRAY_AUTO               1
#define G_ARRAY_EXTEND             2

#define G_IMPOSSIBLE_SIZE          ((unsigned int) -1 >> 1)
#define MAX_AUTO_SIZE              (1 << 20)



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct gundo *PGUndo;

typedef struct			/* Global variable element        */
{				/* ------------------------------ */
  int size;			/* <0:-array dim, 0:link, >0:copy */
  WamWord val;			/* ptr to GVarElt or term or adr  */
  PGUndo undo;			/* ptr to 1st undo for this elem  */
}
GVarElt;




typedef struct gundo		/* Undo record                    */
{				/* ------------------------------ */
  GVarElt *g_elem;		/* elem to restore (NULL=invalid) */
  int save_size;		/* size to restore                */
  WamWord save_val;		/* value to restore               */
  PGUndo next;			/* chain to next undo entry       */
  PGUndo prev;			/* chain to previous undo entry   */
} GUndo;



typedef struct			/* Target designator record       */
{				/* ------------------------------ */
  GVarElt *g_elem;		/* element                        */
  WamWord *g_arg;		/* ptr to sub-term (or NULL)      */
} GTarget;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static GTarget g_target;

static int atom_g_array;
static int atom_g_array_auto;
static int atom_g_array_extend;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void G_Assign(WamWord gvar_word, WamWord gval_word, Bool backtrack,
		     Bool copy);

static void G_Assign_Element(GVarElt *g_elem, WamWord gval_word,
			     Bool backtrack, Bool copy);

static void G_Assign_Arg(GVarElt *g_elem, WamWord *g_arg, WamWord word);

static void G_Assign_Array(GVarElt *g_elem, WamWord *stc_adr, int array_op,
			   Bool backtrack, Bool copy);

static GVarElt *G_Alloc_Array(GVarElt *g_elem, int new_size, Bool backtrack);

static GTarget *Get_Target_From_Gvar(WamWord gvar_word);

static GTarget *Get_Target_From_Selector(WamWord *stc_adr);

static WamWord *Get_Term_Addr_From_Target(GTarget *gt);

static WamWord *Get_Int_Addr_From_Gvar(WamWord gvar_word);

static PlLong Get_Int_From_Gvar(WamWord gvar_word);

static PlLong Get_Int_From_Word(WamWord start_word);

static void G_Free_Element(GVarElt *g_elem, Bool reinit_undo);

static void G_Copy_Element(GVarElt *dst_g_elem, GVarElt *src_g_elem);

static void G_Trail_For_Backtrack(GVarElt *g_elem, int save_size,
				  WamWord save_val);

static void G_Untrail(int n, WamWord *arg_frame);

static Bool G_Read(WamWord gvar_word, WamWord gval_word);

static Bool G_Read_Element(GVarElt *g_elem, WamWord gval_word);

static Bool G_Array_Size(WamWord gvar_word, WamWord size_word);

static Bool G_Inc_Dec(WamWord gvar_word, int inc,
		      WamWord old_word, WamWord new_word);

static void G_Set_Bit(WamWord gvar_word, WamWord bit_word);

static void G_Reset_Bit(WamWord gvar_word, WamWord bit_word);

static Bool G_Test_Set_Bit(WamWord gvar_word, WamWord bit_word);

static Bool G_Test_Reset_Bit(WamWord gvar_word, WamWord bit_word);




/*-------------------------------------------------------------------------*
 * G_VAR_INITIALIZER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Var_Initializer(void)
{
  atom_g_array = Pl_Create_Atom("g_array");
  atom_g_array_auto = Pl_Create_Atom("g_array_auto");
  atom_g_array_extend = Pl_Create_Atom("g_array_extend");
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_ASSIGN                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Assign(WamWord x, WamWord y)
{
  Pl_Set_C_Bip_Name("g_assign", 2);
  G_Assign(x, y, FALSE, TRUE);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_ASSIGNB                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Assignb(WamWord x, WamWord y)
{
  Pl_Set_C_Bip_Name("g_assignb", 2);
  G_Assign(x, y, TRUE, TRUE);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_LINK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Link(WamWord x, WamWord y)
{
  Pl_Set_C_Bip_Name("g_link", 2);
  G_Assign(x, y, TRUE, FALSE);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_READ                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Read(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_read", 2);
  res = G_Read(x, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_ARRAY_SIZE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Array_Size(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_array_size", 2);
  res = G_Array_Size(x, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_INC                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Inc(WamWord x)
{
  Pl_Set_C_Bip_Name("g_inc", 1);
  G_Inc_Dec(x, 1, NOT_A_WAM_WORD, NOT_A_WAM_WORD);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_INCO                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Inco(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_inco", 2);
  res = G_Inc_Dec(x, 1, y, NOT_A_WAM_WORD);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_INC_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Inc_2(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_inc", 2);
  res = G_Inc_Dec(x, 1, NOT_A_WAM_WORD, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_INC_3                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Inc_3(WamWord x, WamWord y, WamWord z)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_inc", 3);
  res = G_Inc_Dec(x, 1, y, z);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_DEC                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Dec(WamWord x)
{
  Pl_Set_C_Bip_Name("g_dec", 1);
  G_Inc_Dec(x, -1, NOT_A_WAM_WORD, NOT_A_WAM_WORD);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_DECO                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Deco(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_deco", 2);
  res = G_Inc_Dec(x, -1, y, NOT_A_WAM_WORD);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_DEC_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Dec_2(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_dec", 2);
  res = G_Inc_Dec(x, -1, NOT_A_WAM_WORD, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_DEC_3                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Dec_3(WamWord x, WamWord y, WamWord z)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_dec", 3);
  res = G_Inc_Dec(x, -1, y, z);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_SET_BIT                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Set_Bit(WamWord x, WamWord y)
{
  Pl_Set_C_Bip_Name("g_set_bit", 2);
  G_Set_Bit(x, y);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_RESET_BIT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Blt_G_Reset_Bit(WamWord x, WamWord y)
{
  Pl_Set_C_Bip_Name("g_reset_bit", 2);
  G_Reset_Bit(x, y);
  Pl_Unset_C_Bip_Name();
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_TEST_SET_BIT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Test_Set_Bit(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_test_set_bit", 2);
  res = G_Test_Set_Bit(x, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_G_TEST_RESET_BIT                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_G_Test_Reset_Bit(WamWord x, WamWord y)
{
  Bool res;

  Pl_Set_C_Bip_Name("g_test_reset_bit", 2);
  res = G_Test_Reset_Bit(x, y);
  Pl_Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * Global variable management                                              *
 *                                                                         *
 * A global variable allows the user to associate an information to an atom*
 * There are 3 types of information (2 basic types + 1 constructor):       *
 *                                                                         *
 *   - copy of a term,   builtin: g_assign[b](Gvar, Term)                  *
 *   - link to a term,   builtin: g_link(Gvar, Term)                       *
 *   - array of k infos, builtin: g_{assign[b]/link}(Gvar, g_array(...))   *
 *                                                                         *
 * The assignments can be backtrackble (g_assignb/g_link) or not (g_assign)*
 * (backtrackable = assignments are undone when backtracking occurs).      *
 *                                                                         *
 * Internal represention:                                                  *
 *                                                                         *
 * An information has a type GVarElt which is a structure with 3 fields    *
 * 'size' (indicating the type of the element), 'val' and 'undo':          *
 *                                                                         *
 * size<0: an array of -size + 1 elements,                                 *
 *         val (GVarElt *) points the first element.                       *
 *         after the -size elements there is an additional elem whose size:*
 *         = G_IMPOSSIBLE_SIZE to indicate a non-auto extendible array     *
 *         != G_IMPOSSIBLE_SIZE indicate the elem to initialize new elems  *
 *                                                                         *
 * size=0: a link to a term,                                               *
 *         val (WamWord) is the staring word of the term.                  *
 *                                                                         *
 * size>0: a copy of a term whose size is 'size',                          *
 *         val (WamWord *) is the address of the copy of the term          *
 *         (space for the copy obtained by malloc).                        *
 *                                                                         *
 * 'undo': points to an undo record which will be activated at backtracking*
 *         when untrail occurs. Basically an undo record contains the      *
 *         address of the GVarElt to restore and the data to restore       *
 *         (size and val). Since several undo operation can be attached to *
 *         a GVarElt, the undo records are chained (dobly linked chain).   *
 *         When an undo entry becomes invalid, the address of the GVarElt  *
 *         to restore is set to NULL, e.g:                                 *
 *         g_assign(t,1), g_assignb(t,2), g_assign(t,3).                   *
 *         after g_assignb(t,2) there is an undo record for t which is     *
 *         invalidated when g_assign(t,3) occurs.                          *
 *         The undo records are mallocated but could be put in the TRAIL   *
 *         as the frame argument for the function call (TFC).              *
 *-------------------------------------------------------------------------*/




/*-------------------------------------------------------------------------*
 * G_ASSIGN                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Assign(WamWord gvar_word, WamWord gval_word, Bool backtrack, Bool copy)
{
  GTarget *gt = Get_Target_From_Gvar(gvar_word);
  GVarElt *g_elem = gt->g_elem;
  WamWord *g_arg = gt->g_arg;

  if (g_arg != NULL)		/* arg selector given */
    {
      if (backtrack)
	Pl_Err_Domain(pl_domain_g_argument_selector, gvar_word);
      G_Assign_Arg(g_elem, g_arg, gval_word);
    }
  else
    G_Assign_Element(g_elem, gval_word, backtrack, copy);
}




/*-------------------------------------------------------------------------*
 * G_ASSIGN_ELEMENT                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Assign_Element(GVarElt *g_elem, WamWord gval_word, Bool backtrack,
		 Bool copy)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int size;
  int atom;
  int save_size;
  WamWord save_val;
  int array_op;

  save_size = g_elem->size;
  save_val = g_elem->val;

  DEREF(gval_word, word, tag_mask);

  if (tag_mask != TAG_STC_MASK)
    goto not_an_array;

  adr = UnTag_STC(word);
  atom = Functor(adr);

  if (atom == atom_g_array)
    array_op = G_ARRAY;
  else if (atom == atom_g_array_auto)
    array_op = G_ARRAY_AUTO;
  else if (atom == atom_g_array_extend)
    array_op = G_ARRAY_EXTEND;
  else goto not_an_array;

				/* an array */
  G_Assign_Array(g_elem, adr, array_op, backtrack, copy);
  goto finish;


 not_an_array:

  if (!backtrack)
    G_Free_Element(g_elem, TRUE);

  if (!copy || tag_mask == TAG_ATM_MASK || tag_mask == TAG_INT_MASK)
    {				/* a link */
      if (tag_mask == TAG_REF_MASK && Is_A_Local_Adr(adr = UnTag_REF(word)))
	Globalize_Local_Unbound_Var(adr, word);

      g_elem->size = 0;
      Do_Copy_Of_Word(tag_mask, word);
      g_elem->val = word;
      goto finish;
    }

				/* a copy */
  size = Pl_Term_Size(word);

  adr = (WamWord *) Malloc(size * sizeof(WamWord));

  g_elem->size = size;
  g_elem->val = (WamWord) adr;

  Pl_Copy_Term(adr, &word);

 finish:

  if (backtrack)
    G_Trail_For_Backtrack(g_elem, save_size, save_val);
}




/*-------------------------------------------------------------------------*
 * G_ASSIGN_ARG                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Assign_Arg(GVarElt *g_elem, WamWord *g_arg, WamWord word)
{
  WamWord *adr;
  int size;
  GUndo *u;

  if (Pl_Term_Size(*g_arg) == 1 && Pl_Term_Size(word) == 1)
    {
      Pl_Copy_Term(g_arg, &word);
				/* simulate the G_Free_Element */
      for(u = g_elem->undo; u; u = u->next)
	u->g_elem = NULL;	/* invalidate this entry */
      g_elem->undo = NULL;

      return;
    }

				/* similar to g_read + g_assign */

  *g_arg = word;		/* set the argument */
  Pl_Copy_Term(H, (WamWord *) g_elem->val);

  G_Free_Element(g_elem, TRUE);

  size = Pl_Term_Size(*H);

  adr = (WamWord *) Malloc(size * sizeof(WamWord));

  g_elem->size = size;
  g_elem->val = (WamWord) adr;

  Pl_Copy_Contiguous_Term(adr, H);
}




/*-------------------------------------------------------------------------*
 * G_ASSIGN_ARRAY                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Assign_Array(GVarElt *g_elem, WamWord *stc_adr, int array_op,
	       Bool backtrack, Bool copy)
{
  WamWord word, tag_mask;
  int arity;
  Bool same_init_value;
  WamWord init_word;
  WamWord lst_word;
  PlLong new_size, size;
  GVarElt *p;
  int i;

  arity = Arity(stc_adr);

  DEREF(Arg(stc_adr, 0), word, tag_mask);
  new_size = (tag_mask == TAG_LST_MASK) ? Pl_List_Length(word) :
    UnTag_INT(word);

  if (!(new_size > 0 && ((tag_mask == TAG_INT_MASK && arity <= 2) ||
			 (tag_mask == TAG_LST_MASK && arity == 1))))
    Pl_Err_Domain(pl_domain_g_array_index, Tag_STC(stc_adr));

  if (tag_mask == TAG_INT_MASK)
    {
      same_init_value = TRUE;
      init_word = (arity == 1) ? G_INITIAL_VALUE : Arg(stc_adr, 1);
    }
  else
    {
      same_init_value = FALSE;
      lst_word = word;
    }

  if (array_op == G_ARRAY_EXTEND && g_elem->size >= 0)
    array_op = G_ARRAY;

  if (array_op != G_ARRAY_EXTEND && !backtrack)
    {
      G_Free_Element(g_elem, TRUE);
      g_elem->size = 0;
    }

  size = -g_elem->size;

  p = G_Alloc_Array(g_elem, new_size, backtrack);

  if (array_op == G_ARRAY_EXTEND)
    {
      if (!same_init_value)
	for(i = 0; i < size; i++) /* skip size 1st elems of list */
	  {
	    Pl_Get_List(lst_word);
	    init_word = Pl_Unify_Variable();
	    lst_word = Pl_Unify_Variable();
	  }
      i = size;
      p += size;
    }
  else
    i = 0;

  for (; i < new_size; i++)
    {
      if (!same_init_value)
	{
	  Pl_Get_List(lst_word);
	  init_word = Pl_Unify_Variable();
	  lst_word = Pl_Unify_Variable();
	}

      p->size = 0;
      p->val = G_INITIAL_VALUE;
      p->undo = NULL;
      G_Assign_Element(p++, init_word, FALSE, copy);
    }

  if (array_op == G_ARRAY_AUTO)
    {
      if (!same_init_value)
	init_word = G_INITIAL_VALUE;
      p->size = 0;
      p->val = G_INITIAL_VALUE;
      p->undo = NULL;
      G_Assign_Element(p, init_word, FALSE, copy);
    }

}




/*-------------------------------------------------------------------------*
 * G_ALLOC_ARRAY                                                           *
 *                                                                         *
 * Set in g_elem an array for new_size elements.                           *
 * If g_elem does not already contain an array a space is mallocated and   *
 * the last elem is initilized with G_IMPOSSIBLE_SIZE.                     *
 *                                                                         *
 * If g_elem containts an array, its values should be reflected to the new *
 * array. In absence of backtrackable assignment we perform a realloc else *
 * a malloc + copy. In both cases the last element is set (copy of old one)*
 * In case of backtrable assignment (i.e. malloc+copy), the new elements   *
 * should not have undo records (G_Copy_Element sets them to NULL).        *
 * While in case of realloc, the undo records have to been adjusted to     *
 * point to new cells (realloc can return a different starting address).   *
 *-------------------------------------------------------------------------*/
static GVarElt *
G_Alloc_Array(GVarElt *g_elem, int new_size, Bool backtrack)
{
  GVarElt *p, *p_new_end, *src, *dst;
  GUndo *u;
  int old_size, i;

  old_size = -g_elem->size;
  src = (GVarElt *) g_elem->val;

  if (old_size <= 0 || backtrack)
    p = (GVarElt *) Malloc((new_size + 1) * sizeof(GVarElt));
  else
    p = (GVarElt *) Realloc((char *) src, (new_size + 1) * sizeof(GVarElt));

  p_new_end = p + new_size;

  if (old_size <= 0)
    {				/* init last elem */
      p_new_end->size = G_IMPOSSIBLE_SIZE;
      p_new_end->val = (WamWord) NULL;
      p_new_end->undo = NULL;
      goto finish;
    }
				/* there is an array at *src */
  if (backtrack)
    {
      dst = p;
      for (i = 0; i < old_size; i++)
	G_Copy_Element(dst++, src++);

				/* copy last elem */
      if (src->size != G_IMPOSSIBLE_SIZE)
	G_Copy_Element(p_new_end, src);
      else
	*p_new_end = *src;
      goto finish;
    }
				/* a realloc */
  dst = p;
  for (i = 0; i < old_size; i++)
    {
      for (u = dst->undo; u; u = u->next)
	u->g_elem = dst;
      dst++;
    }

  *p_new_end = *dst;		/* copy last elem */

 finish:
  g_elem->size = -new_size;
  g_elem->val = (WamWord) p;

  return p;
}




/*-------------------------------------------------------------------------*
 * GET_TARGET_FROM_GVAR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static GTarget *
Get_Target_From_Gvar(WamWord gvar_word)
{
  WamWord word, tag_mask;
  WamWord word1;
  int atom;
  int arity;
  WamWord *arg_adr;
  GVarElt *g_elem, *g_end;
  GVarElt *p;
  int i, j, size;
  int new_size;
  PlLong index;
  GTarget *gt = &g_target;

  arg_adr = Pl_Rd_Callable_Check(gvar_word, &atom, &arity);

  if (atom == ATOM_CHAR('-') && arity == 2)
    return Get_Target_From_Selector(arg_adr - OFFSET_ARG);

  g_elem = (GVarElt *) pl_atom_tbl[atom].info;

  if (g_elem == NULL)
    {
      g_elem = (GVarElt *) Malloc(sizeof(GVarElt)); /* NB: never recovered */
      g_elem->size = 0;
      g_elem->val = G_INITIAL_VALUE;
      g_elem->undo = NULL;
      pl_atom_tbl[atom].info = g_elem;
    }

  if (arity > 0 && g_elem->size >= 0)
    {
    error:
      Pl_Err_Domain(pl_domain_g_array_index, gvar_word);
    }

  for (i = 0; i < arity; i++)
    {
      size = g_elem->size;

      word1 = *arg_adr;
      DEREF(word1, word, tag_mask);

      if (tag_mask != TAG_INT_MASK) /* follow the indirection */
	word = *Get_Int_Addr_From_Gvar(word);

      index = UnTag_INT(word);

      if (size >= 0 || index < 0)
	goto error;

      size = -size;

      if (index >= size)
	{
	  p = (GVarElt *) (g_elem->val);
	  g_end = p + size;

	  if (g_end->size == G_IMPOSSIBLE_SIZE || index > MAX_AUTO_SIZE)
	    goto error;
				/* auto expand */

	  for(new_size = 1; new_size <= index; new_size <<= 1)
	    ;

	  p = G_Alloc_Array(g_elem, new_size, FALSE);

	  g_end = p + new_size;
	  p += size;

	  for (j = size; j < new_size; j++) /* init new cells */
	    G_Copy_Element(p++, g_end);
	}

      g_elem = (GVarElt *) (g_elem->val) + index;
      arg_adr++;
    }

  gt->g_elem = g_elem;
  gt->g_arg = NULL;
  return gt;
}




/*-------------------------------------------------------------------------*
 * GET_TARGET_FROM_SELECTOR                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static GTarget *
Get_Target_From_Selector(WamWord *stc_adr)
{
  WamWord word, tag_mask;
  WamWord *adr, word1;
  int arg_no;
  GTarget *gt;

  gt = Get_Target_From_Gvar(Arg(stc_adr, 0));
  arg_no = Get_Int_From_Word(Arg(stc_adr, 1));
  adr = Get_Term_Addr_From_Target(gt);
  if (adr == NULL)		/* an array */
    goto error;

  word1 = *adr;

  DEREF(word1, word, tag_mask);

  if (tag_mask == STC)
    {
      adr = UnTag_STC(word);
      if (arg_no < 1 || arg_no > Arity(adr))
	goto error;

      gt->g_arg = &Arg(adr, arg_no - 1);
    }
  else if (tag_mask == LST)
    {
      adr = UnTag_LST(word);
      if (arg_no < 0)
	goto error;
      while(--arg_no)
	{
	  DEREF(Cdr(adr), word, tag_mask);
	  if (tag_mask != LST)
	    goto error;
	  adr = UnTag_LST(word);
	}
      gt->g_arg = &Car(adr);
    }
  else
    {
    error:
      Pl_Err_Domain(pl_domain_g_argument_selector, Tag_STC(stc_adr));
    }

  return gt;
}




/*-------------------------------------------------------------------------*
 * GET_TERM_ADDR_FROM_TARGET                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord *
Get_Term_Addr_From_Target(GTarget *gt)
{
  GVarElt *g_elem = gt->g_elem;

  if (gt->g_arg)
    return gt->g_arg;

  if (g_elem->size < 0)
    return NULL;

  if (g_elem->size == 0)
    return (WamWord *) &g_elem->val;

  return (WamWord *) g_elem->val;
}




/*-------------------------------------------------------------------------*
 * GET_INT_ADDR_FROM_GVAR                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord *
Get_Int_Addr_From_Gvar(WamWord gvar_word)
{
  GTarget save_g_target = g_target; /* save for cross-recursion */
  GTarget *gt = Get_Target_From_Gvar(gvar_word);
  WamWord *adr = Get_Term_Addr_From_Target(gt);

  g_target = save_g_target;
				/* should be dereferenced */

  if (adr == NULL)		/* an array */
    Pl_Err_Type(pl_type_integer, Tag_ATM(atom_g_array));

  if (Tag_Mask_Of(*adr) != TAG_INT_MASK)
     Pl_Err_Type(pl_type_integer, *adr);

  return adr;
}




/*-------------------------------------------------------------------------*
 * GET_INT_FROM_GVAR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong
Get_Int_From_Gvar(WamWord gvar_word)
{
  return *Get_Int_Addr_From_Gvar(gvar_word);
}




/*-------------------------------------------------------------------------*
 * GET_INT_FROM_WORD                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong
Get_Int_From_Word(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  if (tag_mask != TAG_INT_MASK) /* follow the indirection */
    word = *Get_Int_Addr_From_Gvar(word);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * G_FREE_ELEMENT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Free_Element(GVarElt *g_elem, Bool reinit_undo)
{
  int size;
  GVarElt *p;
  GUndo *u;
  int i;

  if (reinit_undo)
    {
      for(u = g_elem->undo; u; u = u->next)
	u->g_elem = NULL;	/* invalidate this entry */
      g_elem->undo = NULL;
    }

  size = g_elem->size;

  if (size == 0)		/* a link: nothing */
    return;

  if (size < 0)			/* an array: recursively free elts */
    {
      size = -size;
      p = (GVarElt *) (g_elem->val);

      for (i = 0; i < size; i++)
	G_Free_Element(p++, reinit_undo);

      if (p->size != G_IMPOSSIBLE_SIZE)	/* last elem */
	G_Free_Element(p, reinit_undo);
    }

				/* a copy or an array: free */
  Free((char *) g_elem->val);
}




/*-------------------------------------------------------------------------*
 * G_COPY_ELEMENT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Copy_Element(GVarElt *dst_g_elem, GVarElt *src_g_elem)
{
  WamWord *adr;
  GVarElt *p;
  int size;
  int i;

  size = dst_g_elem->size = src_g_elem->size;
  dst_g_elem->undo = NULL;

  if (size == 0)		/* a link: copy */
    {
      dst_g_elem->val = src_g_elem->val;
      return;
    }

  if (size < 0)			/* an array: alloc + recursively copy elts */
    {
      size = -size;

      p = (GVarElt *) Malloc((size + 1) * sizeof(GVarElt));

      dst_g_elem->val = (WamWord) p;
      dst_g_elem = p;

      src_g_elem = (GVarElt *) (src_g_elem->val);

      for (i = 0; i < size; i++)
	G_Copy_Element(dst_g_elem++, src_g_elem++);

      if (src_g_elem->size == G_IMPOSSIBLE_SIZE) /* last elem */
	*dst_g_elem = *src_g_elem;
      else
	G_Copy_Element(dst_g_elem, src_g_elem);

      return;
    }

				/* a copy: alloc + copy */

  adr = (WamWord *) Malloc(size * sizeof(WamWord));

  dst_g_elem->val = (WamWord) adr;

  Pl_Copy_Contiguous_Term(adr, (WamWord *) src_g_elem->val);
}




/*-------------------------------------------------------------------------*
 * G_TRAIL_FOR_BACKTRACK                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Trail_For_Backtrack(GVarElt *g_elem, int save_size, WamWord save_val)
{
  WamWord arg_frame[1];
  GUndo *u = (GUndo *) Malloc(sizeof(GUndo));

  u->g_elem = g_elem;
  u->save_size = save_size;
  u->save_val = save_val;
  u->next = g_elem->undo;
  u->prev = NULL;

  if (u->next)
    u->next->prev = u;

  g_elem->undo = u;

  arg_frame[0] = (WamWord) u;
  Trail_FC(G_Untrail, 1, arg_frame);
}




/*-------------------------------------------------------------------------*
 * G_UNTRAIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Untrail(int n, WamWord *arg_frame)
{
  GUndo *u = (GUndo *) arg_frame[0];
  GVarElt *g_elem = u->g_elem;

  if (g_elem)			/* valid entry ? */
    {
      G_Free_Element(g_elem, FALSE);
      g_elem->size = u->save_size;
      g_elem->val = u->save_val;
    }

				/* remove undo record */
  if (u->next)
    u->next->prev = u->prev;

  if (u->prev)
    u->prev->next = u->next;
  else if (g_elem)
    g_elem->undo = u->next;

  Free(u);
}




/*-------------------------------------------------------------------------*
 * G_READ                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Read(WamWord gvar_word, WamWord gval_word)
{
  GTarget *gt = Get_Target_From_Gvar(gvar_word);
  GVarElt *g_elem = gt->g_elem;
  WamWord *g_arg = gt->g_arg;
  WamWord word;

  if (g_arg != NULL)
    {
      Pl_Copy_Term(H, g_arg);
      word = *H;
      H += Pl_Term_Size(word);
      return Pl_Unify(word, gval_word);
    }

  return G_Read_Element(g_elem, gval_word);
}




/*-------------------------------------------------------------------------*
 * G_READ_ELEMENT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Read_Element(GVarElt *g_elem, WamWord gval_word)
{
  WamWord word;
  int size = g_elem->size;
  GVarElt *p;
  int i;


  if (size == 0)		/* a link: unify */
    return Pl_Unify(g_elem->val, gval_word);

  if (size > 0)			/* a copy: copy+unify */
    {
      Pl_Copy_Contiguous_Term(H, (WamWord *) g_elem->val);
      word = *H;
      H += size;
      return Pl_Unify(word, gval_word);
    }

				/* an array: unify with g_array([elt,...]) */
  size = -size;
  p = (GVarElt *) g_elem->val;

  if (!Pl_Get_Structure(atom_g_array, 1, gval_word))
    return FALSE;

  gval_word = Pl_Unify_Variable();

  for (i = 0; i < size; i++)
    {
      if (!Pl_Get_List(gval_word))
	return FALSE;

      word = Pl_Unify_Variable();
      gval_word = Pl_Unify_Variable();

      if (!G_Read_Element(p++, word))
	return FALSE;
    }

  return Pl_Get_Nil(gval_word);
}




/*-------------------------------------------------------------------------*
 * G_ARRAY_SIZE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Array_Size(WamWord gvar_word, WamWord size_word)
{
  GTarget *gt = Get_Target_From_Gvar(gvar_word);
  GVarElt *g_elem = gt->g_elem;
  WamWord *g_arg = gt->g_arg;
  int size;

  Pl_Check_For_Un_Integer(size_word);

  size = g_elem->size;

  return g_arg == NULL && size < 0 && Pl_Get_Integer(-size, size_word);
}




/*-------------------------------------------------------------------------*
 * G_INC_DEC                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Inc_Dec(WamWord gvar_word, int inc, WamWord old_word, WamWord new_word)
{
  WamWord *adr;
  PlLong old, new;

  if (old_word != NOT_A_WAM_WORD)
    Pl_Check_For_Un_Integer(old_word);

  if (new_word != NOT_A_WAM_WORD)
    Pl_Check_For_Un_Integer(new_word);


  adr = Get_Int_Addr_From_Gvar(gvar_word);

  old = UnTag_INT(*adr);
  new = old + inc;

  if (old_word != NOT_A_WAM_WORD && !Pl_Get_Integer(old, old_word))
    return FALSE;

  *adr = Tag_INT(new);		/* increment now - cf specif in doc */

  if (new_word != NOT_A_WAM_WORD && !Pl_Get_Integer(new, new_word))
    return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * G_SET_BIT                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Set_Bit(WamWord gvar_word, WamWord bit_word)
{
  WamWord *adr;
  int bit = Pl_Rd_Positive_Check(bit_word) % VALUE_SIZE;
  PlULong mask;

  adr = Get_Int_Addr_From_Gvar(gvar_word);
  mask = 1 << (bit + TAG_SIZE_LOW);
  *adr |= mask;
}




/*-------------------------------------------------------------------------*
 * G_RESET_BIT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Reset_Bit(WamWord gvar_word, WamWord bit_word)
{
  WamWord *adr;
  int bit = Pl_Rd_Positive_Check(bit_word) % VALUE_SIZE;
  PlULong mask;

  adr = Get_Int_Addr_From_Gvar(gvar_word);
  mask = 1 << (bit + TAG_SIZE_LOW);
  *adr &= ~mask;
}




/*-------------------------------------------------------------------------*
 * G_TEST_SET_BIT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Test_Set_Bit(WamWord gvar_word, WamWord bit_word)
{
  int bit = Pl_Rd_Positive_Check(bit_word) % VALUE_SIZE;
  PlULong val, mask;

  val = Get_Int_From_Gvar(gvar_word);
  mask = 1 << (bit + TAG_SIZE_LOW);
  return (val & mask) != 0;
}




/*-------------------------------------------------------------------------*
 * G_TEST_RESET_BIT                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Test_Reset_Bit(WamWord gvar_word, WamWord bit_word)
{
  int bit = Pl_Rd_Positive_Check(bit_word) % VALUE_SIZE;
  PlULong mask, val;

  val = Get_Int_From_Gvar(gvar_word);
  mask = 1 << (bit + TAG_SIZE_LOW);
  return (val & mask) == 0;
}

