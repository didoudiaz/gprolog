/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : g_var_inl_c.c                                                   *
 * Descr.: global variable (inline) management - C part                    *
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

#define OBJ_INIT G_Var_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define G_VAR_INITIAL_VALUE        0	/* i.e. Tag_Value(INT,0) */




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Glabal Variable Element        */
{				/* ------------------------------ */
  int size;			/* <0:-array dim 0:link, >0:copy  */
  WamWord val;			/* ptr to GVarElt or term or adr  */
}
GVarElt;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static GVarElt g_var_tbl[MAX_ATOM];

static int atom_g_array;
static int atom_g_array_extend;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static
  Bool G_Assign(WamWord gvar_word, WamWord gval_word,
		Bool backtrack, Bool copy);

static
  Bool G_Assign_Element(GVarElt *g_elem, WamWord gval_word,
			Bool backtrack, Bool copy);

static
  Bool G_Assign_Array(GVarElt *g_elem, WamWord *stc_adr,
		      Bool extend, Bool copy);

static GVarElt *G_Get_Element(WamWord gvar_word);

static void G_Free_Element(GVarElt *g_elem);

static void G_Copy_Element(GVarElt *dst_g_elem, GVarElt *src_g_elem);

static void G_Untrail(void);

static Bool G_Read(WamWord gvar_word, WamWord gval_word);

static Bool G_Read_Element(GVarElt *g_elem, WamWord gval_word);

static Bool G_Array_Size(WamWord gvar_word, WamWord size_word);




/*-------------------------------------------------------------------------*
 * G_VAR_INITIALIZER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Var_Initializer(void)
{
  atom_g_array = Create_Atom("g_array");
  atom_g_array_extend = Create_Atom("g_array_extend");

  /* g_var_tbl already initialized to {0,0} */
  /* i.e size=0 and val=Tag_Value(INT,0)    */
}




/*-------------------------------------------------------------------------*
 * BLT_G_ASSIGN                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Blt_G_Assign(WamWord x, WamWord y)
{
  Bool res;

  Set_C_Bip_Name("g_assign", 2);
  res = G_Assign(x, y, FALSE, TRUE);
  Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * BLT_G_ASSIGNB                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Blt_G_Assignb(WamWord x, WamWord y)
{
  Bool res;

  Set_C_Bip_Name("g_assignb", 2);
  res = G_Assign(x, y, TRUE, TRUE);
  Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * BLT_G_LINK                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Blt_G_Link(WamWord x, WamWord y)
{
  Bool res;

  Set_C_Bip_Name("g_link", 2);
  res = G_Assign(x, y, TRUE, FALSE);
  Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * BLT_G_READ                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Blt_G_Read(WamWord x, WamWord y)
{
  Bool res;

  Set_C_Bip_Name("g_read", 2);
  res = G_Read(x, y);
  Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * BLT_G_ARRAY_SIZE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Blt_G_Array_Size(WamWord x, WamWord y)
{
  Bool res;

  Set_C_Bip_Name("g_array_size", 2);
  res = G_Array_Size(x, y);
  Unset_C_Bip_Name();
  return res;
}




/*-------------------------------------------------------------------------*
 * Global variable management                                              *
 *                                                                         *
 * A global variable allows the user to associate an information to an atom*
 * There are 3 types of information (2 basic types + 1 constructor):       *
 *                                                                         *
 *   - copy of a term,   builtin: g_assign[b](Gvar,Term)                   *
 *   - link to a term,   builtin: g_link(Gvar,Term)                        *
 *   - array of k infos, builtin: g_{assign[b]/link}(Gvar,g_array(...))    *
 *                                                                         *
 * The assignments can be backtrackble (g_assignb/g_link) or not (g_assign)*
 * (backtrackable = assignments are undone when backtracking occurs).      *
 *                                                                         *
 * Internal represention:                                                  *
 *                                                                         *
 * An information has a type GVarElt which is a structure with 2 fields    *
 * 'size' (indicationg the type of the element) and 'val':                 *
 *                                                                         *
 * size<0: an array of -size elements,                                     *
 *         val (GVarElt *) points the first element.                       *
 *                                                                         *
 * size=0: a link to a term,                                               *
 *         val (WamWord) is the staring word of the term.                  *
 *                                                                         *
 * size>0: a copy of a term whose size is 'size',                          *
 *         val (WamWord *) is the address of the copy of the term          *
 *         (space for the copy obtained by malloc).                        *
 *-------------------------------------------------------------------------*/




/*-------------------------------------------------------------------------*
 * G_ASSIGN                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Assign(WamWord gvar_word, WamWord gval_word, Bool backtrack, Bool copy)
{
  GVarElt *g_elem;

  if ((g_elem = G_Get_Element(gvar_word)) == NULL)
    return FALSE;

  return G_Assign_Element(g_elem, gval_word, backtrack, copy);
}




/*-------------------------------------------------------------------------*
 * G_ASSIGN_ELEMENT                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Assign_Element(GVarElt *g_elem, WamWord gval_word,
		 Bool backtrack, Bool copy)
{
  WamWord word, tag, *adr;
  int size;
  int size_base = 0;
  int atom;
  GVarElt save_g_elem;


  save_g_elem = *g_elem;

  Deref(gval_word, word, tag, adr);

  if (tag == STC)
    {
      adr = UnTag_STC(word);
      atom = Functor(adr);

      if (atom == atom_g_array || atom == atom_g_array_extend)	/* an array */
	{
	  if (!G_Assign_Array
	      (g_elem, adr, atom == atom_g_array_extend, copy))
	    return FALSE;

	  goto finish;
	}
    }



  if (!copy || tag == ATM || tag == INT)	/* a link */
    {
      if (tag == REF && Is_A_Local_Adr(adr))
	{
	  word = Tag_Value(REF, H);
	  Globalize_Local_Unbound_Var(adr);
	}

      g_elem->size = 0;
      g_elem->val = Make_Copy_Of_Word(tag, word);
      goto finish;
    }

  /* a copy */
  size = Term_Size(word);

  adr = (WamWord *) Malloc(size * sizeof(WamWord));

  g_elem->size = size + size_base;
  g_elem->val = (WamWord) adr;

  Copy_Term(adr, &word);



finish:

  if (backtrack)
    {
      Trail_Push(save_g_elem.val);	/* push frame (see G_Untrail) */
      Trail_Push(save_g_elem.size);
      Trail_Push(g_elem);
      Trail_FC(G_Untrail);
    }
  else
    G_Free_Element(&save_g_elem);


  return TRUE;
}




/*-------------------------------------------------------------------------*
 * G_GET_ELEMENT                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static GVarElt *
G_Get_Element(WamWord gvar_word)
{
  WamWord word, tag, *adr;
  WamWord word1;
  int atom;
  int arity;
  WamWord *arg_adr;
  GVarElt *g_elem;
  int i, size;
  int index;

  arg_adr = Rd_Callable_Check(gvar_word, &atom, &arity);

  g_elem = g_var_tbl + atom;

  for (i = 0; i < arity; i++)
    {
      size = g_elem->size;

      word1 = *arg_adr;
    deref:
      Deref(word1, word, tag, adr);

      if (tag != INT)
	{
	  word1 = Make_Self_Ref(H);
	  Global_Push(word1);
	  if (!G_Read(word, word1))
	    return NULL;

	  goto deref;
	}

      index = UnTag_INT(word);

      if (size >= 0 || (unsigned) index >= (unsigned) (-size))
	Pl_Err_Domain(domain_g_array_index, gvar_word);

      g_elem = (GVarElt *) (g_elem->val) + index;
      arg_adr++;
    }

  return g_elem;
}




/*-------------------------------------------------------------------------*
 * G_ASSIGN_ARRAY                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Assign_Array(GVarElt *g_elem, WamWord *stc_adr, Bool extend, Bool copy)
{
  WamWord word, tag, *adr;
  int arity;
  Bool same_init_value;
  WamWord init_word;
  WamWord lst_word;
  int src_size, new_size;
  GVarElt save_g_elem;
  GVarElt *p, *src_p;
  int i;

  arity = Arity(stc_adr);

  Deref(Arg(stc_adr, 0), word, tag, adr);
  new_size = (tag == LST) ? List_Length(word) : UnTag_INT(word);

  if (!
      (new_size > 0
       && ((tag == INT && arity <= 2) || (tag == LST && arity == 1))))
    Pl_Err_Domain(domain_g_array_index, Tag_Value(STC, stc_adr));

  if (tag == INT)
    {
      same_init_value = TRUE;
      init_word = (arity == 1) ? G_VAR_INITIAL_VALUE : Arg(stc_adr, 1);
    }
  else
    {
      same_init_value = FALSE;
      lst_word = word;
    }

  if (extend && g_elem->size < 0)
    {
      src_size = -g_elem->size;
      src_p = (GVarElt *) (g_elem->val);
    }
  else
    src_size = 0;


  save_g_elem = *g_elem;

  p = (GVarElt *) Malloc(new_size * sizeof(GVarElt));

  g_elem->size = -new_size;
  g_elem->val = (WamWord) p;


  for (i = 0; i < new_size; i++)
    {
      if (!same_init_value)
	{
	  Get_List(lst_word);
	  init_word = Unify_Variable();
	  lst_word = Unify_Variable();
	}

      if (src_size > 0)
	{
	  src_size--;
	  G_Copy_Element(p++, src_p++);
	}
      else
	{
	  p->size = 0;
	  p->val = G_VAR_INITIAL_VALUE;
	  if (!G_Assign_Element(p++, init_word, FALSE, copy))
	    {
	      Free((char *) g_elem->val);
	      *g_elem = save_g_elem;
	      return FALSE;
	    }
	}
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * G_FREE_ELEMENT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Free_Element(GVarElt *g_elem)
{
  int size;
  GVarElt *p;
  int i;

  size = g_elem->size;

  if (size == 0)		/* a link: nothing */
    return;

  if (size < 0)			/* an array: recursively free elts */
    {
      size = -size;
      p = (GVarElt *) (g_elem->val);

      for (i = 0; i < size; i++)
	G_Free_Element(p++);
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

  if (size == 0)		/* a link: copy */
    {
      dst_g_elem->val = src_g_elem->val;
      return;
    }

  if (size < 0)			/* an array: alloc + recursively copy elts */
    {
      size = -size;

      p = (GVarElt *) Malloc(size * sizeof(GVarElt));

      dst_g_elem->val = (WamWord) p;
      dst_g_elem = p;

      src_g_elem = (GVarElt *) (src_g_elem->val);

      for (i = 0; i < size; i++)
	G_Copy_Element(dst_g_elem++, src_g_elem++);

      return;
    }

  /* a copy: alloc + copy */

  adr = (WamWord *) Malloc(size * sizeof(WamWord));

  dst_g_elem->val = (WamWord) adr;

  Copy_Contiguous_Term(adr, (WamWord *) src_g_elem->val);
}




/*-------------------------------------------------------------------------*
 * G_UNTRAIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
G_Untrail(void)
{
  GVarElt *g_elem;

  g_elem = (GVarElt *) Trail_Pop;	/* pop frame (see G_Assign_Element) */
  G_Free_Element(g_elem);
  g_elem->size = Trail_Pop;
  g_elem->val = Trail_Pop;
}




/*-------------------------------------------------------------------------*
 * G_READ                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Read(WamWord gvar_word, WamWord gval_word)
{
  GVarElt *g_elem;

  if ((g_elem = G_Get_Element(gvar_word)) == NULL)
    return FALSE;

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
    return Unify(g_elem->val, gval_word);

  if (size > 0)			/* a copy: copy+unify */
    {
      Copy_Contiguous_Term(H, (WamWord *) g_elem->val);
      word = *H;
      H += size;
      return Unify(word, gval_word);
    }

  /* an array: unify with array([elt1,...]) */
  size = -size;
  p = (GVarElt *) g_elem->val;

  if (!Get_Structure(atom_g_array, 1, gval_word))
    return FALSE;

  gval_word = Unify_Variable();

  for (i = 0; i < size; i++)
    {
      if (!Get_List(gval_word))
	return FALSE;

      word = Unify_Variable();
      gval_word = Unify_Variable();

      if (!G_Read_Element(p++, word))
	return FALSE;
    }

  return Get_Nil(gval_word);
}




/*-------------------------------------------------------------------------*
 * G_ARRAY_SIZE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
G_Array_Size(WamWord gvar_word, WamWord size_word)
{
  GVarElt *g_elem;

  Check_For_Un_Integer(size_word);

  if ((g_elem = G_Get_Element(gvar_word)) == NULL || g_elem->size >= 0)
    return FALSE;

  return Get_Integer(-g_elem->size, size_word);
}
