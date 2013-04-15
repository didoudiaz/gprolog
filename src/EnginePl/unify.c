/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : unify.c                                                         *
 * Descr.: unification part                                                *
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


static Bool Check_If_Var_Occurs(WamWord *var_adr, WamWord term_word);



/*-------------------------------------------------------------------------*
 * This file is not compiled separately but included twice by wam_inst.c:  *
 *    - to define the Unify function (classical unification).              *
 *    - to define the Unify_Occurs_Check function (+ occurs check).        *
 *-------------------------------------------------------------------------*/
Bool FC
UNIFY_FCT_NAME(WamWord start_u_word, WamWord start_v_word)
{
  WamWord u_word, u_tag_mask;
  WamWord v_word, v_tag_mask;
  WamWord *u_adr, *v_adr;
  int i;

 terminal_rec:

  DEREF(start_u_word, u_word, u_tag_mask);
  DEREF(start_v_word, v_word, v_tag_mask);

  if (u_tag_mask == TAG_REF_MASK)
    {
      u_adr = UnTag_REF(u_word);
      if (v_tag_mask == TAG_REF_MASK)
	{
	  v_adr = UnTag_REF(v_word);

	  if (u_adr > v_adr)
	    Bind_UV(u_adr, Tag_REF(v_adr));
	  else if (v_adr > u_adr)
	    Bind_UV(v_adr, Tag_REF(u_adr));
	}
      else
	{
#ifdef OCCURS_CHECK
	  if (!Is_A_Local_Adr(u_adr) &&	/* no binding from heap to local */
	      Check_If_Var_Occurs(u_adr, v_word))
	    return FALSE;
#endif
	  Do_Copy_Of_Word(v_tag_mask, v_word);
	  Bind_UV(u_adr, v_word);
	}

      return TRUE;
    }


  if (v_tag_mask == TAG_REF_MASK)
    {
      v_adr = UnTag_REF(v_word);

#ifdef OCCURS_CHECK
      if (!Is_A_Local_Adr(v_adr) &&	/* no binding from heap to local */
	  Check_If_Var_Occurs(v_adr, u_word))
	return FALSE;
#endif
      Do_Copy_Of_Word(u_tag_mask, u_word);
      Bind_UV(v_adr, u_word);

      return TRUE;
    }

  if (u_word == v_word)
    return TRUE;

  if (v_tag_mask == TAG_LST_MASK)
    {
      if (u_tag_mask != v_tag_mask)
	return FALSE;

      u_adr = UnTag_LST(u_word);
      v_adr = UnTag_LST(v_word);

      u_adr = &Car(u_adr);
      v_adr = &Car(v_adr);

      if (!UNIFY_FCT_NAME(*u_adr++, *v_adr++))
	return FALSE;

      start_u_word = *u_adr;
      start_v_word = *v_adr;
      goto terminal_rec;
    }

  if (v_tag_mask == TAG_STC_MASK)
    {
      if (u_tag_mask != v_tag_mask)
	return FALSE;

      u_adr = UnTag_STC(u_word);
      v_adr = UnTag_STC(v_word);

      if (Functor_And_Arity(u_adr) != Functor_And_Arity(v_adr))
	return FALSE;

      i = Arity(u_adr);
      u_adr = &Arg(u_adr, 0);
      v_adr = &Arg(v_adr, 0);
      while (--i)
	if (!UNIFY_FCT_NAME(*u_adr++, *v_adr++))
	  return FALSE;

      start_u_word = *u_adr;
      start_v_word = *v_adr;
      goto terminal_rec;
    }

#ifndef NO_USE_FD_SOLVER
  if (v_tag_mask == TAG_INT_MASK && u_tag_mask == TAG_FDV_MASK)
    return Fd_Unify_With_Integer(UnTag_FDV(u_word), UnTag_INT(v_word));

  if (v_tag_mask == TAG_FDV_MASK)
    {
      v_adr = UnTag_FDV(v_word);

      if (u_tag_mask == TAG_INT_MASK)
	return Fd_Unify_With_Integer(v_adr, UnTag_INT(u_word));

      if (u_tag_mask != v_tag_mask) /* i.e. TAG_FDV_MASK */
	return FALSE;

      return Fd_Unify_With_Fd_Var(UnTag_FDV(u_word), v_adr);
    }
#endif

  if (v_tag_mask == TAG_FLT_MASK)
    return (u_tag_mask == v_tag_mask &&
	    Pl_Obtain_Float(UnTag_FLT(u_word)) ==
	    Pl_Obtain_Float(UnTag_FLT(v_word)));

  return FALSE;
}



#ifdef OCCURS_CHECK

/*-------------------------------------------------------------------------*
 * CHECK_IF_VAR_OCCURS                                                     *
 *                                                                         *
 * Only called if var_adr resides in the heap since a var residing in the  *
 * local stack cannot appear in a term (there is no binding from the heap  *
 * to the local stack in the WAM).                                         *
 *-------------------------------------------------------------------------*/
static Bool
Check_If_Var_Occurs(WamWord *var_adr, WamWord term_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int i;

 terminal_rec:

  DEREF(term_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    return UnTag_REF(word) == var_adr;

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      adr = &Car(adr);
      if (Check_If_Var_Occurs(var_adr, *adr++))
	return TRUE;

      term_word = *adr;
      goto terminal_rec;
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);

      i = Arity(adr);
      adr = &Arg(adr, 0);
      while (--i)
	if (Check_If_Var_Occurs(var_adr, *adr++))
	  return TRUE;

      term_word = *adr;
      goto terminal_rec;
    }

  return FALSE;
}


#endif
