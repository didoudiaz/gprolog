/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : unify.c                                                         *
 * Descr.: unification part                                                *
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

static Bool Check_If_Var_Occurs(WamWord *var_adr, WamWord term_word);



/*-------------------------------------------------------------------------*
 * This file is not compiled separately but included twice by wam_inst.c:  *
 *    - to define the Unify function (classical unification).              *
 *    - to define the Unify_Occurs_Check function (+ occurs check).        *
 *-------------------------------------------------------------------------*/
Bool
UNIFY_FCT_NAME(WamWord start_u_word, WamWord start_v_word)
{
  WamWord u_word, u_tag, *u_adr;
  WamWord v_word, v_tag, *v_adr;
  int i;

terminal_rec:

  Deref(start_u_word, u_word, u_tag, u_adr);
  Deref(start_v_word, v_word, v_tag, v_adr);


  if (u_tag == REF)
    {
      if (v_tag == REF)
	{
	  if (u_adr > v_adr)
	    Bind_UV(u_adr, Tag_Value(REF, v_adr));
	  else if (v_adr > u_adr)
	    Bind_UV(v_adr, Tag_Value(REF, u_adr));
	}
      else
	{
#ifdef OCCURS_CHECK
	  if (!Is_A_Local_Adr(u_adr) &&	/* no binding from heap to local */
	      Check_If_Var_Occurs(u_adr, v_word))
	    return FALSE;
#endif
	  Bind_UV(u_adr, Make_Copy_Of_Word(v_tag, v_word));
	}

      return TRUE;
    }


  switch (v_tag)
    {
    case REF:
#ifdef OCCURS_CHECK
      if (!Is_A_Local_Adr(v_adr) &&	/* no binding from heap to local */
	  Check_If_Var_Occurs(v_adr, u_word))
	return FALSE;
#endif
      Bind_UV(v_adr, Make_Copy_Of_Word(u_tag, u_word));

      return TRUE;


    case INT:
      if (u_tag == FDV)
	{
	  u_adr = UnTag_FDV(u_word);
	  return Fd_Unify_With_Integer(u_adr, UnTag_INT(v_word));
	}
    case ATM:
      return (u_word == v_word);	/* test tag and value */


    case FDV:
      v_adr = UnTag_FDV(v_word);
      if (u_tag == INT)
	return Fd_Unify_With_Integer(v_adr, UnTag_INT(u_word));

      if (u_tag != FDV)
	return (FALSE);

      v_adr = UnTag_FDV(v_word);
      return Fd_Unify_With_Fd_Var(u_adr, v_adr);


    case FLT:
      return (u_word == v_word) ||
	(u_tag == FLT && (Obtain_Float(UnTag_FLT(u_word)) ==
			  Obtain_Float(UnTag_FLT(v_word))));


    case LST:
      if (u_tag != LST)
	return FALSE;

      u_adr = UnTag_LST(u_word);
      v_adr = UnTag_LST(v_word);

      if (u_adr == v_adr)
	return TRUE;

      u_adr = &Car(u_adr);
      v_adr = &Car(v_adr);
      if (!UNIFY_FCT_NAME(*u_adr++, *v_adr++))
	return FALSE;

      start_u_word = *u_adr;
      start_v_word = *v_adr;
      goto terminal_rec;

    default:			/* v_tag==STC */
      if (u_tag != STC)
	return FALSE;

      u_adr = UnTag_STC(u_word);
      v_adr = UnTag_STC(v_word);

      if (u_adr == v_adr)
	return TRUE;

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
  WamWord word, tag, *adr;
  int i;

terminal_rec:

  Deref(term_word, word, tag, adr);

  switch (tag)
    {
    case REF:
      return adr == var_adr;

    case LST:
      adr = UnTag_LST(word);
      adr = &Car(adr);
      if (Check_If_Var_Occurs(var_adr, *adr++))
	return TRUE;

      term_word = *adr;
      goto terminal_rec;

    case STC:			/* tag==STC */
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
