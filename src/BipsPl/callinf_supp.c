/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : callinf_supp.c                                                  *
 * Descr.: meta call info support                                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int save_call_info;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * SAVE_CALL_INFO_3                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Save_Call_Info_3(WamWord func_word, WamWord arity_word,
		 WamWord debug_call_word)
{
  int func, arity;
  Bool debug_call;

  func = Rd_Atom(func_word);
  arity = Rd_Integer(arity_word);
  debug_call = *Rd_String(debug_call_word) == 't';

  save_call_info = Call_Info(func, arity, debug_call);
}



/*-------------------------------------------------------------------------*
 * LOAD_CALL_INFO_ARG_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Load_Call_Info_Arg_1(WamWord arg_no_word)
{
  int arg_no;

  arg_no = Rd_Integer(arg_no_word);
  A(arg_no) = Tag_INT(save_call_info);
}




/*-------------------------------------------------------------------------*
 * CALL_INFO_BIP_NAME_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Call_Info_Bip_Name_1(WamWord call_info_word)
{
  long call_info;
  int func, arity;

  call_info = UnTag_INT(call_info_word) >> 1;

  func = Functor_Of(call_info);
  arity = Arity_Of(call_info);

  Set_Bip_Name_2(Tag_ATM(func), Tag_INT(arity));
}
