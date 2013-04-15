/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : callinf_supp.c                                                  *
 * Descr.: meta call info support                                          *
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
 * PL_SAVE_CALL_INFO_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Save_Call_Info_3(WamWord func_word, WamWord arity_word,
		 WamWord debug_call_word)
{
  int func, arity;
  Bool debug_call;

  func = Pl_Rd_Atom(func_word);
  arity = Pl_Rd_Integer(arity_word);
  debug_call = *Pl_Rd_String(debug_call_word) == 't';

  save_call_info = Call_Info(func, arity, debug_call);
}



/*-------------------------------------------------------------------------*
 * PL_LOAD_CALL_INFO_ARG_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Load_Call_Info_Arg_1(WamWord arg_no_word)
{
  int arg_no;

  arg_no = Pl_Rd_Integer(arg_no_word);
  A(arg_no) = Tag_INT(save_call_info);
}




/*-------------------------------------------------------------------------*
 * PL_CALL_INFO_BIP_NAME_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Call_Info_Bip_Name_1(WamWord call_info_word)
{
  PlLong call_info;
  int func, arity;

  call_info = UnTag_INT(call_info_word) >> 1;

  func = Functor_Of(call_info);
  arity = Arity_Of(call_info);

  Pl_Set_Bip_Name_2(Tag_ATM(func), Tag_INT(arity));
}
