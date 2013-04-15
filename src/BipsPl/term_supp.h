/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_supp.h                                                     *
 * Descr.: term support - header file                                      *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef TERM_SUPP_FILE

WamWord pl_pi_name_word;
WamWord pl_pi_arity_word;

PlLong pl_glob_dico_var[MAX_VAR_IN_TERM];	/* a general purpose dico */

#else

extern WamWord pl_pi_name_word;
extern WamWord pl_pi_arity_word;

extern PlLong pl_glob_dico_var[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

PlLong Pl_Term_Compare(WamWord start_u_word, WamWord start_v_word);

Bool Is_List(WamWord start_word);

Bool Is_Partial_List(WamWord start_word);

Bool Is_List_Or_Partial(WamWord start_word);

Bool Pl_Treat_Vars_Of_Term(WamWord start_word, Bool generic_var,
			   Bool (*fct) ());

int Pl_List_Length(WamWord start_word);

int Pl_Term_Size(WamWord start_word);

void Pl_Copy_Term(WamWord *dst_adr, WamWord *src_adr);

void Pl_Copy_Contiguous_Term(WamWord *dst_adr, WamWord *src_adr);

int Pl_Get_Pred_Indicator(WamWord pred_indic_word, Bool must_be_ground,
			  int *arity);

Bool Pl_Acyclic_Term_1(WamWord start_word);
