/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_supp.h                                                     *
 * Descr.: term support - header file                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

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

long pl_glob_dico_var[MAX_VAR_IN_TERM];	/* a general purpose dico */

#else

extern WamWord pl_pi_name_word;
extern WamWord pl_pi_arity_word;

extern long pl_glob_dico_var[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

long Pl_Term_Compare(WamWord start_u_word, WamWord start_v_word);

Bool Is_List(WamWord start_word);

Bool Is_Partial_List(WamWord start_word);

Bool Is_List_Or_Partial(WamWord start_word);

void Pl_Treat_Vars_Of_Term(WamWord start_word, Bool generic_var,
			void (*fct) ());

int Pl_List_Length(WamWord start_word);

int Pl_Term_Size(WamWord start_word);

void Pl_Copy_Term(WamWord *dst_adr, WamWord *src_adr);

void Pl_Copy_Contiguous_Term(WamWord *dst_adr, WamWord *src_adr);

int Pl_Get_Pred_Indicator(WamWord pred_indic_word, Bool must_be_ground,
		       int *arity);
