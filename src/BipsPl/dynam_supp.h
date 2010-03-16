/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dynam_supp.h                                                    *
 * Descr.: dynamic predicate support - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DYN_ALT_FCT_FOR_TEST       0
#define DYN_ALT_FCT_FOR_JUMP       1




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef long (*ScanFct) ();

typedef unsigned long DynStamp;

typedef struct dynpinf *DynPInfP;

typedef struct dyncinf *DynCInfP;

typedef struct			/* Dobly-linked chain header     */
{				/* ----------------------------- */
  DynCInfP first;		/* first clause (or NULL)        */
  DynCInfP last;		/* last  clause (or NULL)        */
}D2ChHdr;


typedef struct			/* Dobly-linked chain cell       */
{				/* ----------------------------- */
  DynCInfP next;		/* next     clause (or NULL)     */
  DynCInfP prev;		/* previous clause (or NULL)     */
}D2ChCell;


typedef struct dyncinf		/* Dynamic clause information     */
{				/* ------------------------------ */
  D2ChCell seq_chain;		/* sequential chain               */
  D2ChCell ind_chain;		/* indexical  chain               */
  DynPInfP dyn;			/* back ptr to associated dyn inf */
  D2ChHdr *p_ind_hdr;		/* back ptr to ind_chain header   */
  char **p_ind_htbl;		/* back ptr to ind htbl (or NULL) */
  int cl_no;			/* clause number                  */
  DynStamp erase_stamp;		/* FFF...F if not erased or stamp */
  DynCInfP next_erased_cl;	/* pointer to next erased clause  */
  unsigned *pl_byte_code;		/* bc pointer (NULL=interpreted)  */
  int term_size;		/* size of the term of the clause */
  WamWord term_word;		/* clause [Head|Body]=<LST,adr+1> */
  WamWord head_word;		/* adr+1 = Car = clause term Head */
  WamWord body_word;		/* adr+2 = Cdr = clause term Body */
}
DynCInf;




typedef struct			/* Dynamic switch item info       */
{				/* ------------------------------ */
  long key;			/* key: atm, int, f/n             */
  D2ChHdr ind_chain;		/* indexical chain                */
}
DSwtInf;




typedef struct dynpinf		/* Dynamic predicate information  */
{				/* ------------------------------ */
  D2ChHdr seq_chain;		/* sequential chain               */
  D2ChHdr var_ind_chain;	/* index if 1st arg=VAR (chain)   */
  char *atm_htbl;		/* index if 1st arg=ATM (htable)  */
  char *int_htbl;		/* index if 1st arg=INT (htable)  */
  D2ChHdr lst_ind_chain;	/* index if 1st arg=LST (chain)   */
  char *stc_htbl;		/* index if 1st arg=STC (htable)  */
  int arity;			/* arity (redundant but faster)   */
  int count_a;			/* next clause nb for asserta     */
  int count_z;			/* next clause nb for assertz     */
  DynCInfP first_erased_cl;	/* 1st erased clause NULL if none */
  DynPInfP next_dyn_with_erase;	/* next dyn with erased clauses   */
}
DynPInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

DynCInf *Pl_Add_Dynamic_Clause(WamWord head_word, WamWord body_word,
			    Bool asserta, Bool check_perm);

void Pl_Delete_Dynamic_Clause(DynCInf *clause);

PredInf *Pl_Update_Dynamic_Pred(int func, int arity, int what_to_do);

DynCInf *Pl_Scan_Dynamic_Pred(int owner_func, int owner_arity,
			   DynPInf *dyn, WamWord first_arg_word,
			   ScanFct alt_fct, int alt_fct_type,
			   int alt_info_size, WamWord *alt_info);

int Pl_Scan_Choice_Point_Pred(WamWord *b, int *arity);

void Pl_Copy_Clause_To_Heap(DynCInf *clause, WamWord *head_word,
			 WamWord *body_word);
