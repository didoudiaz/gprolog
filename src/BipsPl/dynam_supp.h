
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dynam_supp.h                                                    *
 * Descr.: dynamic predicate support - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

/* see explanations in dynam_supp.c */
#if 1
#define OPTIM_FIRST_FOR_SCAN
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DYN_ALT_FCT_FOR_TEST       0
#define DYN_ALT_FCT_FOR_JUMP       1




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef PlULong DynStamp;

typedef struct dynpinf DynPInf;

typedef struct dyncinf DynCInf;

typedef PlLong (*ScanFct) (DynCInf *clause, WamWord *alt_ino, Bool is_last);

typedef struct			/* Double-linked chain header    */
{				/* ----------------------------- */
  DynCInf *first;		/* first clause (or NULL)        */
#ifdef OPTIM_FIRST_FOR_SCAN
  DynCInf *first_for_scan;	/* first for next scan (optim)   */
#endif
  DynCInf *last;		/* last  clause (or NULL)        */
}D2ChHdr;


typedef struct			/* Double-linked chain cell      */
{				/* ----------------------------- */
  DynCInf *next;		/* next     clause (or NULL)     */
  DynCInf *prev;		/* previous clause (or NULL)     */
}D2ChCell;


struct dyncinf			/* Dynamic clause information     */
{				/* ------------------------------ */
  D2ChCell seq_chain;		/* sequential chain               */
  D2ChCell ind_chain;		/* indexical  chain               */
  DynPInf *dyn;			/* back ptr to associated dyn inf */
  D2ChHdr *p_ind_hdr;		/* back ptr to ind_chain header   */
  char **p_ind_htbl;		/* back ptr to ind htbl (or NULL) */
  int cl_no;			/* clause number                  */
  int pl_file;			/* file name of its def (or -1)   */
  DynStamp erase_stamp;		/* erase stamp or FFF...F if not  */
  DynCInf *next_erased_cl;	/* pointer to next erased clause  */
  unsigned *byte_code;		/* bc pointer (NULL=interpreted)  */
  int term_size;		/* size of the term of the clause */
  WamWord term_word;		/* clause [Head|Body]=<LST,adr+1> */
  WamWord head_word;		/* adr+1 = Car = clause term Head */
  WamWord body_word;		/* adr+2 = Cdr = clause term Body */
				/* ... space for rest of term ... */
};


typedef struct			/* Dynamic switch item info       */
{				/* ------------------------------ */
  PlLong key;			/* key: atm, int, f/n             */
  D2ChHdr ind_chain;		/* indexical chain                */
}
DSwtInf;


struct dynpinf			/* Dynamic predicate information  */
{				/* ------------------------------ */
  D2ChHdr seq_chain;		/* sequential chain               */
  D2ChHdr var_ind_chain;	/* index if 1st arg=VAR (chain)   */
  char *atm_htbl;		/* index if 1st arg=ATM (htable)  */
  char *int_htbl;		/* index if 1st arg=INT (htable)  */
  D2ChHdr lst_ind_chain;	/* index if 1st arg=LST (chain)   */
  char *stc_htbl;		/* index if 1st arg=STC (htable)  */
  int func;			/* functor (redundant but for dbg)*/
  int arity;			/* arity (redundant but faster)   */
  int count_a;			/* next clause no for asserta, < 0*/
  int count_z;			/* next clause no for assertz, >=0*/
				/* ------- LDUV handling -------- */
  DynStamp curr_stamp;		/* erase stamp or FFF...F if not  */
  DynCInf *first_erased_cl;	/* 1st erased clause, NULL if none*/
  DynPInf *next_dyn_with_erase;	/* next dyn with erased clauses   */
};




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

DynCInf *Pl_Add_Dynamic_Clause(WamWord head_word, WamWord body_word,
			       Bool asserta, Bool check_perm, int pl_file);

void Pl_Delete_Dynamic_Clause(DynCInf *clause);

PredInf *Pl_Update_Dynamic_Pred(int func, int arity, int what_to_do, int pl_file_for_multi);

DynCInf *Pl_Scan_Dynamic_Pred(int owner_func, int owner_arity,
			      DynPInf *dyn, WamWord first_arg_word,
			      ScanFct alt_fct, int alt_fct_type,
			      int alt_info_size, WamWord *alt_info);

int Pl_Scan_Choice_Point_Pred(WamWord *b, int *arity);

void Pl_Copy_Clause_To_Heap(DynCInf *clause, WamWord *head_word, WamWord *body_word);
