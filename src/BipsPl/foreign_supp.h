/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : foreign_supp.h                                                  */
/* Descr.: foreign interface support - header file                         */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

#define PL_RECOVER                 0
#define PL_CUT                     1
#define PL_KEEP_FOR_PROLOG         2

#define PL_FAILURE                 FALSE
#define PL_SUCCESS                 TRUE
#define PL_EXCEPTION               2




/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

typedef struct
    {
     Bool is_var;
     Bool unify;
     union
        {
         long   l;
         char  *s;
         double d;
        }value;
    }FIOArg;





/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

#ifdef FOREIGN_SUPP_FILE

       int   foreign_bkt_counter;
       char *foreign_bkt_buffer;

#else

extern int   foreign_bkt_counter;
extern char *foreign_bkt_buffer;

#endif




/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

void      Foreign_Create_Choice (CodePtr codep_alt,int arity,
                                 int choice_size);
void      Foreign_Update_Choice (CodePtr codep_alt,int arity,
                                 int choice_size);
CodePtr   Foreign_Jump_Ret      (CodePtr codep);

FIOArg   *Foreign_Rd_IO_Arg     (int arg_long,WamWord start_word,
                                 long (*rd_fct)(),int fio_arg_index);

Bool      Foreign_Un_IO_Arg     (int arg_long,Bool (*un_fct)(),FIOArg *fa,
                                 WamWord start_word);


void      Emit_Syntax_Error     (char *file_name,int err_line,int err_col,
                                 char *err_msg);

int       Type_Of_Term          (WamWord start_word);



void      Pl_Exec_Continuation  (int func,int arity,WamWord *arg_adr);
int       Pl_Query_Start        (int func,int arity,WamWord *arg_adr,
                                 Bool recoverable);
int       Pl_Query_Next_Solution(void);
void      Pl_Query_End          (int op);
WamWord   Pl_Get_Exception      (void);



#define Get_Choice_Counter()   foreign_bkt_counter
#define Get_Choice_Buffer(t)   ((t) foreign_bkt_buffer)
#define No_More_Choice()       Delete_Last_Choice_Point()

#define PlTerm                 WamWord
#define PLV                    REF

#define Atom_Name(a)           (atom_tbl[(a)].name)
#define Atom_Length(a)         (atom_tbl[(a)].prop.length)
#define Atom_Needs_Quote(a)    (atom_tbl[(a)].prop.needs_quote)
#define Atom_Needs_Scan(a)     (atom_tbl[(a)].prop.needs_scan)

#define atom_nil               ATOM_NIL

#define Stream_Pointer(s)      (stm_tbl+(s))


