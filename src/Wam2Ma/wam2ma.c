/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam2ma.c                                                        *
 * Descr.: code generation                                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "wam_parser.h"
#include "../EnginePl/gp_config.h"
#include "../EnginePl/arch_dep.h"
#define ONLY_TAG_PART
#include "../EnginePl/wam_archi.h"
#include "../EnginePl/pl_params.h"
#include "../BipsPl/pred_supp.h"

#include "bt_string.c"
#include "../TopComp/copying.c"

#ifdef FC_USED_TO_COMPILE_CORE
#define FAST "fast "
#else
#define FAST
#endif

#if 1
#define USE_TAGGED_CALLS
#endif

#if 0
#define CHECK_PRINTF_ARGS
#endif

#ifdef CHECK_PRINTF_ARGS
#define GCCPRINTF(x) __attribute__((format(printf,x,x+1)))
#else
#define GCCPRINTF(x)
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DEFAULT_OUTPUT_SUFFIX      ".ma"

#define MAX_PRED_NAME_LENGTH       2048
#define MAX_LABEL_LENGTH           32


#define ANY_SIZE                   1

#define FOREIGN_TYPE_INTEGER       0
#define FOREIGN_TYPE_POSITIVE      1
#define FOREIGN_TYPE_FLOAT         2
#define FOREIGN_TYPE_NUMBER        3
#define FOREIGN_TYPE_ATOM          4
#define FOREIGN_TYPE_BOOLEAN       5
#define FOREIGN_TYPE_CHAR          6
#define FOREIGN_TYPE_IN_CHAR       7
#define FOREIGN_TYPE_CODE          8
#define FOREIGN_TYPE_IN_CODE       9
#define FOREIGN_TYPE_BYTE          10
#define FOREIGN_TYPE_IN_BYTE       11
#define FOREIGN_TYPE_STRING        12
#define FOREIGN_TYPE_CHARS         13
#define FOREIGN_TYPE_CODES         14
#define FOREIGN_TYPE_TERM          15

#define FOREIGN_TBL_SIZE           16

#define FOREIGN_MODE_IN            0	/* same as in read_file.pl */
#define FOREIGN_MODE_OUT           1
#define FOREIGN_MODE_IN_OUT        2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct swt_elt
{
  BTNode *atom;
  long n;
  long label;
}
SwtElt;




typedef struct swt_tbl *PSwtTbl;

typedef struct swt_tbl
{
  enum
  {
    TBL_ATM,			/* key: atom                             */
    TBL_INT,			/* key: n (used if SWT_INT_NO_OPT)       */
    TBL_STC			/* key: atom/n                           */
  }
  type;
  int tbl_no;			/* sequential no associated to the table */
  PSwtTbl next;			/* next table                            */
  int nb_elem;			/* number of elements                    */
  SwtElt elem[ANY_SIZE];	/* table of switch elements              */
}
SwtTbl;



typedef struct predinf *PredP;

typedef struct predinf
{
  BTNode *functor;
  int arity;
  char *hexa;
  int line_no;
  int prop;
  BTNode *pl_file;
  int pl_line;
  SwtTbl *swt_tbl[3];
  PredP next;
}
Pred;



typedef struct directinf *DirectP;

typedef struct directinf
{
  BTNode *pl_file;
  int pl_line;
  int system;
  DirectP next;
}
Direct;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *file_name_in;
char *file_name_out;
int comment;

FILE *file_out;

BTString bt_atom;
#ifdef USE_TAGGED_CALLS
BTString bt_tagged_atom;
BTString bt_tagged_f_n;
#endif


BTNode *cur_pl_file;

char buff_hexa[MAX_PRED_NAME_LENGTH * 2 + 2];

Pred dummy_pred_start;
Pred *pred_end = &dummy_pred_start;

Direct dummy_direct_start;
Direct *direct_end = &dummy_direct_start;

int nb_swt_tbl = 0;

Pred *cur_pred;
int cur_pred_no = 0;
int cur_arity;
long cur_sub_label;

int cur_direct_no = 0;

char *foreign_tbl[FOREIGN_TBL_SIZE];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Init_Foreign_Table(void);

void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);

void Compute_Hexa(char *str, char *hexa);

void Emit_Obj_Initializer(void);

void Emit_Exec_Directives(void);

void Emit_One_Atom(int no, char *str);

#ifdef USE_TAGGED_CALLS
void Emit_One_Atom_Tagged(int no, char *str);

int Add_F_N_Tagged(char *atom, int n);

void Emit_One_F_N_Tagged(int no, char *str);
#endif

void Label_Printf(char *label, ...) GCCPRINTF(1);

void Inst_Printf(char *op, char *operands, ...) GCCPRINTF(2);

SwtTbl *Create_Switch_Table(int type, int nb_elem);

void Write_Call_C(char *fct_name, ArgVal arg[]);



#define Check_Arg(i, str)      (strncmp(argv[i], str, strlen(argv[i]))==0)



#define DEF_STR(str)          char *str

#define DEF_ATOM(atom)        BTNode *atom; char *str_##atom

#define DEF_PRED(hexa)        char *hexa = buff_hexa; char *str_##hexa

#define DEF_INTEGER(n)        long n

#define DEF_FLOAT(n)          double n

#define DEF_X_Y(xy)           long xy; char c

#ifdef USE_TAGGED_CALLS
#define DEF_F_N(atom, n)      DEF_STR(str_##atom); DEF_INTEGER(n); int f_n_no
#else
#define DEF_F_N(atom, n)      DEF_ATOM(atom); DEF_INTEGER(n)
#endif

#define DEF_P_N(hexa, n)      DEF_PRED(hexa); DEF_INTEGER(n)

#define DEF_LABEL(l)          char l[MAX_LABEL_LENGTH]; long val_##l



#define LOAD_STR(str)         Get_Arg(top, char *, str)

#define LOAD_ATOM_T(atom, t)  Get_Arg(top, char *, str_##atom); \
                              atom = BT_String_Add(&t, str_##atom)

#ifdef USE_TAGGED_CALLS
#define LOAD_ATOM(atom)       LOAD_ATOM_T(atom, bt_tagged_atom)
#else
#define LOAD_ATOM(atom)       LOAD_ATOM_T(atom, bt_atom)
#endif

#define LOAD_PRED(hexa)       Get_Arg(top, char *, str_##hexa); \
                              Compute_Hexa(str_##hexa, buff_hexa)

#define LOAD_INTEGER(n)       Get_Arg(top, long, n)

#define LOAD_FLOAT(n)         Get_Arg(top, double, n)

#define LOAD_X_Y(xy)          Get_Arg(top, long, xy); \
                              if (xy < 5000) c = 'X'; else xy -= 5000, c='Y'

#ifdef USE_TAGGED_CALLS
#define LOAD_F_N(atom, n)     LOAD_STR(str_##atom); LOAD_INTEGER(n);\
                              f_n_no = Add_F_N_Tagged(str_##atom, n)
#else
#define LOAD_F_N(atom, n)     LOAD_ATOM_T(atom, bt_atom); LOAD_INTEGER(n)
#endif

#define LOAD_P_N(hexa, n)     LOAD_PRED(hexa); LOAD_INTEGER(n)

#define LOAD_LABEL(l)         Get_Arg(top, long, val_##l); \
                              if (val_##l==-1) strcpy(l, "0"); \
                              else sprintf(l, FORMAT_LABEL(val_##l))

#define Args1(a1)             ArgVal *top=arg; DEF_##a1; \
                              LOAD_##a1

#define Args2(a1, a2)         ArgVal *top=arg; DEF_##a1; DEF_##a2; \
                              LOAD_##a1; LOAD_##a2

#define Args3(a1, a2, a3)     ArgVal *top=arg; DEF_##a1; DEF_##a2; DEF_##a3;\
                              LOAD_##a1; LOAD_##a2; LOAD_##a3

#define Args4(a1, a2, a3, a4) ArgVal *top=arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4

#define Args5(a1, a2, a3, a4, a5) \
                              ArgVal *top=arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; DEF_##a5; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4; LOAD_##a5

#define Args6(a1, a2, a3, a4, a5, a6) \
                              ArgVal *top=arg; \
                              DEF_##a1; DEF_##a2; DEF_##a3; DEF_##a4; DEF_##a5; DEF_##a6; \
                              LOAD_##a1; LOAD_##a2; LOAD_##a3; LOAD_##a4; LOAD_##a5; LOAD_##a6



#define FORMAT_LABEL(l)       "Lpred%d_%ld", cur_pred_no, (l)

#define FORMAT_SUB_LABEL(sl)  "Lpred%d_sub_%ld", cur_pred_no, (sl)




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  Parse_Arguments(argc, argv);

  if (file_name_out == NULL)
    file_out = stdout;
  else if ((file_out = fopen(file_name_out, "wt")) == NULL)
    {
      fprintf(stderr, "cannot open output file %s\n", file_name_out);
      exit(1);
    }

  BT_String_Init(&bt_atom);
#ifdef USE_TAGGED_CALLS
  BT_String_Init(&bt_tagged_atom);
  BT_String_Init(&bt_tagged_f_n);
#endif

  Init_Foreign_Table();
  dummy_pred_start.next = NULL;
  dummy_direct_start.next = NULL;

  if (!Parse_Wam_File(file_name_in, comment))
    {
      fprintf(stderr, "Translation aborted\n");
      exit(1);
    }

  Emit_Obj_Initializer();
  Emit_Exec_Directives();

  if (file_out != stdout)
    fclose(file_out);

  exit(0);
}




/*-------------------------------------------------------------------------*
 * COMPUTE_HEXA                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Compute_Hexa(char *str, char *hexa)
{
  *hexa++ = 'X';

  while (*str)
    {
      sprintf(hexa, "%02X", *str++);
      hexa += 2;
    }

  *hexa = '\0';
}




/*-------------------------------------------------------------------------*
 * EMIT_OBJ_INITIALIZER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Obj_Initializer(void)
{
  SwtTbl *t;
  Pred *p;
  int i, j;
  char l[MAX_LABEL_LENGTH];


  Label_Printf("\n");

  if (bt_atom.nb_elem)
    Label_Printf("long local at(%d)", bt_atom.nb_elem);

#ifdef USE_TAGGED_CALLS
  if (bt_tagged_atom.nb_elem)
    Label_Printf("long local ta(%d)", bt_tagged_atom.nb_elem);

  if (bt_tagged_f_n.nb_elem)
    Label_Printf("long local fn(%d)", bt_tagged_f_n.nb_elem);
#endif

  if (nb_swt_tbl)
    Label_Printf("long local st(%d)", nb_swt_tbl);

  Label_Printf("\n");

  Label_Printf("c_code  initializer Object_Initializer\n");

  Inst_Printf("call_c", "New_Object(&System_Directives,&User_Directives)");

  BT_String_List(&bt_atom, Emit_One_Atom);
#ifdef USE_TAGGED_CALLS
  BT_String_List(&bt_tagged_atom, Emit_One_Atom_Tagged);
  BT_String_List(&bt_tagged_f_n, Emit_One_F_N_Tagged);
#endif

  cur_pred_no = 0;
  for (p = dummy_pred_start.next; p; p = p->next)
    {
      fputc('\n', file_out);
      if (!(p->prop & MASK_PRED_DYNAMIC))
	{
	  Inst_Printf("call_c", FAST
		      "Create_Pred(at(%d),%d,at(%d),%d,%d,&%s_%d)",
		      p->functor->no, p->arity, p->pl_file->no, p->pl_line,
		      p->prop, p->hexa, p->arity);
	}
      else
	Inst_Printf("call_c", FAST "Create_Pred(at(%d),%d,at(%d),%d,%d,0)",
		    p->functor->no, p->arity, p->pl_file->no, p->pl_line,
		    p->prop);

      cur_pred_no++;		/* for FORMAT_LABEL */

      for (i = 0; i < 3; i++)
	for (t = p->swt_tbl[i]; t != NULL; t = t->next)
	  {
	    Inst_Printf("call_c", FAST "Create_Swt_Table(%d)", t->nb_elem);
	    Inst_Printf("move_ret", "st(%d)", t->tbl_no);

	    switch (i)
	      {
	      case TBL_ATM:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Create_Swt_Atm_Element(st(%d),%d,at(%d),&%s)",
				t->tbl_no, t->nb_elem,
				(t->elem[j].atom)->no, l);
		  }
		break;
#ifdef SWT_INT_NO_OPT
	      case TBL_INT:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Create_Swt_Int_Element(st(%d),%d,%ld,&%s)",
				t->tbl_no, t->nb_elem, t->elem[j].n, l);
		  }
		break;
#endif
	      default:
		for (j = 0; j < t->nb_elem; j++)
		  {
		    sprintf(l, FORMAT_LABEL(t->elem[j].label));
		    Inst_Printf("call_c", FAST
				"Create_Swt_Stc_Element(st(%d),%d,at(%d),%ld,&%s)",
				t->tbl_no, t->nb_elem,
				(t->elem[j].atom)->no, t->elem[j].n, l);
		  }
	      }
	  }
    }

  Inst_Printf("c_ret", "");
}




/*-------------------------------------------------------------------------*
 * EMIT_EXEC_DIRECTIVES                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Exec_Directives(void)
{
  int i;
  Direct *p;

  fputc('\n', file_out);
  Label_Printf("c_code  local System_Directives\n");

  i = 0;
  for (p = dummy_direct_start.next; p; p = p->next)
    {
      i++;
      if (!p->system)
	continue;

      Inst_Printf("call_c", "Execute_Directive(at(%d),%d,%d,&directive_%d)",
		  p->pl_file->no, p->pl_line, 1, i);
    }

  Inst_Printf("c_ret", "");

  fputc('\n', file_out);
  Label_Printf("c_code  local User_Directives\n");

  i = 0;
  for (p = dummy_direct_start.next; p; p = p->next)
    {
      i++;
      if (p->system)
	continue;

      Inst_Printf("call_c", "Execute_Directive(at(%d),%d,%d,&directive_%d)",
		  p->pl_file->no, p->pl_line, 0, i);
    }

  Inst_Printf("c_ret", "");
}




/*-------------------------------------------------------------------------*
 * EMIT_ONE_ATOM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_Atom(int no, char *str)
{
  Inst_Printf("call_c", FAST "Create_Atom(\"%s\")", str);
  Inst_Printf("move_ret", "at(%d)", no);
}




#ifdef USE_TAGGED_CALLS
/*-------------------------------------------------------------------------*
 * EMIT_ONE_ATOM_TAGGED                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_Atom_Tagged(int no, char *str)
{
  Inst_Printf("call_c", FAST "Create_Atom_Tagged(\"%s\")", str);
  Inst_Printf("move_ret", "ta(%d)", no);
}




/*-------------------------------------------------------------------------*
 * ADD_F_N_TAGGED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Add_F_N_Tagged(char *atom, int n)
{
  int l = strlen(atom);


  atom = (char *) realloc(atom, l + 5 + 1);
  sprintf(atom + l, "/%d", n);

  return BT_String_Add(&bt_tagged_f_n, atom)->no;
}




/*-------------------------------------------------------------------------*
 * EMIT_ONE_F_N_TAGGED                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_One_F_N_Tagged(int no, char *str)
{
  int n;
  char *p = str + strlen(str) - 1;

  for(p = str + strlen(str) - 1; *p != '/'; p--)
    ;

  n = atoi(p+1);
  *p = '\0';

  Inst_Printf("call_c", FAST "Create_Functor_Arity_Tagged(\"%s\",%d)", str, n);
  Inst_Printf("move_ret", "fn(%d)", no);
}
#endif




/*-------------------------------------------------------------------------*
 * SOURCE_LINE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Source_Line(int line_no, char *cmt)
{
  Label_Printf("\t; %6d: %s", line_no, cmt);
}




/*-------------------------------------------------------------------------*
 * PROLOG_FILE_NAME                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Prolog_File_Name(char *pl_file)
{
  cur_pl_file = BT_String_Add(&bt_atom, pl_file);
}




/*-------------------------------------------------------------------------*
 * NEW_PREDICATE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
New_Predicate(char *functor, int arity, int pl_line, int dynamic,
	      int public, int built_in, int built_in_fd)
{
  BTNode *atom;
  char *hexa;
  int prop;

  atom = BT_String_Add(&bt_atom, functor);

  if ((hexa = (char *) malloc(1 + 2 * strlen(functor) + 1)) == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for hexa of: %s\n",
	      atom->str);
      exit(1);
    }
  Compute_Hexa(functor, hexa);

  cur_arity = arity;
  cur_sub_label = 0;


  if (comment)
    Label_Printf("\n\n; *** Predicate: %s/%d (%s:%d)",
		 functor, arity, cur_pl_file->str, pl_line);

  Label_Printf("\n\npl_code global %s_%d", hexa, arity);

  if (dynamic)
    prop = MASK_PRED_DYNAMIC;
  else
    prop = MASK_PRED_NATIVE_CODE;

  if (public)
    prop |= MASK_PRED_PUBLIC;

  if (built_in)
    prop |= MASK_PRED_BUILTIN;

  if (built_in_fd)
    prop |= MASK_PRED_BUILTIN_FD;


  cur_pred_no++;

  cur_pred = (Pred *) malloc(sizeof(Pred));
  if (cur_pred == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for predicate #%d\n",
	      cur_pred_no);
      exit(1);
    }

  cur_pred->functor = atom;
  cur_pred->arity = arity;
  cur_pred->hexa = hexa;
  cur_pred->pl_file = cur_pl_file;
  cur_pred->pl_line = pl_line;
  cur_pred->prop = prop;
  cur_pred->swt_tbl[0] = NULL;
  cur_pred->swt_tbl[1] = NULL;
  cur_pred->swt_tbl[2] = NULL;
  cur_pred->next = NULL;

  pred_end->next = cur_pred;
  pred_end = cur_pred;
}




/*-------------------------------------------------------------------------*
 * NEW_DIRECTIVE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
New_Directive(int pl_line, int system)
{
  Direct *p;

  cur_direct_no++;
  p = (Direct *) malloc(sizeof(Direct));
  if (p == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for directive #%d\n",
	      cur_direct_no);
      exit(1);
    }

  p->pl_file = cur_pl_file;
  p->pl_line = pl_line;
  p->system = system;
  p->next = NULL;

  direct_end->next = p;
  direct_end = p;

  if (comment)
    Label_Printf("\n\n; *** %s Directive (%s:%d)",
		 (system) ? "System" : "User", cur_pl_file->str, pl_line);

  Label_Printf("\n\npl_code local directive_%d", cur_direct_no);
}




/*-------------------------------------------------------------------------*
 * ENSURE_LINKED                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Ensure_Linked(ArgVal arg[])
{
  DEF_PRED(hexa);
  DEF_INTEGER(n);
  Args1(INTEGER(nb_elem));

  Label_Printf("\n\npl_code local ensure_linked");
  while (nb_elem--)
    {
      LOAD_PRED(hexa);
      LOAD_INTEGER(n);
      Inst_Printf("pl_jump", "%s_%ld", hexa, n);
    }

}




/*-------------------------------------------------------------------------*
 * LABEL_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Label_Printf(char *label, ...)
{
  va_list arg_ptr;


  va_start(arg_ptr, label);

  vfprintf(file_out, label, arg_ptr);

  va_end(arg_ptr);
  fputc('\n', file_out);
}




/*-------------------------------------------------------------------------*
 * INST_PRINTF                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Inst_Printf(char *op, char *operands, ...)
{
  va_list arg_ptr;


  va_start(arg_ptr, operands);

  fprintf(file_out, "\t%-10s ", op);
  if (operands)
    {
      vfprintf(file_out, operands, arg_ptr);
      fputc('\n', file_out);
    }

  va_end(arg_ptr);
}




/*-------------------------------------------------------------------------*
 * F_GET_VARIABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_variable(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("move", "X(%ld),%c(%ld)", a, c, xy);
}




/*-------------------------------------------------------------------------*
 * F_GET_VALUE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_value(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("call_c", FAST "Unify(%c(%ld),X(%ld))", c, xy, a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_atom(ArgVal arg[])
{
  Args2(ATOM(atom), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Get_Atom_Tagged(ta(%d),X(%ld))", atom->no, a);
#else
  Inst_Printf("call_c", FAST "Get_Atom(at(%d),X(%ld))", atom->no, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_integer(ArgVal arg[])
{
  Args2(INTEGER(n), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Get_Integer_Tagged(%ld,X(%ld))", Tag_INT(n), a);
#else
  Inst_Printf("call_c", FAST "Get_Integer(%ld,X(%ld))", n, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_float(ArgVal arg[])
{
  Args2(FLOAT(n), INTEGER(a));
  Inst_Printf("call_c", FAST "Get_Float(%1.20e,X(%ld))", n, a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_NIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_nil(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Get_Nil(X(%ld))", a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_list(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Get_List(X(%ld))", a);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_GET_STRUCTURE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_get_structure(ArgVal arg[])
{
  Args2(F_N(atom, n), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Get_Structure_Tagged(fn(%d),X(%ld))", f_n_no,
	      a);
#else
  Inst_Printf("call_c", FAST "Get_Structure(at(%d),%ld,X(%ld))", atom->no,
	      n, a);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_PUT_VARIABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_variable(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  if (c == 'X')
    {
      Inst_Printf("call_c", FAST "Put_X_Variable()");
      Inst_Printf("move_ret", "X(%ld)", a);
      Inst_Printf("move", "X(%ld),X(%ld)", a, xy);
    }
  else
    {
      Inst_Printf("call_c", FAST "Put_Y_Variable(&Y(%ld))", xy);
      Inst_Printf("move_ret", "X(%ld)", a);
    }
}




/*-------------------------------------------------------------------------*
 * F_PUT_VOID                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_void(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Put_X_Variable()");
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_VALUE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_value(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("move", "%c(%ld),X(%ld)", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_UNSAFE_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_unsafe_value(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("call_c", FAST "Put_Unsafe_Value(%c(%ld))", c, xy);
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_atom(ArgVal arg[])
{
  Args2(ATOM(atom), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Put_Atom_Tagged(ta(%d))", atom->no);
#else
  Inst_Printf("call_c", FAST "Put_Atom(at(%d))", atom->no);
#endif
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_integer(ArgVal arg[])
{
  Args2(INTEGER(n), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Put_Integer_Tagged(%ld)", Tag_INT(n));
#else
  Inst_Printf("call_c", FAST "Put_Integer(%ld)", n);
#endif
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_float(ArgVal arg[])
{
  Args2(FLOAT(n), INTEGER(a));
  Inst_Printf("call_c", FAST "Put_Float(%1.20e)", n);
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_NIL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_nil(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Put_Nil()");
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_list(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Put_List()");
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_PUT_STRUCTURE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_put_structure(ArgVal arg[])
{
  Args2(F_N(atom, n), INTEGER(a));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Put_Structure_Tagged(fn(%d))", f_n_no);
#else
  Inst_Printf("call_c", FAST "Put_Structure(at(%d),%ld)", atom->no, n);
#endif
  Inst_Printf("move_ret", "X(%ld)", a);
}




/*-------------------------------------------------------------------------*
 * F_MATH_LOAD_VALUE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_math_load_value(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("call_c", FAST "Math_Load_Value(%c(%ld),&X(%ld))", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_MATH_FAST_LOAD_VALUE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_math_fast_load_value(ArgVal arg[])
{
  Args2(X_Y(xy), INTEGER(a));
  Inst_Printf("call_c", FAST "Math_Fast_Load_Value(%c(%ld),&X(%ld))", c, xy, a);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VARIABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_variable(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Unify_Variable()");
  Inst_Printf("move_ret", "%c(%ld)", c, xy);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VOID                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_void(ArgVal arg[])
{
  Args1(INTEGER(n));
  Inst_Printf("call_c", FAST "Unify_Void(%ld)", n);
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_VALUE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_value(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Unify_Value(%c(%ld))", c, xy);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_LOCAL_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_local_value(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Unify_Local_Value(%c(%ld))", c, xy);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_ATOM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_atom(ArgVal arg[])
{
  Args1(ATOM(atom));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Unify_Atom_Tagged(ta(%d))", atom->no);
#else
  Inst_Printf("call_c", FAST "Unify_Atom(at(%d))", atom->no);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_INTEGER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_integer(ArgVal arg[])
{
  Args1(INTEGER(n));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Unify_Integer_Tagged(%ld)", Tag_INT(n));
#else
  Inst_Printf("call_c", FAST "Unify_Integer(%ld)", n);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_NIL                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_nil(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Unify_Nil()");
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_LIST                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_list(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Unify_List()");
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_UNIFY_STRUCTURE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_unify_structure(ArgVal arg[])
{
  Args1(F_N(atom, n));
#ifdef USE_TAGGED_CALLS
  Inst_Printf("call_c", FAST "Unify_Structure_Tagged(fn(%d))", f_n_no);
#else
  Inst_Printf("call_c", FAST "Unify_Structure(at(%d),%ld)", atom->no, n);
#endif
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_ALLOCATE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_allocate(ArgVal arg[])
{
  Args1(INTEGER(n));
  Inst_Printf("call_c", FAST "Allocate(%ld)", n);
}




/*-------------------------------------------------------------------------*
 * F_DEALLOCATE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_deallocate(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Deallocate()");
}




/*-------------------------------------------------------------------------*
 * F_CALL                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_call(ArgVal arg[])
{
  Args1(P_N(hexa, n));

  Inst_Printf("pl_call", "%s_%ld", hexa, n);
}




/*-------------------------------------------------------------------------*
 * F_EXECUTE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_execute(ArgVal arg[])
{
  Args1(P_N(hexa, n));

  Inst_Printf("pl_jump", "%s_%ld", hexa, n);
}




/*-------------------------------------------------------------------------*
 * F_PROCEED                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_proceed(ArgVal arg[])
{
  Inst_Printf("pl_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_FAIL                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_fail(ArgVal arg[])
{
  Inst_Printf("pl_fail", "");
}




/*-------------------------------------------------------------------------*
 * F_LABEL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_label(ArgVal arg[])
{
  Args1(LABEL(l));
  Label_Printf("\n%s:", l);
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_TERM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_term(ArgVal arg[])
{
  Args5(LABEL(lv), LABEL(lc), LABEL(li), LABEL(ll), LABEL(ls));

  Inst_Printf("call_c", FAST "Switch_On_Term(%s%s,%s%s,%s%s,%s%s,%s%s)",
	      (*lv == '0') ? "" : "&", lv,
	      (*lc == '0') ? "" : "&", lc,
	      (*li == '0') ? "" : "&", li,
	      (*ll == '0') ? "" : "&", ll, (*ls == '0') ? "" : "&", ls);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * CREATE_SWITCH_TABLE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
SwtTbl *
Create_Switch_Table(int type, int nb_elem)
{
  SwtTbl *t;

  t = (SwtTbl *) malloc(sizeof(SwtTbl) + sizeof(SwtElt) * (nb_elem - 1));
  if (t == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for switch table\n");
      exit(1);
    }

  t->type = type;
  t->tbl_no = nb_swt_tbl++;
  t->next = cur_pred->swt_tbl[type];
  t->nb_elem = nb_elem;

  cur_pred->swt_tbl[type] = t;

  return t;
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_ATOM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_atom(ArgVal arg[])
{
  SwtTbl *t;
  SwtElt *elem;

  DEF_STR(str);
  DEF_INTEGER(label);
  Args1(INTEGER(nb_elem));


  t = Create_Switch_Table(TBL_ATM, nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_STR(str);
      LOAD_INTEGER(label);
      elem->atom = BT_String_Add(&bt_atom, str);
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Switch_On_Atom(st(%d),%d)",
	      nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_INTEGER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_integer(ArgVal arg[])
{
#ifdef SWT_INT_NO_OPT
  SwtTbl *t;
  SwtElt *elem;

  DEF_INTEGER(n);
  DEF_INTEGER(label);
  Args1(INTEGER(nb_elem));

  t = Create_Switch_Table(TBL_INT, nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_INTEGER(n);
      LOAD_INTEGER(label);
      elem->n = n;
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Switch_On_Integer(st(%d),%d)",
	      nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");

#else

  char c;

  DEF_INTEGER(n);
  DEF_LABEL(l);
  Args1(INTEGER(nb_elem));

  Inst_Printf("call_c", FAST "Switch_On_Integer()");
  Inst_Printf("switch_ret", NULL);	/* NULL to avoid newline */
  c = '(';
  while (nb_elem--)
    {
      LOAD_INTEGER(n);
      LOAD_LABEL(l);
      fprintf(file_out, "%c%ld=%s", c, n, l);
      c = ',';
    }
  fprintf(file_out, ")\n");
#endif
}




/*-------------------------------------------------------------------------*
 * F_SWITCH_ON_STRUCTURE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_switch_on_structure(ArgVal arg[])
{
  SwtTbl *t;
  SwtElt *elem;

  DEF_STR(str);
  DEF_INTEGER(arity);
  DEF_INTEGER(label);
  Args1(INTEGER(nb_elem));


  t = Create_Switch_Table(TBL_STC, nb_elem);

  for (elem = t->elem; nb_elem--; elem++)
    {
      LOAD_STR(str);
      LOAD_INTEGER(arity);
      LOAD_INTEGER(label);
      elem->atom = BT_String_Add(&bt_atom, str);
      elem->n = arity;
      elem->label = label;
    }

  Inst_Printf("call_c", FAST "Switch_On_Structure(st(%d),%d)",
	      nb_swt_tbl - 1, t->nb_elem);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_TRY_ME_ELSE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_try_me_else(ArgVal arg[])
{
  Args1(LABEL(l));
  Inst_Printf("call_c", FAST "Create_Choice_Point(&%s,%d)", l, cur_arity);
}




/*-------------------------------------------------------------------------*
 * F_RETRY_ME_ELSE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_retry_me_else(ArgVal arg[])
{
  Args1(LABEL(l));
  Inst_Printf("call_c", FAST "Update_Choice_Point(&%s,%d)", l, cur_arity);
}




/*-------------------------------------------------------------------------*
 * F_TRUST_ME_ELSE_FAIL                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_trust_me_else_fail(ArgVal arg[])
{
  Inst_Printf("call_c", FAST "Delete_Choice_Point(%d)", cur_arity);
}




/*-------------------------------------------------------------------------*
 * F_TRY                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_try(ArgVal arg[])
{
  char sl[MAX_LABEL_LENGTH];

  Args1(LABEL(l));

  sprintf(sl, FORMAT_SUB_LABEL(cur_sub_label++));

  Inst_Printf("call_c", FAST "Create_Choice_Point(&%s,%d)", sl, cur_arity);
  Inst_Printf("jump", "%s", l);
  Label_Printf("%s:", sl);
}




/*-------------------------------------------------------------------------*
 * F_RETRY                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_retry(ArgVal arg[])
{
  char sl[MAX_LABEL_LENGTH];

  Args1(LABEL(l));

  sprintf(sl, FORMAT_SUB_LABEL(cur_sub_label++));

  Inst_Printf("call_c", FAST "Update_Choice_Point(&%s,%d)", sl, cur_arity);
  Inst_Printf("jump", "%s", l);
  Label_Printf("%s:", sl);
}




/*-------------------------------------------------------------------------*
 * F_TRUST                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_trust(ArgVal arg[])
{
  Args1(LABEL(l));

  Inst_Printf("call_c", FAST "Delete_Choice_Point(%d)", cur_arity);
  Inst_Printf("jump", "%s", l);
}




/*-------------------------------------------------------------------------*
 * F_LOAD_CUT_LEVEL                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_load_cut_level(ArgVal arg[])
{
  Args1(INTEGER(a));
  Inst_Printf("call_c", FAST "Load_Cut_Level(&X(%ld))", a);
  cur_arity = a + 1;		/* to save X(a) in backatrack points */
}




/*-------------------------------------------------------------------------*
 * F_CUT                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_cut(ArgVal arg[])
{
  Args1(X_Y(xy));
  Inst_Printf("call_c", FAST "Cut(%c(%ld))", c, xy);
}




/*-------------------------------------------------------------------------*
 * WRITE_CALL_C                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Call_C(char *fct_name, ArgVal arg[])
{
  int i;

  DEF_INTEGER(n);
  Args1(INTEGER(nb_elem));


  Inst_Printf("call_c", NULL);
  fprintf(file_out, "%s(", fct_name);

  for (i = 0; i < nb_elem; i++)
    {
      if (i > 0)
	fputc(',', file_out);
      LOAD_INTEGER(n);
      fprintf(file_out, "X(%ld)", n);
    }

  fprintf(file_out, ")\n");
}




/*-------------------------------------------------------------------------*
 * F_FUNCTION                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_function(ArgVal arg[])
{
  Args2(STR(name), INTEGER(x));

  Write_Call_C(name, top);
  Inst_Printf("move_ret", "X(%ld)", x);
}




/*-------------------------------------------------------------------------*
 * F_CALL_C                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_call_c(ArgVal arg[])
{
  Args1(STR(name));

  Write_Call_C(name, top);
}




/*-------------------------------------------------------------------------*
 * F_CALL_C_TEST                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_call_c_test(ArgVal arg[])
{
  Args1(STR(name));

  Write_Call_C(name, top);
  Inst_Printf("fail_ret", "");
}




/*-------------------------------------------------------------------------*
 * F_CALL_C_JUMP                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_call_c_jump(ArgVal arg[])
{
  Args1(STR(name));

  Write_Call_C(name, top);
  Inst_Printf("jump_ret", "");
}




/*-------------------------------------------------------------------------*
 * INIT_FOREIGN_TABLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Foreign_Table(void)
{
  foreign_tbl[FOREIGN_TYPE_INTEGER] = "Integer";
  foreign_tbl[FOREIGN_TYPE_POSITIVE] = "Positive";
  foreign_tbl[FOREIGN_TYPE_FLOAT] = "Float";
  foreign_tbl[FOREIGN_TYPE_NUMBER] = "Number";
  foreign_tbl[FOREIGN_TYPE_ATOM] = "Atom";
  foreign_tbl[FOREIGN_TYPE_BOOLEAN] = "Boolean";
  foreign_tbl[FOREIGN_TYPE_CHAR] = "Char";
  foreign_tbl[FOREIGN_TYPE_IN_CHAR] = "In_Char";
  foreign_tbl[FOREIGN_TYPE_CODE] = "Code";
  foreign_tbl[FOREIGN_TYPE_IN_CODE] = "In_Code";
  foreign_tbl[FOREIGN_TYPE_BYTE] = "Byte";
  foreign_tbl[FOREIGN_TYPE_IN_BYTE] = "In_Byte";
  foreign_tbl[FOREIGN_TYPE_STRING] = "String";
  foreign_tbl[FOREIGN_TYPE_CHARS] = "Chars";
  foreign_tbl[FOREIGN_TYPE_CODES] = "Codes";
  foreign_tbl[FOREIGN_TYPE_TERM] = "Term";
}




/*-------------------------------------------------------------------------*
 * F_FOREIGN_CALL_C                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
F_foreign_call_c(ArgVal arg[])
#define F_Double(t) ((t)==FOREIGN_TYPE_FLOAT || (t)==FOREIGN_TYPE_NUMBER)
#define F_Array_Letter(t) (F_Double(t) ? 'D' : 'L')
{
  static int type[NB_OF_X_REGS], reg[NB_OF_X_REGS], mode[NB_OF_X_REGS];
  int i, j, fio_arg_index = 0, nb_c_str = 0, s_dup, complex_jump_ret = 0;
  char c;
  char l[MAX_LABEL_LENGTH];

  DEF_INTEGER(n);
  DEF_STR(str_type);

  Args6(STR(fct_name), STR(ret_mode), STR(bip_name), INTEGER(bip_arity),
	INTEGER(chc_size), INTEGER(nb_elem));

  for (i = 0; i < nb_elem; i++)
    {
      LOAD_STR(str_type);
      LOAD_INTEGER(n);
      j = 0;
      for (;;)
	if (strcasecmp(foreign_tbl[j], str_type) == 0)
	  break;
	else if (++j >= FOREIGN_TBL_SIZE)
	  {
	    fprintf(stderr, "invalid foreign type:%s\n", str_type);
	    exit(1);
	  }

      type[i] = j;
      mode[i] = n;
      if ((mode[i] == FOREIGN_MODE_IN || mode[i] == FOREIGN_MODE_IN_OUT) &&
	  (j == FOREIGN_TYPE_CHARS || j == FOREIGN_TYPE_CODES))
	nb_c_str++;
    }

  LOAD_INTEGER(nb_elem);


  for (i = 0; i < nb_elem; i++)
    {
      LOAD_INTEGER(n);
      reg[i] = n;
    }

  if (chc_size >= 0)
    {
      sprintf(l, FORMAT_LABEL(1L));
      Inst_Printf("call_c", "Foreign_Create_Choice(&%s,%d,%ld)",
		  l, cur_arity, chc_size);
      Label_Printf("%s:", l);
      Inst_Printf("call_c", "Foreign_Update_Choice(&%s,%d,%ld)",
		  l, cur_arity, chc_size);
    }

  if (*bip_name || bip_arity != -2)
    Inst_Printf("call_c", FAST "Set_C_Bip_Name(\"%s\",%ld)",
		bip_name, bip_arity);

  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = F_Array_Letter(n);

      s_dup = (mode[i] == FOREIGN_MODE_IN || FOREIGN_MODE_IN_OUT) &&
	(n == FOREIGN_TYPE_CHARS || n == FOREIGN_TYPE_CODES) &&
	--nb_c_str != 0;

      switch (mode[i])
	{
	case FOREIGN_MODE_IN:
	  if (n != FOREIGN_TYPE_TERM)
	    {
	      Inst_Printf("call_c", "Rd_%s_Check(X(%d))",
			  foreign_tbl[n], reg[i]);
	      Inst_Printf("move_ret", "F%c(%d)", c, i);

	      if (s_dup)
		{
		  Inst_Printf("call_c", "strdup(FL(%d))", i);
		  Inst_Printf("move_ret", "FL(%d)", i);
		}
	    }
	  break;

	case FOREIGN_MODE_OUT:
	  complex_jump_ret = 1;	/* arg to unif. complex jump_ret */
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Check_For_Un_%s(X(%d))",
			foreign_tbl[n], reg[i]);
	  break;

	case FOREIGN_MODE_IN_OUT:
	  complex_jump_ret = 1;	/* arg to unif. complex jump_ret */
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Foreign_Rd_IO_Arg(%d,X(%d),"
			"&Rd_%s_Check,%d)", (c == 'L') + s_dup,	/* 0,1 or 2 if strdup */
			reg[i], foreign_tbl[n], fio_arg_index++);
	  else
	    Inst_Printf("call_c", "Foreign_Rd_IO_Arg(1,X(%d),0,%d)",
			reg[i], fio_arg_index++);
	  Inst_Printf("move_ret", "FL(%d)", i);
	  break;
	}
    }

  Inst_Printf("call_c", NULL);
  fprintf(file_out, "%s(", fct_name);
  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = (mode[i] == FOREIGN_MODE_IN_OUT) ? 'L' : F_Array_Letter(n);
      if (i > 0)
	fputc(',', file_out);
      if (mode[i] == FOREIGN_MODE_OUT)
	fprintf(file_out, "&");
      if (n == FOREIGN_TYPE_TERM && mode[i] == FOREIGN_MODE_IN)
	fprintf(file_out, "X(%d)", reg[i]);
      else
	fprintf(file_out, "F%c(%d)", c, i);
    }

  fprintf(file_out, ")\n");
  if (strcmp(ret_mode, "jump") == 0)
    {
      if (!complex_jump_ret)
	Inst_Printf("jump_ret", "");
      else
	Inst_Printf("move_ret", "FL(%d)", NB_OF_X_REGS - 1);
    }
  else
    {
      complex_jump_ret = 0;
      if (strcmp(ret_mode, "none") != 0)
	Inst_Printf("fail_ret", "");
    }


  for (i = 0; i < nb_elem; i++)
    {
      n = type[i];
      c = F_Array_Letter(type[i]);
      switch (mode[i])
	{
	case FOREIGN_MODE_OUT:
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Un_%s(F%c(%d),X(%d))",
			foreign_tbl[n], c, i, reg[i]);
	  else
	    Inst_Printf("call_c", FAST "Unify(X(%d),FL(%d))", reg[i], i);
	  Inst_Printf("fail_ret", "");
	  break;

	case FOREIGN_MODE_IN_OUT:
	  if (n != FOREIGN_TYPE_TERM)
	    Inst_Printf("call_c", "Foreign_Un_IO_Arg(%d,&Un_%s,FL(%d),"
			"X(%d))", c == 'L', foreign_tbl[n], i, reg[i]);
	  else
	    Inst_Printf("call_c", "Foreign_Un_IO_Arg(1,&Unify,FL(%d),"
			"X(%d))", i, reg[i]);
	  Inst_Printf("fail_ret", "");
	  break;
	}
    }


  if (complex_jump_ret)
    {
      Inst_Printf("call_c", "Foreign_Jump_Ret(FL(%d))", NB_OF_X_REGS - 1);
      Inst_Printf("jump_ret", "");
    }
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGUMENTS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Parse_Arguments(int argc, char *argv[])
{
  static char str[1024];
  int i;


  file_name_in = file_name_out = NULL;
  comment = 0;

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "-o") || Check_Arg(i, "--output"))
	    {
	      if (++i >= argc)
		{
		  fprintf(stderr, "FILE missing after %s option\n",
			  argv[i - 1]);
		  exit(1);
		}

	      file_name_out = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--comment"))
	    {
	      comment = 1;
	      continue;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      Display_Copying("WAM to Mini-Assembly Compiler");
	      exit(0);
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }

	  fprintf(stderr, "unknown option %s - try wam2ma --help\n",
		  argv[i]);
	  exit(1);
	}

      if (file_name_in != NULL)
	{
	  fprintf(stderr, "input file already specified (%s)\n",
		  file_name_in);
	  exit(1);
	}
      file_name_in = argv[i];
    }


  if (file_name_in != NULL && strcmp(file_name_in, "-") == 0)
    file_name_in = NULL;

  if (file_name_out == NULL && file_name_in != NULL)
    {
      strcpy(str, file_name_in);
      i = strlen(str);
      if (strcmp(str + i - 4, ".wam") == 0)
	strcpy(str + i - 4, DEFAULT_OUTPUT_SUFFIX);
      else
	strcpy(str + i, DEFAULT_OUTPUT_SUFFIX);
      file_name_out = str;
    }

  if (file_name_out != NULL && strcmp(file_name_out, "-") == 0)
    file_name_out = NULL;
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Help(void)
#define L(msg)  fprintf(stderr,"%s\n",msg)
{
  L("Usage: wam2ma [OPTION...] FILE");
  L("");
  L("Options:");
  L("  -o FILE, --output FILE      set output file name");
  L("  --comment                   include comments in the output file");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("");
  L("'-' can be given as FILE for the standard input/output");
  L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
