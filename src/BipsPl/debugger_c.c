/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : debugger_c.c                                                    *
 * Descr.: debugger - C part                                               *
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

#include <string.h>
#include <stdlib.h>

#define OBJ_INIT Debug_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define BANK_NAME_OFFSET_LENGTH    15

#if WORD_SIZE == 32
#define HEXADECIMAL_LENGTH         10
#define DECIMAL_LENGTH             10
#else
#define HEXADECIMAL_LENGTH         20
#define DECIMAL_LENGTH             20
#endif
#define VALUE_PART_LENGTH          BANK_NAME_OFFSET_LENGTH

#define SEPARATOR_LIST             " ,[]\n"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef Bool (*FctPtr) ();

typedef struct
{
  char *name;
  FctPtr fct;
}
InfCmd;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

WamCont debug_call_code;

static int nb_read_arg;
static char read_arg[30][80];


static char *envir_name[] = ENVIR_NAMES;
static char *choice_name[] = CHOICE_NAMES;
static char *trail_tag_name[] = TRAIL_TAG_NAMES;
static WamWord reg_copy[NB_OF_REGS];

static StmInf *pstm_i;
static StmInf *pstm_o;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void My_System_Directives(void);

static void Scan_Command(char *source_str);

static FctPtr Find_Function(void);

static Bool Write_Data_Modify(void);

static Bool Where(void);

static Bool What(void);

static Bool Dereference(void);

static Bool Environment(void);

static Bool Backtrack(void);

static WamWord *Read_Bank_Adr(Bool only_stack, int arg_nb, char **bank_name);

static long Read_An_Integer(int arg_nb);

static void Print_Bank_Name_Offset(char *prefix, char *bank_name, int offset);

static void Print_Wam_Word(WamWord *word_adr);

static void Modify_Wam_Word(WamWord *word_adr);

static WamWord *Detect_Stack(WamWord *adr, char **stack_name);

static PredInf *Detect_Pred_From_Code(long *codep);

static Bool Help(void);



#define INIT_DEBUGGER              X24696E69745F6465627567676572

#define DEBUG_CALL                 X2464656275675F63616C6C

Prolog_Prototype(INIT_DEBUGGER, 0);
Prolog_Prototype(DEBUG_CALL, 2);




/*-------------------------------------------------------------------------*
 * DEBUG_INITIALIZER                                                       *
 *                                                                         *
 * Calls '$init_debugger' and reset the heap actual start (cf. engine.c).  *
 * '$init_debugger' is not called via a directive :- initialize to avoid to*
 * count it in Exec_Directive() (cf engine.c).                             *
 * However, it cannot be called directly since we do not if the initializer*
 * of g_var_inl_c.c has been executed ('$init_debugger' uses g_assign).    *
 * We thus act like a Prolog object, calling New_Object.                   *
 *-------------------------------------------------------------------------*/
static void
Debug_Initializer(void)
{
  New_Object(My_System_Directives, NULL);
}


static void
My_System_Directives(void)
{
  Call_Prolog(Prolog_Predicate(INIT_DEBUGGER, 0));
  
  Set_Heap_Actual_Start(H);	/* changed to store global info */
}




/*-------------------------------------------------------------------------*
 * SET_DEBUG_CALL_CODE_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Debug_Call_Code_0(void)
{
  debug_call_code = Prolog_Predicate(DEBUG_CALL, 2);

  Flag_Value(FLAG_DEBUG) = TRUE;
}




/*-------------------------------------------------------------------------*
 * RESET_DEBUG_CALL_CODE_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Reset_Debug_Call_Code_0(void)
{
  debug_call_code = NULL;
  Flag_Value(FLAG_DEBUG) = FALSE;
}




/*-------------------------------------------------------------------------*
 * REMOVE_ONE_CHOICE_POINT_1                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Remove_One_Choice_Point_1(WamWord b_word)
{
  WamWord word, tag_mask;
  WamWord *b;
   
  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  Assign_B(BB(b));
}




/*-------------------------------------------------------------------------*
 * CHOICE_POINT_INFO_4                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Choice_Point_Info_4(WamWord b_word, WamWord name_word, WamWord arity_word,
		    WamWord lastb_word)
{
  WamWord word, tag_mask;
  WamWord *b;
  HashScan scan;
  PredInf *pred;
  PredInf *last_pred;
  WamCont code, code1;
  WamCont last_code = 0;
  int func, arity;


  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  code = (WamCont) ALTB(b);

  for (pred = (PredInf *) Hash_First(pred_tbl, &scan); pred;
       pred = (PredInf *) Hash_Next(&scan))
    {
      code1 = (WamCont) (pred->codep);
      if (code >= code1 && code1 >= last_code)
	{
	  last_pred = pred;
	  last_code = code1;
	}
    }

  func = Functor_Of(last_pred->f_n);
  arity = Arity_Of(last_pred->f_n);

  Get_Atom(func, name_word);
  Get_Integer(arity, arity_word);
  Unify(From_B_To_WamWord(BB(b)), lastb_word);
}




/*-------------------------------------------------------------------------*
 * SCAN_CHOICE_POINT_INFO_3                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Scan_Choice_Point_Info_3(WamWord b_word, WamWord name_word,
			 WamWord arity_word)
{
  WamWord word, tag_mask;
  WamWord *b;
  int func, arity;

  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  func = Scan_Choice_Point_Pred(b, &arity);
  if (func < 0)
    return FALSE;

  Get_Atom(func, name_word);
  Get_Integer(arity, arity_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * CHOICE_POINT_ARG_3                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Choice_Point_Arg_3(WamWord b_word, WamWord i_word, WamWord arg_word)
{
  WamWord word, tag_mask;
  WamWord *b;
  int i;


  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  DEREF(i_word, word, tag_mask);
  i = UnTag_INT(word) - 1;

  Unify(arg_word, AB(b, i));
}




/*-------------------------------------------------------------------------*
 * DEBUG_WAM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Debug_Wam(void)
{
  FctPtr command;
  char str[80] = "";


  pstm_i = stm_tbl + stm_debugger_input;
  pstm_o = stm_tbl + stm_debugger_output;

  for (;;)
    {
      Stream_Printf(pstm_o, "\nwam debugger> ");

      if (Stream_Gets(str, sizeof(str) - 1, pstm_i) == NULL)
	break;

      Scan_Command(str);
      command = Find_Function();
      if (command == (FctPtr) -1)
	break;

      if (command)
	(*command) ();
    }
}




/*-------------------------------------------------------------------------*
 * SCAN_COMMAND                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Scan_Command(char *source_str)
{
  char str[80];
  char *p, *q;

  strcpy(str, source_str);
  nb_read_arg = 0;
  p = (char *) strtok(str, SEPARATOR_LIST);

  while (p)
    {
      q = p;
      p = (char *) strtok(NULL, SEPARATOR_LIST);
      strcpy(read_arg[nb_read_arg++], q);
    }
}




/*-------------------------------------------------------------------------*
 * FIND_FUNCTION                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static FctPtr
Find_Function(void)
{
  int lg;
  int i;
  static InfCmd cmd[] = { {"write", Write_Data_Modify},
  {"data", Write_Data_Modify},
  {"modify", Write_Data_Modify},
  {"where", Where},
  {"what", What},
  {"deref", Dereference},
  {"envir", Environment},
  {"backtrack", Backtrack},
  {"quit", (FctPtr) -1},
  {"help", Help}
  };

  if (nb_read_arg == 0)
    return NULL;

  lg = strlen(read_arg[0]);

  for (i = 0; i < sizeof(cmd) / sizeof(InfCmd); i++)
    if (strncmp(cmd[i].name, read_arg[0], lg) == 0)
      return cmd[i].fct;

  Stream_Printf(pstm_o, "Unknown command - try help\n");

  return NULL;
}




/*-------------------------------------------------------------------------*
 * WRITE_DATA_MODIFY                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Write_Data_Modify(void)
{
  WamWord *adr;
  char *bank_name;
  int offset;
  int nb;

  if ((adr = Read_Bank_Adr(FALSE, 1, &bank_name)) != NULL)
    {
      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);
      nb = (nb_read_arg < 4) ? 1 : Read_An_Integer(3);

      if (adr == reg_copy)
	{
	  if (offset >= NB_OF_REGS)
	    offset = 0;

	  if (nb_read_arg < 4 && *read_arg[0] != 'm')
	    nb = NB_OF_REGS - offset;
	  else if (nb > NB_OF_REGS - offset)
	    nb = NB_OF_REGS - offset;
	}

      while (nb--)
	{
	  Print_Bank_Name_Offset((adr == reg_copy) ? reg_tbl[offset] : "",
				 bank_name, offset);
	  Stream_Printf(pstm_o, ":");

	  if (*read_arg[0] == 'w')
	    Write_Term(pstm_o, -1,
		       MAX_PREC, WRITE_NUMBER_VARS | WRITE_NAME_VARS,
		       adr[offset]);
	  else
	    {
	      Print_Wam_Word(adr + offset);
	      if (*read_arg[0] == 'm')
		Modify_Wam_Word(adr + offset);
	    }

	  Stream_Printf(pstm_o, "\n");
	  offset++;
	}
    }


  if (adr == reg_copy)		/* saved by Read_Bank_Adr */
    Restore_All_Regs(reg_copy);

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * WHAT                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
What(void)
{
  long *adr;
  WamWord *adr1;
  char *stack_name;
  PredInf *pred;
  int func, arity;


  if (nb_read_arg < 2)
    {
      Stream_Printf(pstm_o, "integer expected\n");
      return FALSE;
    }

  adr = (long *) Read_An_Integer(1);

  Stream_Printf(pstm_o, " %#lx = ", (long) adr);
  if ((adr1 = Detect_Stack(adr, &stack_name)) != NULL)
    {
      Print_Bank_Name_Offset("", stack_name, adr - adr1);
      Stream_Printf(pstm_o, "\n");
      return FALSE;
    }

  if ((pred = Detect_Pred_From_Code(adr)) != NULL)
    {
      func = Functor_Of(pred->f_n);
      arity = Arity_Of(pred->f_n);
      Stream_Printf(pstm_o, "%s/%d", atom_tbl[func].name, arity);
      if (adr > pred->codep)
	Stream_Printf(pstm_o, "+%d", (char *) adr - (char *) (pred->codep));
      Stream_Printf(pstm_o, "\n");
      return FALSE;
    }

  Stream_Printf(pstm_o, "???\n");

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * WHERE                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Where(void)
{
  char *stack_name;
  int offset;
  WamWord *adr;

  if ((adr = Read_Bank_Adr(TRUE, 1, &stack_name)) != NULL)
    {
      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);

      Print_Bank_Name_Offset((adr == reg_copy) ? reg_tbl[offset] : "",
			     stack_name, offset);
      Stream_Printf(pstm_o, " at %#lx\n", (long) (adr + offset));
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * DEREFERENCE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Dereference(void)
{
  char *bank_name;
  char *stack_name;
  int offset;
  WamWord word, tag_mask;
  WamWord word1, *d_adr;
  WamWord *adr;

  if ((adr = Read_Bank_Adr(FALSE, 1, &bank_name)) != NULL)
    {
      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);

				/* my own DEREF here to get the address */
      d_adr = NULL;		/* added this */
      word = adr[offset];
      do
	{
	  word1 = word;
	  tag_mask = Tag_Mask_Of(word);
	  if (tag_mask != TAG_REF_MASK)
	    break;

	  d_adr = UnTag_REF(word); /* added this */
	  word = *d_adr;
	}
      while (word != word1);

      Print_Bank_Name_Offset((adr == reg_copy) ? reg_tbl[offset] : "",
			     bank_name, offset);
      Stream_Printf(pstm_o, ":");

      if (d_adr && (adr = Detect_Stack(d_adr, &stack_name)) != NULL)
	{
	  Stream_Printf(pstm_o, " --> \n");
	  Print_Bank_Name_Offset("", stack_name, d_adr - adr);
	  Stream_Printf(pstm_o, ":");
	}

      Print_Wam_Word(d_adr ? d_adr : adr + offset);
      Stream_Printf(pstm_o, "\n");
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * ENVIRONMENT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Environment(void)
{
  WamWord *adr;
  int offset;
  char *stack_name;
  int i;

  if (nb_read_arg == 1)
    {
      adr = Detect_Stack(E, &stack_name);
      offset = E - adr;
      adr = E;
    }
  else
    {
      if ((adr = Read_Bank_Adr(TRUE, 1, &stack_name)) == NULL)
	return FALSE;

      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);
      adr += offset;
    }

  for (i = ENVIR_STATIC_SIZE; i > 0; i--)
    {
      Print_Bank_Name_Offset(envir_name[i - 1], stack_name, offset - i);
      Stream_Printf(pstm_o, ":");
      Print_Wam_Word(adr - i);
      Stream_Printf(pstm_o, "\n");
    }

  return FALSE;
}



/*-------------------------------------------------------------------------*
 * BACKTRACK                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Backtrack(void)
{
  WamWord *adr;
  int offset;
  char *stack_name;
  int i;
  PredInf *pred;
  int func, arity;


  if (nb_read_arg == 2
      && strncmp(read_arg[1], "all", strlen(read_arg[1])) == 0)
    {
      Detect_Stack(B, &stack_name);
      for (adr = B; adr > Local_Stack + 10; adr = BB(adr))
	{
	  pred = Detect_Pred_From_Code((long *) ALTB(adr));

	  func = Functor_Of(pred->f_n);
	  arity = Arity_Of(pred->f_n);
	  Print_Bank_Name_Offset("", stack_name, adr - Local_Stack);
	  Stream_Printf(pstm_o, ": %s/%d", atom_tbl[func].name, arity);
	  if (arity == 0 && strcmp(atom_tbl[func].name, "$clause_alt") == 0)
	    {
	      Stream_Printf(pstm_o, " for ");
	      Write_Term(pstm_o, -1, MAX_PREC,
			 WRITE_NUMBER_VARS | WRITE_NAME_VARS, AB(adr, 0));
	    }

	  Stream_Printf(pstm_o, "\n");
	}

      return FALSE;
    }


  if (nb_read_arg == 1)
    {
      adr = Detect_Stack(B, &stack_name);
      offset = B - adr;
      adr = B;
    }
  else
    {
      if ((adr = Read_Bank_Adr(TRUE, 1, &stack_name)) == NULL)
	return FALSE;

      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);
      adr += offset;
    }

  pred = Detect_Pred_From_Code((long *) ALTB(adr));

  func = Functor_Of(pred->f_n);
  arity = Arity_Of(pred->f_n);
  Stream_Printf(pstm_o, "Created by  %s/%d", atom_tbl[func].name, arity);
  if (arity == 0 && strcmp(atom_tbl[func].name, "$clause_alt") == 0)
    {
      Stream_Printf(pstm_o, " for ");
      Write_Term(pstm_o, -1, MAX_PREC, WRITE_NUMBER_VARS | WRITE_NAME_VARS,
		 AB(adr, 0));
    }
  Stream_Printf(pstm_o, "\n");

  for (i = CHOICE_STATIC_SIZE; i > 0; i--)
    {
      Print_Bank_Name_Offset(choice_name[i - 1], stack_name, offset - i);
      Stream_Printf(pstm_o, ":");
      Print_Wam_Word(adr - i);
      Stream_Printf(pstm_o, "\n");
    }

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * READ_BANK_ADR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord *
Read_Bank_Adr(Bool only_stack, int arg_nb, char **bank_name)
{
  int lg;
  int i;

  if (nb_read_arg < arg_nb + 1)
    {
      Stream_Printf(pstm_o, "%s name expected\n", (only_stack) ? "Stack"
		    : "Bank");
      return NULL;
    }

  lg = strlen(read_arg[arg_nb]);

  if (!only_stack)
    {
      if (read_arg[arg_nb][0] == 'x' && lg == 1)
	{
	  *bank_name = "x";
	  return &X(0);
	}

      if (read_arg[arg_nb][0] == 'y' && lg == 1)
	{
	  *bank_name = "y";
	  return &Y(E, 0);
	}

      if (strncmp("reg", read_arg[arg_nb], lg) == 0)
	{
	  *bank_name = "reg";
	  Save_All_Regs(reg_copy);
	  return reg_copy;
	}
    }


  for (i = 0; i < NB_OF_STACKS; i++)
    if (strncmp(stk_tbl[i].name, read_arg[arg_nb], lg) == 0)
      {
	*bank_name = stk_tbl[i].name;
	return stk_tbl[i].stack;
      }

  Stream_Printf(pstm_o, "Incorrect %s name\n",
		(only_stack) ? "stack" : "bank");

  return NULL;
}




/*-------------------------------------------------------------------------*
 * READ_AN_INTEGER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static long
Read_An_Integer(int arg_nb)
{
  char *p;
  long val = 0;

  val = strtol(read_arg[arg_nb], &p, 0);
  if (*p)
    Stream_Printf(pstm_o, "Incorrect integer\n");

  return val;
}




/*-------------------------------------------------------------------------*
 * PRINT_BANK_NAME_OFFSET                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Print_Bank_Name_Offset(char *prefix, char *bank_name, int offset)
{
  char str[80];
  int lg = strlen(prefix);

  if (lg)
    Stream_Printf(pstm_o, "%s", prefix);

  sprintf(str, "%s[%d]", bank_name, offset);
  lg += strlen(str);

  if (lg > BANK_NAME_OFFSET_LENGTH)
    lg = BANK_NAME_OFFSET_LENGTH;

  Stream_Printf(pstm_o, "%*s%s", BANK_NAME_OFFSET_LENGTH - lg, "", str);
}




/*-------------------------------------------------------------------------*
 * PRINT_WAM_WORD                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Print_Wam_Word(WamWord *word_adr)
{
  WamWord word = *word_adr;
  WamWord tag;
  WamWord value;
  char *stack_name;
  WamWord *adr;
  int functor;
  int arity;

  Stream_Printf(pstm_o, "%#*lx  %*ld  ", HEXADECIMAL_LENGTH, (long) word,
		DECIMAL_LENGTH, (long) word);

  if ((adr = Detect_Stack((WamWord *) word, &stack_name)) != NULL)
    Print_Bank_Name_Offset("", stack_name, (WamWord *) word - adr);
  else
    Stream_Printf(pstm_o, "%*s", BANK_NAME_OFFSET_LENGTH, "?[?]");

  Stream_Printf(pstm_o, "  ");

  tag = Tag_Of(word);
  if (tag < NB_OF_TAGS)
    {
      switch (tag_tbl[tag].type)
	{
	case INTEGER:
	  value = (WamWord) UnTag_Integer(word);
	  Stream_Printf(pstm_o, "%s,%*ld", tag_tbl[tag].name,
			VALUE_PART_LENGTH, (long) value);
	  break;

	case SHORT_UNS:
	  value = (WamWord) UnTag_Short_Uns(word);
	  if (tag == ATM && value >= 0 && value < MAX_ATOM &&
	      atom_tbl[value].name != NULL)
	    Stream_Printf(pstm_o, "ATM,%*s (%ld)",
			  VALUE_PART_LENGTH, atom_tbl[value].name,
			  (long) value);
	  else
	    Stream_Printf(pstm_o, "%s,%*lu", tag_tbl[tag].name,
			  VALUE_PART_LENGTH, (long) value);
	  break;

	case ADDRESS:
	  value = (WamWord) UnTag_Address(word);
	  if ((adr = Detect_Stack((WamWord *) value, &stack_name)) != NULL)
	    {
	      Stream_Printf(pstm_o, "%s,", tag_tbl[tag].name);
	      Print_Bank_Name_Offset("", stack_name,
				     (WamWord *) value - adr);
	    }
	  else
	    tag = -1;
	  break;
	}
    }
  else
    tag = -1;

  if (tag == -1)
    Stream_Printf(pstm_o, "???,%*s", VALUE_PART_LENGTH, "?");

  Stream_Printf(pstm_o, "  ");

  if (word_adr >= Trail_Stack && word_adr < Trail_Stack + Trail_Size)
    {
      tag = Trail_Tag_Of(word);
      value = Trail_Value_Of(word);

      if (tag == TFC)
	Stream_Printf(pstm_o, "%s,%#*lx", trail_tag_name[tag],
		      VALUE_PART_LENGTH, (long) value);
      else
	if (tag < NB_OF_TRAIL_TAGS &&
	    (adr = Detect_Stack((WamWord *) value, &stack_name)) != NULL &&
	    *stack_name != 't')
	{
	  Stream_Printf(pstm_o, "%s,", trail_tag_name[tag]);
	  Print_Bank_Name_Offset("", stack_name, (WamWord *) value - adr);
	}
      else
	Stream_Printf(pstm_o, "???,%*s", VALUE_PART_LENGTH, "?");

      Stream_Printf(pstm_o, "  ");
    }

  functor = Functor_Of(word);
  arity = Arity_Of(word);

  if (functor >= 0 && functor < MAX_ATOM && atom_tbl[functor].name != NULL
      && arity >= 0 && arity <= MAX_ARITY)
    Stream_Printf(pstm_o, "%12s/%-3d", atom_tbl[functor].name, arity);
}




/*-------------------------------------------------------------------------*
 * MODIFY_WAM_WORD                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Modify_Wam_Word(WamWord *word_adr)
{
  WamWord word;
  char *bank_name;
  WamWord *adr;
  int offset;
  char str[80];
  char *comma;
  char *slash;
  char *p;
  int i, j;

  for (;;)
    {
      Stream_Printf(pstm_o, "\nNew value: ");

      if (Stream_Gets(str, sizeof(str) - 1, pstm_i) == NULL ||
	  *str == '\0' || *str == '\n')
	break;

      Scan_Command(str);

      if ((comma = (char *) strchr(str, ',')) != NULL)
	goto tag_value;

      if ((slash = (char *) strchr(read_arg[0], '/')) != NULL)
	goto functor_arity;


      /* integer */
      if (nb_read_arg == 1 && *read_arg[0] >= '0' && *read_arg[0] <= '9')
	{
	  word = strtol(read_arg[0], &p, 0);
	  if (*p == '\0')
	    {
	      *word_adr = word;
	      return;
	    }
	  else
	    goto err;
	}


      /* stack address */
      if ((adr = Read_Bank_Adr(TRUE, 0, &bank_name)) != NULL)
	{
	  offset = (nb_read_arg < 2) ? 0 : Read_An_Integer(1);
	  *word_adr = (WamWord) (adr + offset);
	  return;
	}

      goto err;


      /* tag,value */
    tag_value:
      comma++;

      for (i = 0; i < NB_OF_TAGS; i++)
	if (strcmp(tag_tbl[i].name, read_arg[0]) == 0)
	  break;

      if (i < NB_OF_TAGS)
	{
	  switch (tag_tbl[i].type)
	    {
	    case INTEGER:
	      word = strtol(comma, &p, 0);
	      if (*p != '\0')
		goto err;

	      *word_adr = Tag_Integer(tag_tbl[i].tag_mask, Read_An_Integer(1));
	      return;

	    case SHORT_UNS:
	      word = strtol(comma, &p, 0);
	      if (*p == '\0')
		j = Read_An_Integer(1);
	      else if (strcmp(read_arg[0], "ATM") == 0)
		  j = Create_Allocate_Atom(comma);
	      else
		goto err;
	      
	      *word_adr = Tag_Short_Uns(tag_tbl[i].tag_mask, j);
	      return;

	    case ADDRESS:
	      if ((adr = Read_Bank_Adr(TRUE, 1, &bank_name)) != NULL)
		{
		  offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);
		  *word_adr = Tag_Address(tag_tbl[i].tag_mask, adr + offset);
		  return;
		}
	      goto err;
	    }
	}


      /* trail_tag,value */
      for (i = 0; i < NB_OF_TRAIL_TAGS; i++)
	if (strcmp(trail_tag_name[i], read_arg[0]) == 0)
	  if ((adr = Read_Bank_Adr(TRUE, 1, &bank_name)) != NULL)
	    {
	      offset = (nb_read_arg < 3) ? 0 : Read_An_Integer(2);
	      *word_adr = Trail_Tag_Value(i, adr + offset);
	      return;
	    }

      goto err;


      /* functor/arity */
    functor_arity:
      *slash = '\0';
      i = strtol(slash + 1, &p, 0);
      if (*p != '\0' || i < 1 || i > MAX_ARITY)
	goto err;

      word = strtol(read_arg[0], &p, 0);
      if (*p != '\0')
	word = (long) Create_Allocate_Atom(read_arg[0]);
      else if (word < 0 || word >= MAX_ATOM)
	goto err;

      *word_adr = Functor_Arity(word, i);
      Stream_Printf(pstm_o, "--> %s/%d", atom_tbl[word].name, i);
      return;


    err:
      Stream_Printf(pstm_o, "Error...");
    }
}




/*-------------------------------------------------------------------------*
 * DETECT_STACK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord *
Detect_Stack(WamWord *adr, char **stack_name)
{
  int i;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (adr >= stk_tbl[i].stack && adr < stk_tbl[i].stack + stk_tbl[i].size)
      {
	*stack_name = stk_tbl[i].name;
	return stk_tbl[i].stack;
      }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * DETECT_PRED_FROM_CODE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PredInf *
Detect_Pred_From_Code(long *codep)
{
  HashScan scan;
  PredInf *pred;
  PredInf *last_pred = NULL;
  long dist, d;

  for (pred = (PredInf *) Hash_First(pred_tbl, &scan); pred;
       pred = (PredInf *) Hash_Next(&scan))
    {
      d = codep - pred->codep;

      if ((pred->prop & MASK_PRED_DYNAMIC) || d < 0)
	continue;

      if (last_pred == NULL || d < dist)
	{
	  last_pred = pred;
	  dist = d;
	}
    }

  return last_pred;
}




/*-------------------------------------------------------------------------*
 * HELP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Help(void)
{
  int i;

#define L(str)  Stream_Printf(pstm_o,"%s\n",str)


  L("Wam debugging options:");
  L("");
  L("   write     A [N] write   N (or 1) Prolog terms starting at A");
  L("   data      A [N] display N (or 1) words starting at A");
  L("   modify    A [N] display and modify N (or 1) words starting at A");
  L("   where     SA    display the real address corresponding to SA");
  L("   what      RA    display what corresponds to the real address RA");
  L("   deref     A     display the dereferenced word starting at A");
  L("   envir     [SA]  display an environment located at SA (or current)");
  L("   backtrack [SA]  display a choice point located at SA (or current)");
  L("   backtrack all   display all choice points");
  L("   quit            return to Prolog debugger");
  L("");
  L("A WAM address (A) has the following syntax: bank_name [N]");
  L("   bank_name  is either reg/x/y/stack_name (see below)");
  L("   N          is an optional index (default 0)");
  Stream_Printf(pstm_o, "   stack_name is either:");

  for (i = 0; i < NB_OF_STACKS; i++)
    Stream_Printf(pstm_o, " %s", stk_tbl[i].name);

  Stream_Printf(pstm_o, "\n");
  L("");
  L("A WAM stack address (SA) has the following syntax: stack_name [N]");
  L("");
  L("A real address (RA) is a C integer (0x... notation is allowed)");

  return FALSE;
}
