/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : error_supp.c                                                    *
 * Descr.: Prolog errors support                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

#include <errno.h>
#include <string.h>

#define OBJ_INIT Error_Supp_Initializer

#define ERROR_SUPP_FILE

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

static int cur_bip_func;
static int cur_bip_arity;

static char *c_bip_func_str;
static int c_bip_arity;


static char *last_err_file = NULL;
static int last_err_line;
static int last_err_col;
static char *last_err_msg;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Update_Cur_From_C_Bip(void);

static char *Context_Error_String(void);



#define PL_ERR_INSTANTIATION       X24706C5F6572725F696E7374616E74696174696F6E

#define PL_ERR_TYPE                X24706C5F6572725F74797065

#define PL_ERR_DOMAIN              X24706C5F6572725F646F6D61696E

#define PL_ERR_EXISTENCE           X24706C5F6572725F6578697374656E6365

#define PL_ERR_PERMISSION          X24706C5F6572725F7065726D697373696F6E

#define PL_ERR_REPRESENTATION      X24706C5F6572725F726570726573656E746174696F6E

#define PL_ERR_EVALUATION          X24706C5F6572725F6576616C756174696F6E

#define PL_ERR_RESOURCE            X24706C5F6572725F7265736F75726365

#define PL_ERR_SYNTAX              X24706C5F6572725F73796E746178

#define PL_ERR_SYSTEM              X24706C5F6572725F73797374656D



Prolog_Prototype(PL_ERR_INSTANTIATION, 0);
Prolog_Prototype(PL_ERR_TYPE, 2);
Prolog_Prototype(PL_ERR_DOMAIN, 2);
Prolog_Prototype(PL_ERR_EXISTENCE, 2);
Prolog_Prototype(PL_ERR_PERMISSION, 3);
Prolog_Prototype(PL_ERR_REPRESENTATION, 1);
Prolog_Prototype(PL_ERR_EVALUATION, 1);
Prolog_Prototype(PL_ERR_RESOURCE, 1);
Prolog_Prototype(PL_ERR_SYNTAX, 1);
Prolog_Prototype(PL_ERR_SYSTEM, 1);




/*-------------------------------------------------------------------------*
 * ERROR_SUPP_INITIALIZER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Error_Supp_Initializer(void)
{
  type_atom = Create_Atom("atom");
  type_atomic = Create_Atom("atomic");
  type_byte = Create_Atom("byte");
  type_callable = Create_Atom("callable");
  type_character = Create_Atom("character");
  type_compound = Create_Atom("compound");
  type_evaluable = Create_Atom("evaluable");
  type_float = Create_Atom("float"); /* for arithmetic */
  type_boolean = Create_Atom("boolean"); /* for setarg/4 */
  type_in_byte = Create_Atom("in_byte");
  type_in_character = Create_Atom("in_character");
  type_integer = Create_Atom("integer");
  type_list = Create_Atom("list");
  type_number = Create_Atom("number");
  type_predicate_indicator = Create_Atom("predicate_indicator");
  type_variable = Create_Atom("variable");
  if (fd_init_solver)		/* FD solver linked */
    {
      type_fd_variable = Create_Atom("fd_variable"); /* for FD */
      type_fd_evaluable = Create_Atom("fd_evaluable"); /* for FD */
      type_fd_bool_evaluable = Create_Atom("fd_bool_evaluable"); /* for FD */
    }

  domain_character_code_list = Create_Atom("character_code_list");
  domain_close_option = Create_Atom("close_option");
  domain_flag_value = Create_Atom("flag_value");
  domain_io_mode = Create_Atom("io_mode");
  domain_non_empty_list = Create_Atom("non_empty_list");
  domain_not_less_than_zero = Create_Atom("not_less_than_zero");
  domain_operator_priority = Create_Atom("operator_priority");
  domain_operator_specifier = Create_Atom("operator_specifier");
  domain_prolog_flag = Create_Atom("prolog_flag");
  domain_read_option = Create_Atom("read_option");
  domain_source_sink = Create_Atom("source_sink");
  domain_stream = Create_Atom("stream");
  domain_stream_option = Create_Atom("stream_option");
  domain_stream_or_alias = Create_Atom("stream_or_alias");
  domain_stream_position = Create_Atom("stream_position");
  domain_stream_property = Create_Atom("stream_property");
  domain_write_option = Create_Atom("write_option");
  domain_term_stream_or_alias = Create_Atom("term_stream_or_alias");
				/* for term_streams */
  domain_g_array_index = Create_Atom("g_array_index");/* for g_vars */
  domain_g_argument_selector = Create_Atom("g_argument_selector");
				/* for g_vars */
  domain_stream_seek_method = Create_Atom("stream_seek_method"); /* for seek/4 */
  domain_format_control_sequence = Create_Atom("format_control_sequence");
				/* for format/2-3 */
  domain_os_path = Create_Atom("os_path");/* for absolute_file_name/2 */
  domain_os_file_permission = Create_Atom("os_file_permission");
				/* for file_permission/2 */
  domain_date_time = Create_Atom("date_time");/* for os_interf */
  domain_selectable_item = Create_Atom("selectable_item");
				/* for select_read/3 */
#ifndef NO_USE_SOCKETS
  domain_socket_domain = Create_Atom("socket_domain"); /* for sockets */
  domain_socket_address = Create_Atom("socket_address"); /* for sockets */
#endif

  existence_procedure = Create_Atom("procedure");
  existence_source_sink = Create_Atom("source_sink");
  existence_stream = Create_Atom("stream");
  existence_sr_descriptor = Create_Atom("sr_descriptor");
				/* for source reader */


  permission_operation_access = Create_Atom("access");
  permission_operation_close = Create_Atom("close");
  permission_operation_create = Create_Atom("create");
  permission_operation_input = Create_Atom("input");
  permission_operation_modify = Create_Atom("modify");
  permission_operation_open = Create_Atom("open");
  permission_operation_output = Create_Atom("output");
  permission_operation_reposition = Create_Atom("reposition");


  permission_type_binary_stream = Create_Atom("binary_stream");
  permission_type_flag = Create_Atom("flag");
  permission_type_operator = Create_Atom("operator");
  permission_type_past_end_of_stream = Create_Atom("past_end_of_stream");
  permission_type_private_procedure = Create_Atom("private_procedure");
  permission_type_static_procedure = Create_Atom("static_procedure");
  permission_type_source_sink = Create_Atom("source_sink");
  permission_type_stream = Create_Atom("stream");
  permission_type_text_stream = Create_Atom("text_stream");


  representation_character = Create_Atom("character");
  representation_character_code = Create_Atom("character_code");
  representation_in_character_code = Create_Atom("in_character_code");
  representation_max_arity = Create_Atom("max_arity");
  representation_max_integer = Create_Atom("max_integer");
  representation_min_integer = Create_Atom("min_integer");
  representation_too_many_variables = Create_Atom("too_many_variables");
				/* for Copy_Term(),... */

  evluation_float_overflow = Create_Atom("float_overflow");
  evluation_int_overflow = Create_Atom("int_overflow");
  evluation_undefined = Create_Atom("undefined");
  evluation_underflow = Create_Atom("underflow");
  evluation_zero_divisor = Create_Atom("zero_divisor");


  resource_print_object_not_linked = Create_Atom("print_object_not_linked");
				/* for print and format */
  if (fd_init_solver)		/* FD solver linked */
    {
      resource_too_big_fd_constraint = Create_Atom("too_big_fd_constraint");
				/* for FD */
    }
}




/*-------------------------------------------------------------------------*
 * SET_BIP_NAME_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Bip_Name_2(WamWord func_word, WamWord arity_word)
{
  Set_C_Bip_Name("set_bip_name", 2);

  cur_bip_func = Rd_Atom_Check(func_word);
  cur_bip_arity = Rd_Integer_Check(arity_word);

  Unset_C_Bip_Name();

  c_bip_func_str = NULL;
}




/*-------------------------------------------------------------------------*
 * CURRENT_BIP_NAME_2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Bip_Name_2(WamWord func_word, WamWord arity_word)
{
  Bool res;

  Set_C_Bip_Name("current_bip_name", 2);

  res = Un_Atom_Check(cur_bip_func, func_word) &&
    Un_Integer_Check(cur_bip_arity, arity_word);

  Unset_C_Bip_Name();

  return res;
}




/*-------------------------------------------------------------------------*
 * SET_C_BIP_NAME                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Set_C_Bip_Name(char *func_str, int arity)
{
  c_bip_func_str = func_str;
  c_bip_arity = arity;
}




/*-------------------------------------------------------------------------*
 * UNSET_C_BIP_NAME                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unset_C_Bip_Name(void)
{
  c_bip_func_str = NULL;
}




/*-------------------------------------------------------------------------*
 * GET_CURRENT_BIP                                                         *
 *                                                                         *
 * returns func and initializes arity of the current bip.                  *
 *-------------------------------------------------------------------------*/
int
Get_Current_Bip(int *arity)
{
  Update_Cur_From_C_Bip();

  *arity = cur_bip_arity;
  return cur_bip_func;
}




/*-------------------------------------------------------------------------*
 * UPDATE_CUR_FROM_C_BIP                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Update_Cur_From_C_Bip(void)
{
  if (c_bip_func_str)
    {
      cur_bip_func = Create_Allocate_Atom(c_bip_func_str);
      cur_bip_arity = c_bip_arity;
    }
}




/*-------------------------------------------------------------------------*
 * CONTEXT_ERROR_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Context_Error_1(WamWord err_word)
{
  if (cur_bip_arity >= 0)
    {
      Get_Structure(ATOM_CHAR('/'), 2, err_word);
      Unify_Atom(cur_bip_func);
      Unify_Integer(cur_bip_arity);
    }
  else
    Get_Atom(cur_bip_func, err_word);
}




/*-------------------------------------------------------------------------*
 * CONTEXT_ERROR_STRING                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Context_Error_String(void)
{
  static char buff[256];

  sprintf(buff, "%s", atom_tbl[cur_bip_func].name);
  if (cur_bip_arity >= 0)
    sprintf(buff + strlen(buff), "/%d", cur_bip_arity);

  return buff;
}




/*-------------------------------------------------------------------------*
 * SET_LAST_SYNTAX_ERROR                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Last_Syntax_Error(char *file_name, int err_line, int err_col,
		      char *err_msg)
{
  last_err_file = file_name;
  last_err_line = err_line;
  last_err_col = err_col;
  last_err_msg = err_msg;
}




/*-------------------------------------------------------------------------*
 * SYNTAX_ERROR_INFO_4                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Syntax_Error_Info_4(WamWord file_name_word,
		    WamWord line_word, WamWord char_word, WamWord msg_word)
{
  Check_For_Un_Atom(file_name_word);
  Check_For_Un_Integer(line_word);
  Check_For_Un_Integer(char_word);
  Check_For_Un_Atom(msg_word);

  return last_err_file &&
    Un_String(last_err_file, file_name_word) &&
    Un_Integer(last_err_line, line_word) &&
    Un_Integer(last_err_col, char_word) &&
    Un_String(last_err_msg, msg_word);
}




/*-------------------------------------------------------------------------*
 * SYNTAX_ERROR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Syntax_Error(int flag_value)
{
  char str[512];

  if (last_err_file == NULL || *last_err_file == '\0')
    sprintf(str, "(char:%d) %s", last_err_col, last_err_msg);
  else
    sprintf(str, "%s:%d (char:%d) %s", last_err_file,
	    last_err_line, last_err_col, last_err_msg);

  if (flag_value == FLAG_VALUE_ERROR)
    Pl_Err_Syntax(Create_Allocate_Atom(str));

  Update_Cur_From_C_Bip();
  if (flag_value == FLAG_VALUE_WARNING)
    Stream_Printf(stm_tbl[stm_top_level_output],
		  "warning: syntax error: %s (from %s)\n",
		  str, Context_Error_String());
}




/*-------------------------------------------------------------------------*
 * UNKNOWN_PRED_ERROR                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Unknown_Pred_Error(int func, int arity)
{
  WamWord term;

  if (Flag_Value(FLAG_UNKNOWN) == FLAG_VALUE_ERROR)
    {
      term = Put_Structure(ATOM_CHAR('/'), 2);
      Unify_Atom(func);
      Unify_Integer(arity);
      Pl_Err_Existence(existence_procedure, term);
    }

  Update_Cur_From_C_Bip();
  if (Flag_Value(FLAG_UNKNOWN) == FLAG_VALUE_WARNING)
    Stream_Printf(stm_tbl[stm_top_level_output],
		  "warning: unknown procedure %s/%d (from %s)\n",
		  atom_tbl[func].name, arity, Context_Error_String());
}




/*-------------------------------------------------------------------------*
 * OS_ERROR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Os_Error(void)
{
  char *err_str = M_Sys_Err_String(errno);

  if (Flag_Value(FLAG_OS_ERROR) == FLAG_VALUE_ERROR)
    Pl_Err_System(Create_Allocate_Atom(err_str));

  Update_Cur_From_C_Bip();
  if (Flag_Value(FLAG_OS_ERROR) == FLAG_VALUE_WARNING)
    Stream_Printf(stm_tbl[stm_top_level_output],
		  "warning: OS error: %s (from %s)\n",
		  err_str, Context_Error_String());
}




/*-------------------------------------------------------------------------*
 * PL_ERR_INSTANTIATION                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Instantiation(void)
{
  Update_Cur_From_C_Bip();
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_INSTANTIATION, 0));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_TYPE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Type(int atom_type, WamWord term)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_type);
  A(1) = term;
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_TYPE, 2));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_DOMAIN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Domain(int atom_domain, WamWord term)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_domain);
  A(1) = term;
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_DOMAIN, 2));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_EXISTENCE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Existence(int atom_object, WamWord term)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_object);
  A(1) = term;
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_EXISTENCE, 2));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_PERMISSION                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Permission(int atom_oper, int atom_perm, WamWord term)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_oper);
  A(1) = Tag_ATM(atom_perm);
  A(2) = term;
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_PERMISSION, 3));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_REPRESENTATION                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Representation(int atom_flag)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_flag);
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_REPRESENTATION, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_EVALUATION                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Evaluation(int atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_error);
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_EVALUATION, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_RESOURCE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Resource(int atom_resource)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_resource);
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_RESOURCE, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_SYNTAX                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Syntax(int atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_error);
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_SYNTAX, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_SYSTEM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_System(int atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(atom_error);
  Execute_A_Continuation(Prolog_Predicate(PL_ERR_SYSTEM, 1));
}
