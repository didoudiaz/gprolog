/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : error_supp.c                                                    *
 * Descr.: Prolog errors support                                           *
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



#define PL_ERR_INSTANTIATION       X1_24706C5F6572725F696E7374616E74696174696F6E

#define PL_ERR_UNINSTANTIATION     X1_24706C5F6572725F756E696E7374616E74696174696F6E

#define PL_ERR_TYPE                X1_24706C5F6572725F74797065

#define PL_ERR_DOMAIN              X1_24706C5F6572725F646F6D61696E

#define PL_ERR_EXISTENCE           X1_24706C5F6572725F6578697374656E6365

#define PL_ERR_PERMISSION          X1_24706C5F6572725F7065726D697373696F6E

#define PL_ERR_REPRESENTATION      X1_24706C5F6572725F726570726573656E746174696F6E

#define PL_ERR_EVALUATION          X1_24706C5F6572725F6576616C756174696F6E

#define PL_ERR_RESOURCE            X1_24706C5F6572725F7265736F75726365

#define PL_ERR_SYNTAX              X1_24706C5F6572725F73796E746178

#define PL_ERR_SYSTEM              X1_24706C5F6572725F73797374656D



Prolog_Prototype(PL_ERR_INSTANTIATION, 0);
Prolog_Prototype(PL_ERR_UNINSTANTIATION, 1);
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
  pl_type_atom = Pl_Create_Atom("atom");
  pl_type_atomic = Pl_Create_Atom("atomic");
  pl_type_byte = Pl_Create_Atom("byte");
  pl_type_callable = Pl_Create_Atom("callable");
  pl_type_character = Pl_Create_Atom("character");
  pl_type_compound = Pl_Create_Atom("compound");
  pl_type_evaluable = Pl_Create_Atom("evaluable");
  pl_type_float = Pl_Create_Atom("float"); /* for arithmetic */
  pl_type_boolean = Pl_Create_Atom("boolean"); /* for setarg/4 */
  pl_type_in_byte = Pl_Create_Atom("in_byte");
  pl_type_in_character = Pl_Create_Atom("in_character");
  pl_type_integer = Pl_Create_Atom("integer");
  pl_type_list = Pl_Create_Atom("list");
  pl_type_number = Pl_Create_Atom("number");
  pl_type_predicate_indicator = Pl_Create_Atom("predicate_indicator");
  pl_type_variable = Pl_Create_Atom("variable"); /* deprecated: new code should emit an uninstantiation_error */
  pl_type_pair = Pl_Create_Atom("pair");
  if (pl_fd_init_solver)		/* FD solver linked */
    {
      pl_type_fd_variable = Pl_Create_Atom("fd_variable"); /* for FD */
      pl_type_fd_evaluable = Pl_Create_Atom("fd_evaluable"); /* for FD */
      pl_type_fd_bool_evaluable = Pl_Create_Atom("fd_bool_evaluable"); /* for FD */
    }

  pl_domain_character_code_list = Pl_Create_Atom("character_code_list");
  pl_domain_close_option = Pl_Create_Atom("close_option");
  pl_domain_flag_value = Pl_Create_Atom("flag_value");
  pl_domain_io_mode = Pl_Create_Atom("io_mode");
  pl_domain_non_empty_list = Pl_Create_Atom("non_empty_list");
  pl_domain_not_less_than_zero = Pl_Create_Atom("not_less_than_zero");
  pl_domain_operator_priority = Pl_Create_Atom("operator_priority");
  pl_domain_operator_specifier = Pl_Create_Atom("operator_specifier");
  pl_domain_prolog_flag = Pl_Create_Atom("prolog_flag");
  pl_domain_read_option = Pl_Create_Atom("read_option");
  pl_domain_source_sink = Pl_Create_Atom("source_sink");
  pl_domain_stream = Pl_Create_Atom("stream");
  pl_domain_stream_option = Pl_Create_Atom("stream_option");
  pl_domain_stream_or_alias = Pl_Create_Atom("stream_or_alias");
  pl_domain_stream_position = Pl_Create_Atom("stream_position");
  pl_domain_stream_property = Pl_Create_Atom("stream_property");
  pl_domain_write_option = Pl_Create_Atom("write_option");
  pl_domain_order = Pl_Create_Atom("order");
  pl_domain_term_stream_or_alias = Pl_Create_Atom("term_stream_or_alias");
				/* for term_streams */
  pl_domain_g_array_index = Pl_Create_Atom("g_array_index");/* for g_vars */
  pl_domain_g_argument_selector = Pl_Create_Atom("g_argument_selector");
				/* for g_vars */
  pl_domain_stream_seek_method = Pl_Create_Atom("stream_seek_method"); /* for seek/4 */
  pl_domain_format_control_sequence = Pl_Create_Atom("format_control_sequence");
				/* for format/2-3 */
  pl_domain_os_path = Pl_Create_Atom("os_path");/* for absolute_file_name/2 */
  pl_domain_os_file_permission = Pl_Create_Atom("os_file_permission");
				/* for file_permission/2 */
  pl_domain_date_time = Pl_Create_Atom("date_time");/* for os_interf */
  pl_domain_selectable_item = Pl_Create_Atom("selectable_item");
				/* for select_read/3 */
#ifndef NO_USE_SOCKETS
  pl_domain_socket_domain = Pl_Create_Atom("socket_domain"); /* for sockets */
  pl_domain_socket_address = Pl_Create_Atom("socket_address"); /* for sockets */
#endif

  pl_existence_procedure = Pl_Create_Atom("procedure");
  pl_existence_source_sink = Pl_Create_Atom("source_sink");
  pl_existence_stream = Pl_Create_Atom("stream");
  pl_existence_sr_descriptor = Pl_Create_Atom("sr_descriptor");
				/* for source reader */


  pl_permission_operation_access = Pl_Create_Atom("access");
  pl_permission_operation_close = Pl_Create_Atom("close");
  pl_permission_operation_create = Pl_Create_Atom("create");
  pl_permission_operation_input = Pl_Create_Atom("input");
  pl_permission_operation_modify = Pl_Create_Atom("modify");
  pl_permission_operation_open = Pl_Create_Atom("open");
  pl_permission_operation_output = Pl_Create_Atom("output");
  pl_permission_operation_reposition = Pl_Create_Atom("reposition");


  pl_permission_type_binary_stream = Pl_Create_Atom("binary_stream");
  pl_permission_type_flag = Pl_Create_Atom("flag");
  pl_permission_type_operator = Pl_Create_Atom("operator");
  pl_permission_type_past_end_of_stream = Pl_Create_Atom("past_end_of_stream");
  pl_permission_type_private_procedure = Pl_Create_Atom("private_procedure");
  pl_permission_type_static_procedure = Pl_Create_Atom("static_procedure");
  pl_permission_type_source_sink = Pl_Create_Atom("source_sink");
  pl_permission_type_stream = Pl_Create_Atom("stream");
  pl_permission_type_text_stream = Pl_Create_Atom("text_stream");


  pl_representation_character = Pl_Create_Atom("character");
  pl_representation_character_code = Pl_Create_Atom("character_code");
  pl_representation_in_character_code = Pl_Create_Atom("in_character_code");
  pl_representation_max_arity = Pl_Create_Atom("max_arity");
  pl_representation_max_integer = Pl_Create_Atom("max_integer");
  pl_representation_min_integer = Pl_Create_Atom("min_integer");
  pl_representation_too_many_variables = Pl_Create_Atom("too_many_variables");
				/* for Pl_Copy_Term(),... */

  pl_evluation_float_overflow = Pl_Create_Atom("float_overflow");
  pl_evluation_int_overflow = Pl_Create_Atom("int_overflow");
  pl_evluation_undefined = Pl_Create_Atom("undefined");
  pl_evluation_underflow = Pl_Create_Atom("underflow");
  pl_evluation_zero_divisor = Pl_Create_Atom("zero_divisor");


  pl_resource_print_object_not_linked = Pl_Create_Atom("print_object_not_linked");
				/* for print and format */
  if (pl_fd_init_solver)		/* FD solver linked */
    {
      pl_resource_too_big_fd_constraint = Pl_Create_Atom("too_big_fd_constraint");
				/* for FD */
    }
}



/*-------------------------------------------------------------------------*
 * PL_SET_BIP_NAME_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Bip_Name_2(WamWord func_word, WamWord arity_word)
{
  PlLong arity;			/* use PlLong to avoid truncation */
  Pl_Set_C_Bip_Name("set_bip_name", 2);

  cur_bip_func = Pl_Rd_Atom_Check(func_word);
  arity = Pl_Rd_Integer_Check(arity_word);
  if (arity > MAX_ARITY)
    arity = -1;
  cur_bip_arity = arity;

  Pl_Unset_C_Bip_Name();

  c_bip_func_str = NULL;
}




/*-------------------------------------------------------------------------*
 * PL_SET_BIP_NAME_UNTAGGED_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Bip_Name_Untagged_2(int func, int arity)
{
  cur_bip_func = func;
  cur_bip_arity = arity;
}



/*-------------------------------------------------------------------------*
 * PL_CURRENT_BIP_NAME_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Bip_Name_2(WamWord func_word, WamWord arity_word)
{
  Bool res;

  Pl_Set_C_Bip_Name("current_bip_name", 2);

  res = Pl_Un_Atom_Check(cur_bip_func, func_word) &&
    Pl_Un_Integer_Check(cur_bip_arity, arity_word);

  Pl_Unset_C_Bip_Name();

  return res;
}




/*-------------------------------------------------------------------------*
 * PL_SET_C_BIP_NAME                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_C_Bip_Name(char *func_str, int arity)
{
  c_bip_func_str = func_str;
  c_bip_arity = arity;
}




/*-------------------------------------------------------------------------*
 * PL_UNSET_C_BIP_NAME                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unset_C_Bip_Name(void)
{
  c_bip_func_str = NULL;
}




/*-------------------------------------------------------------------------*
 * PL_GET_CURRENT_BIP                                                      *
 *                                                                         *
 * returns func and initializes arity of the current bip.                  *
 *-------------------------------------------------------------------------*/
int
Pl_Get_Current_Bip(int *arity)
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
      cur_bip_func = Pl_Create_Allocate_Atom(c_bip_func_str);
      cur_bip_arity = c_bip_arity;
    }
}




/*-------------------------------------------------------------------------*
 * PL_CONTEXT_ERROR_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Context_Error_1(WamWord err_word)
{
  if (cur_bip_arity >= 0)
    {
      Pl_Get_Structure(ATOM_CHAR('/'), 2, err_word);
      Pl_Unify_Atom(cur_bip_func);
      Pl_Unify_Integer(cur_bip_arity);
    }
  else
    Pl_Get_Atom(cur_bip_func, err_word);
}




/*-------------------------------------------------------------------------*
 * CONTEXT_ERROR_STRING                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Context_Error_String(void)
{
  static char buff[256];

  sprintf(buff, "%s", pl_atom_tbl[cur_bip_func].name);
  if (cur_bip_arity >= 0)
    sprintf(buff + strlen(buff), "/%d", cur_bip_arity);

  return buff;
}




/*-------------------------------------------------------------------------*
 * PL_SET_LAST_SYNTAX_ERROR                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Last_Syntax_Error(char *file_name, int err_line, int err_col,
			 char *err_msg)
{
  last_err_file = file_name;
  last_err_line = err_line;
  last_err_col = err_col;
  last_err_msg = err_msg;
}




/*-------------------------------------------------------------------------*
 * PL_SYNTAX_ERROR_INFO_4                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Syntax_Error_Info_4(WamWord file_name_word,
		       WamWord line_word, WamWord char_word, WamWord msg_word)
{
  Pl_Check_For_Un_Atom(file_name_word);
  Pl_Check_For_Un_Integer(line_word);
  Pl_Check_For_Un_Integer(char_word);
  Pl_Check_For_Un_Atom(msg_word);

  return last_err_file &&
    Pl_Un_String(last_err_file, file_name_word) &&
    Pl_Un_Integer(last_err_line, line_word) &&
    Pl_Un_Integer(last_err_col, char_word) &&
    Pl_Un_String(last_err_msg, msg_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYNTAX_ERROR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Syntax_Error(int flag_value)
{
  char str[512];

  if (last_err_file == NULL || *last_err_file == '\0')
    sprintf(str, "(char:%d) %s", last_err_col, last_err_msg);
  else
    sprintf(str, "%s:%d (char:%d) %s", last_err_file,
	    last_err_line, last_err_col, last_err_msg);

  if (flag_value == PF_ERR_ERROR)
    Pl_Err_Syntax(Pl_Create_Allocate_Atom(str));

  Update_Cur_From_C_Bip();
  if (flag_value == PF_ERR_WARNING)
    Pl_Stream_Printf(pl_stm_tbl[pl_stm_top_level_output],
		  "warning: syntax error: %s (from %s)\n",
		  str, Context_Error_String());
}




/*-------------------------------------------------------------------------*
 * PL_UNKNOWN_PRED_ERROR                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unknown_Pred_Error(int func, int arity)
{
  WamWord term;

  if (Flag_Value(unknown) == PF_ERR_ERROR)
    {
      term = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Existence(pl_existence_procedure, term);
    }

  Update_Cur_From_C_Bip();
  if (Flag_Value(unknown) == PF_ERR_WARNING)
    Pl_Stream_Printf(pl_stm_tbl[pl_stm_top_level_output],
		  "warning: unknown procedure %s/%d (from %s)\n",
		  pl_atom_tbl[func].name, arity, Context_Error_String());
}




/*-------------------------------------------------------------------------*
 * PL_OS_ERROR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Os_Error(int ret_val)
{
  char *err_str = Pl_M_Sys_Err_String(ret_val);

  if (Flag_Value(os_error) == PF_ERR_ERROR)
    Pl_Err_System(Pl_Create_Allocate_Atom(err_str));

  Update_Cur_From_C_Bip();
  if (Flag_Value(os_error) == PF_ERR_WARNING)
    Pl_Stream_Printf(pl_stm_tbl[pl_stm_top_level_output],
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_INSTANTIATION, 0));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_UNINSTANTIATION                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Uninstantiation(WamWord term)
{
  Update_Cur_From_C_Bip();
  A(0) = term;
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_UNINSTANTIATION, 1));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_TYPE, 2));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_DOMAIN, 2));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_EXISTENCE, 2));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_PERMISSION, 3));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_REPRESENTATION, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_EVALUATION                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Evaluation(int pl_atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(pl_atom_error);
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_EVALUATION, 1));
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
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_RESOURCE, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_SYNTAX                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_Syntax(int pl_atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(pl_atom_error);
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_SYNTAX, 1));
}




/*-------------------------------------------------------------------------*
 * PL_ERR_SYSTEM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Err_System(int pl_atom_error)
{
  Update_Cur_From_C_Bip();
  A(0) = Tag_ATM(pl_atom_error);
  Pl_Execute_A_Continuation(Prolog_Predicate(PL_ERR_SYSTEM, 1));
}
