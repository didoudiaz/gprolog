/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : error_supp.h                                                    *
 * Descr.: Prolog errors support - header file                             *
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

#ifdef ERROR_SUPP_FILE

int pl_type_atom;
int pl_type_atomic;
int pl_type_byte;
int pl_type_callable;
int pl_type_character;
int pl_type_compound;
int pl_type_evaluable;
int pl_type_float;				/* for arithmetic */
int pl_type_boolean;				/* for setarg/4 */
int pl_type_in_byte;
int pl_type_in_character;
int pl_type_integer;
int pl_type_list;
int pl_type_number;
int pl_type_predicate_indicator;
int pl_type_variable; /* deprecated: new code should emit an uninstantiation_error */
int pl_type_pair;
int pl_type_fd_variable;			/* for FD */
int pl_type_fd_evaluable;			/* for FD */
int pl_type_fd_bool_evaluable;			/* for FD */


int pl_domain_character_code_list;
int pl_domain_close_option;
int pl_domain_flag_value;
int pl_domain_io_mode;
int pl_domain_non_empty_list;
int pl_domain_not_less_than_zero;
int pl_domain_operator_priority;
int pl_domain_operator_specifier;
int pl_domain_prolog_flag;
int pl_domain_read_option;
int pl_domain_source_sink;
int pl_domain_stream;
int pl_domain_stream_option;
int pl_domain_stream_or_alias;
int pl_domain_stream_position;
int pl_domain_stream_property;
int pl_domain_write_option;
int pl_domain_order;
int pl_domain_term_stream_or_alias;		/* for term_streams */
int pl_domain_g_array_index;			/* for g_vars */
int pl_domain_g_argument_selector;		/* for g_vars */
int pl_domain_stream_seek_method;		/* for seek/4 */
int pl_domain_format_control_sequence;		/* for format/2-3 */
int pl_domain_os_path;				/* for absolute_file_name/2 */
int pl_domain_os_file_permission;		/* for file_permission/2 */
int pl_domain_selectable_item;			/* for select_read/3 */
int pl_domain_date_time;			/* for os_interf */
#ifndef NO_USE_SOCKETS
int pl_domain_socket_domain;			/* for sockets */
int pl_domain_socket_address;			/* for sockets */
#endif

int pl_existence_procedure;
int pl_existence_source_sink;
int pl_existence_stream;
int pl_existence_sr_descriptor;			/* for source reader */

int pl_permission_operation_access;
int pl_permission_operation_close;
int pl_permission_operation_create;
int pl_permission_operation_input;
int pl_permission_operation_modify;
int pl_permission_operation_open;
int pl_permission_operation_output;
int pl_permission_operation_reposition;


int pl_permission_type_binary_stream;
int pl_permission_type_flag;
int pl_permission_type_operator;
int pl_permission_type_past_end_of_stream;
int pl_permission_type_private_procedure;
int pl_permission_type_static_procedure;
int pl_permission_type_source_sink;
int pl_permission_type_stream;
int pl_permission_type_text_stream;


int pl_representation_character;
int pl_representation_character_code;
int pl_representation_in_character_code;
int pl_representation_max_arity;
int pl_representation_max_integer;
int pl_representation_min_integer;
int pl_representation_too_many_variables;	/* for Pl_Copy_Term(),... */

int pl_evluation_float_overflow;
int pl_evluation_int_overflow;
int pl_evluation_undefined;
int pl_evluation_underflow;
int pl_evluation_zero_divisor;


int pl_resource_print_object_not_linked; 	/* for print and format */
int pl_resource_too_big_fd_constraint; 		/* for FD */


#else


extern int pl_type_atom;
extern int pl_type_atomic;
extern int pl_type_byte;
extern int pl_type_callable;
extern int pl_type_character;
extern int pl_type_compound;
extern int pl_type_evaluable;
extern int pl_type_float;			/* for arithmetic */
extern int pl_type_boolean;			/* for setarg/4 */
extern int pl_type_in_byte;
extern int pl_type_in_character;
extern int pl_type_integer;
extern int pl_type_list;
extern int pl_type_number;
extern int pl_type_predicate_indicator;
extern int pl_type_variable;  /* deprecated: new code should emit an uninstantiation_error */
extern int pl_type_pair;
extern int pl_type_fd_variable;			/* for FD */
extern int pl_type_fd_evaluable;		/* for FD */
extern int pl_type_fd_bool_evaluable;		/* for FD */


extern int pl_domain_character_code_list;
extern int pl_domain_close_option;
extern int pl_domain_flag_value;
extern int pl_domain_io_mode;
extern int pl_domain_non_empty_list;
extern int pl_domain_not_less_than_zero;
extern int pl_domain_operator_priority;
extern int pl_domain_operator_specifier;
extern int pl_domain_prolog_flag;
extern int pl_domain_read_option;
extern int pl_domain_source_sink;
extern int pl_domain_stream;
extern int pl_domain_stream_option;
extern int pl_domain_stream_or_alias;
extern int pl_domain_stream_position;
extern int pl_domain_stream_property;
extern int pl_domain_write_option;
extern int pl_domain_order;
extern int pl_domain_term_stream_or_alias;	/* for term_streams */
extern int pl_domain_g_array_index;		/* for g_vars */
extern int pl_domain_g_argument_selector;	/* for g_vars */
extern int pl_domain_stream_seek_method;	/* for seek/4 */
extern int pl_domain_format_control_sequence;	/* for format/2-3 */
extern int pl_domain_os_path;			/* for absolute_file_name/2 */
extern int pl_domain_os_file_permission; 	/* for file_permission/2 */
extern int pl_domain_selectable_item; 		/* for select_read/3 */
extern int pl_domain_date_time;			/* for os_interf */
#ifndef NO_USE_SOCKETS
extern int pl_domain_socket_domain; 		/* for sockets */
extern int pl_domain_socket_address; 		/* for sockets */
#endif


extern int pl_existence_procedure;
extern int pl_existence_source_sink;
extern int pl_existence_stream;
extern int pl_existence_sr_descriptor; 		/* for source reader */


extern int pl_permission_operation_access;
extern int pl_permission_operation_close;
extern int pl_permission_operation_create;
extern int pl_permission_operation_input;
extern int pl_permission_operation_modify;
extern int pl_permission_operation_open;
extern int pl_permission_operation_output;
extern int pl_permission_operation_reposition;


extern int pl_permission_type_binary_stream;
extern int pl_permission_type_flag;
extern int pl_permission_type_operator;
extern int pl_permission_type_past_end_of_stream;
extern int pl_permission_type_private_procedure;
extern int pl_permission_type_static_procedure;
extern int pl_permission_type_source_sink;
extern int pl_permission_type_stream;
extern int pl_permission_type_text_stream;


extern int pl_representation_character;
extern int pl_representation_character_code;
extern int pl_representation_in_character_code;
extern int pl_representation_max_arity;
extern int pl_representation_max_integer;
extern int pl_representation_min_integer;
extern int pl_representation_too_many_variables;/* for Pl_Copy_Term(),... */


extern int pl_evluation_float_overflow;
extern int pl_evluation_int_overflow;
extern int pl_evluation_undefined;
extern int pl_evluation_underflow;
extern int pl_evluation_zero_divisor;


extern int resource_too_many_open_streams; 	/* for streams */
extern int pl_resource_print_object_not_linked; /* for print and format */
extern int pl_resource_too_big_fd_constraint; 	/* for FD */

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Set_Bip_Name_2(WamWord func_word, WamWord arity_word);

void Pl_Set_Bip_Name_Untagged_2(int func, int arity);

void Pl_Set_C_Bip_Name(char *func_str, int arity);

void Pl_Unset_C_Bip_Name(void);

int Pl_Get_Current_Bip(int *arity);

void Pl_Set_Last_Syntax_Error(char *file_name, int err_line, int err_col,
			   char *err_msg);

void Pl_Syntax_Error(int flag_value);

void Pl_Unknown_Pred_Error(int func, int arity);

void Pl_Os_Error(int ret_val);

void Pl_Err_Instantiation(void);

void Pl_Err_Uninstantiation(WamWord term);

void Pl_Err_Type(int atom_type, WamWord term);

void Pl_Err_Domain(int atom_domain, WamWord term);

void Pl_Err_Existence(int atom_object, WamWord term);

void Pl_Err_Permission(int atom_oper, int atom_perm, WamWord term);

void Pl_Err_Representation(int atom_flag);

void Pl_Err_Evaluation(int pl_atom_error);

void Pl_Err_Resource(int atom_resource);

void Pl_Err_Syntax(int pl_atom_error);

void Pl_Err_System(int pl_atom_error);

#define Os_Test_Error_Null(tst)			\
  do {						\
    if ((tst) == NULL)				\
      {						\
	Pl_Os_Error(-1);			\
	return FALSE;				\
      }						\
  } while(0)


#define Os_Test_Error(tst)			\
  do {						\
    int _tst = (tst);				\
    if (_tst < 0)				\
      {						\
	Pl_Os_Error(_tst);			\
	return FALSE;				\
      }						\
  } while(0)
