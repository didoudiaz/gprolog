/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : error_supp.h                                                    *
 * Descr.: Prolog errors support - header file                             *
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

int type_atom;
int type_atomic;
int type_byte;
int type_callable;
int type_character;
int type_compound;
int type_evaluable;
int type_float;			/* for arithmetic */
int type_boolean;		/* for setarg/4 */
int type_in_byte;
int type_in_character;
int type_integer;
int type_list;
int type_number;
int type_predicate_indicator;
int type_variable;
int type_fd_variable;		/* for FD */
int type_fd_evaluable;		/* for FD */
int type_fd_bool_evaluable;	/* for FD */


int domain_character_code_list;
int domain_close_option;
int domain_flag_value;
int domain_io_mode;
int domain_non_empty_list;
int domain_not_less_than_zero;
int domain_operator_priority;
int domain_operator_specifier;
int domain_prolog_flag;
int domain_read_option;
int domain_source_sink;
int domain_stream;
int domain_stream_option;
int domain_stream_or_alias;
int domain_stream_position;
int domain_stream_property;
int domain_write_option;
int domain_term_stream_or_alias;	/* for term_streams */
int domain_g_array_index;	/* for g_vars */
int domain_stream_seek_method;	/* for seek/4 */
int domain_format_control_sequence;	/* for format/2-3 */
int domain_os_path;		/* for absolute_file_name/2 */
int domain_os_file_permission;	/* for file_permission/2 */
int domain_selectable_item;	/* for select_read/3 */
int domain_date_time;		/* for os_interf */
#ifndef NO_USE_SOCKETS
int domain_socket_domain;	/* for sockets */
int domain_socket_address;	/* for sockets */
#endif

int existence_procedure;
int existence_source_sink;
int existence_stream;
int existence_sr_descriptor;	/* for source reader */

int permission_operation_access;
int permission_operation_close;
int permission_operation_create;
int permission_operation_input;
int permission_operation_modify;
int permission_operation_open;
int permission_operation_output;
int permission_operation_reposition;


int permission_type_binary_stream;
int permission_type_flag;
int permission_type_operator;
int permission_type_past_end_of_stream;
int permission_type_private_procedure;
int permission_type_static_procedure;
int permission_type_source_sink;
int permission_type_stream;
int permission_type_text_stream;


int representation_character;
int representation_character_code;
int representation_in_character_code;
int representation_max_arity;
int representation_max_integer;
int representation_min_integer;
int representation_too_many_variables;
				/* for Copy_Term(),... */

int evluation_float_overflow;
int evluation_int_overflow;
int evluation_undefined;
int evluation_underflow;
int evluation_zero_divisor;


int resource_print_object_not_linked; /* for print and format */
int resource_too_big_fd_constraint; /* for FD */


#else


extern int type_atom;
extern int type_atomic;
extern int type_byte;
extern int type_callable;
extern int type_character;
extern int type_compound;
extern int type_evaluable;
extern int type_float;		/* for arithmetic */
extern int type_boolean;	/* for setarg/4 */
extern int type_in_byte;
extern int type_in_character;
extern int type_integer;
extern int type_list;
extern int type_number;
extern int type_predicate_indicator;
extern int type_variable;
extern int type_fd_variable;	/* for FD */
extern int type_fd_evaluable;	/* for FD */
extern int type_fd_bool_evaluable;	/* for FD */


extern int domain_character_code_list;
extern int domain_close_option;
extern int domain_flag_value;
extern int domain_io_mode;
extern int domain_non_empty_list;
extern int domain_not_less_than_zero;
extern int domain_operator_priority;
extern int domain_operator_specifier;
extern int domain_prolog_flag;
extern int domain_read_option;
extern int domain_source_sink;
extern int domain_stream;
extern int domain_stream_option;
extern int domain_stream_or_alias;
extern int domain_stream_position;
extern int domain_stream_property;
extern int domain_write_option;
extern int domain_term_stream_or_alias;	/* for term_streams */
extern int domain_g_array_index;	/* for g_vars */
extern int domain_stream_seek_method;	/* for seek/4 */
extern int domain_format_control_sequence;	/* for format/2-3 */
extern int domain_os_path;	/* for absolute_file_name/2 */
extern int domain_os_file_permission; /* for file_permission/2 */
extern int domain_selectable_item; /* for select_read/3 */
extern int domain_date_time;	/* for os_interf */
#ifndef NO_USE_SOCKETS
extern int domain_socket_domain; /* for sockets */
extern int domain_socket_address; /* for sockets */
#endif


extern int existence_procedure;
extern int existence_source_sink;
extern int existence_stream;
extern int existence_sr_descriptor; /* for source reader */


extern int permission_operation_access;
extern int permission_operation_close;
extern int permission_operation_create;
extern int permission_operation_input;
extern int permission_operation_modify;
extern int permission_operation_open;
extern int permission_operation_output;
extern int permission_operation_reposition;


extern int permission_type_binary_stream;
extern int permission_type_flag;
extern int permission_type_operator;
extern int permission_type_past_end_of_stream;
extern int permission_type_private_procedure;
extern int permission_type_static_procedure;
extern int permission_type_source_sink;
extern int permission_type_stream;
extern int permission_type_text_stream;


extern int representation_character;
extern int representation_character_code;
extern int representation_in_character_code;
extern int representation_max_arity;
extern int representation_max_integer;
extern int representation_min_integer;
extern int representation_too_many_variables;
				/* for Copy_Term(),... */


extern int evluation_float_overflow;
extern int evluation_int_overflow;
extern int evluation_undefined;
extern int evluation_underflow;
extern int evluation_zero_divisor;


extern int resource_too_many_open_streams; /* for streams */
extern int resource_print_object_not_linked; /* for print and format */
extern int resource_too_big_fd_constraint; /* for FD */

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Set_Bip_Name_2(WamWord func_word, WamWord arity_word);

void Set_C_Bip_Name(char *func_str, int arity) FC;

void Unset_C_Bip_Name(void);

int Get_Current_Bip(int *arity);

void Set_Last_Syntax_Error(char *file_name, int err_line, int err_col,
			   char *err_msg);

void Syntax_Error(int flag_value);

void Unknown_Pred_Error(int func, int arity);

void Os_Error(void);

void Pl_Err_Instantiation(void);

void Pl_Err_Type(int atom_type, WamWord term);

void Pl_Err_Domain(int atom_domain, WamWord term);

void Pl_Err_Existence(int atom_object, WamWord term);

void Pl_Err_Permission(int atom_oper, int atom_perm, WamWord term);

void Pl_Err_Representation(int atom_flag);

void Pl_Err_Evaluation(int atom_error);

void Pl_Err_Resource(int atom_resource);

void Pl_Err_Syntax(int atom_error);

void Pl_Err_System(int atom_error);



#define Os_Test_Error(tst)  \
      do { if (tst) { Os_Error(); return FALSE; } } while(0)
