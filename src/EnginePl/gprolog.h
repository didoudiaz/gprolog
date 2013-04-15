/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Foreign interface                                               *
 * File  : gprolog.h                                                       *
 * Descr.: GNU Prolog - general header file (for users)                    *
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


#ifndef _GPROLOG_H

#include <stdint.h>

#define _GPROLOG_H
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Since GNU Prolog 1.4.0 C types 'long' have been replaced by PlLong
 * to work on X86_64/Windows (where long are 32-bits and not 64-bits).
 * New foreign code should use PlLong instead of long.
 *
 * Since GNU Prolog 1.3.1 all public names are prefixed with Pl_, PL_ or pl_
 * so you should avoid these prefixes.
 * The names of C functions used in the foreign interface have been
 * renamed to start with the Pl_ prefix. To keep a bacward compatibility
 * with foreign code developed with gprolog < 1.3.1 macros are defined
 * for old names.
 * These macros can be deactivated if __GPROLOG_FOREIGN_STRICT__ is defined.
 *
 * #define __GPROLOG_FOREIGN_STRICT__
 * #include <gprolog.h>
 */

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#include "gprolog_cst.h"

#define PL_RECOVER        	0
#define PL_CUT			1
#define PL_KEEP_FOR_PROLOG	2

#define PL_FAILURE		0
#define PL_SUCCESS		1
#define PL_EXCEPTION		2




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum { PL_FALSE, PL_TRUE } PlBool;

typedef intptr_t PlLong;

typedef uintptr_t PlULong;

typedef intptr_t PlTerm;

typedef struct
{
  PlBool is_var;
  PlBool unify;
  union
  {
    PlLong l;
    char *s;
    double d;
  }
  value;
}
PlFIOArg;


/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

extern int pl_foreign_bkt_counter;
extern char *pl_foreign_bkt_buffer;

extern int pl_type_atom;
extern int pl_type_atomic;
extern int pl_type_byte;
extern int pl_type_callable;
extern int pl_type_character;
extern int pl_type_compound;
extern int pl_type_evaluable;
extern int pl_type_float;
extern int pl_type_boolean;
extern int pl_type_in_byte;
extern int pl_type_in_character;
extern int pl_type_integer;
extern int pl_type_list;
extern int pl_type_number;
extern int pl_type_predicate_indicator;
extern int pl_type_variable;	/* deprecated: new code should emit an uninstantiation_error */
extern int pl_type_fd_variable;
extern int pl_type_fd_evaluable;
extern int pl_type_fd_bool_evaluable;


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
extern int pl_domain_term_stream_or_alias;
extern int pl_domain_g_array_index;
extern int pl_domain_g_argument_selector;
extern int pl_domain_stream_seek_method;
extern int pl_domain_format_control_sequence;
extern int pl_domain_os_path;
extern int pl_domain_os_file_permission;
extern int pl_domain_selectable_item;
extern int pl_domain_date_time;

extern int pl_existence_procedure;
extern int pl_existence_source_sink;
extern int pl_existence_stream;
extern int pl_existence_sr_descriptor;


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
extern int pl_representation_too_many_variables;

extern int pl_evluation_float_overflow;
extern int pl_evluation_int_overflow;
extern int pl_evluation_undefined;
extern int pl_evluation_underflow;
extern int pl_evluation_zero_divisor;


extern int pl_resource_print_object_not_linked;
extern int pl_resource_too_big_fd_constraint;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Pl_Start_Prolog(int argc, char *argv[]);

void Pl_Stop_Prolog(void);

void Pl_Reset_Prolog(void);

PlBool Pl_Try_Execute_Top_Level(void);




#define Pl_Get_Choice_Counter()   pl_foreign_bkt_counter

#define Pl_Get_Choice_Buffer(t)   ((t) pl_foreign_bkt_buffer)

void Pl_No_More_Choice(void);



char *Pl_Atom_Name(int atom);

int Pl_Atom_Length(int atom);

PlBool Pl_Atom_Needs_Quote(int atom);

PlBool Pl_Atom_Needs_Scan(int atom);

PlBool Pl_Is_Valid_Atom(int atom);

int Pl_Create_Atom(const char *atom);

int Pl_Create_Allocate_Atom(const char *atom);

int Pl_Find_Atom(const char *atom);

int Pl_Atom_Char(char c);

int Pl_Atom_Nil(void);

int Pl_Atom_False(void);

int Pl_Atom_True(void);

int Pl_Atom_End_Of_File(void);



PlBool Pl_Unif(PlTerm term1, PlTerm term2);

PlBool Pl_Unif_With_Occurs_Check(PlTerm term1, PlTerm term2);




PlLong Pl_Rd_Integer_Check(PlTerm term);

PlLong Pl_Rd_Integer(PlTerm term);

PlLong Pl_Rd_Positive_Check(PlTerm term);

PlLong Pl_Rd_Positive(PlTerm term);

double Pl_Rd_Float_Check(PlTerm term);

double Pl_Rd_Float(PlTerm term);

double Pl_Rd_Number_Check(PlTerm term);

double Pl_Rd_Number(PlTerm term);

int Pl_Rd_Atom_Check(PlTerm term);

int Pl_Rd_Atom(PlTerm term);

int Pl_Rd_Boolean_Check(PlTerm term);

int Pl_Rd_Boolean(PlTerm term);

int Pl_Rd_Char_Check(PlTerm term);

int Pl_Rd_Char(PlTerm term);

int Pl_Rd_In_Char_Check(PlTerm term);

int Pl_Rd_In_Char(PlTerm term);

int Pl_Rd_Code_Check(PlTerm term);

int Pl_Rd_Code(PlTerm term);

int Pl_Rd_In_Code_Check(PlTerm term);

int Pl_Rd_In_Code(PlTerm term);

int Pl_Rd_Byte_Check(PlTerm term);

int Pl_Rd_Byte(PlTerm term);

int Pl_Rd_In_Byte_Check(PlTerm term);

int Pl_Rd_In_Byte(PlTerm term);

char *Pl_Rd_String_Check(PlTerm term);

char *Pl_Rd_String(PlTerm term);

char *Pl_Rd_Chars_Check(PlTerm term);

char *Pl_Rd_Chars(PlTerm term);

char *Pl_Rd_Codes_Check(PlTerm term);

char *Pl_Rd_Codes(PlTerm term);

int Pl_Rd_Chars_Str_Check(PlTerm term, char *str);

int Pl_Rd_Chars_Str(PlTerm term, char *str);

int Pl_Rd_Codes_Str_Check(PlTerm term, char *str);

int Pl_Rd_Codes_Str(PlTerm term, char *str);

PlTerm *Pl_Rd_List_Check(PlTerm term);

PlTerm *Pl_Rd_List(PlTerm term);

int Pl_Rd_Proper_List_Check(PlTerm term, PlTerm *arg);

int Pl_Rd_Proper_List(PlTerm term, PlTerm *arg);

PlTerm *Pl_Rd_Compound_Check(PlTerm term, int *func, int *arity);

PlTerm *Pl_Rd_Compound(PlTerm term, int *func, int *arity);

PlTerm *Pl_Rd_Callable_Check(PlTerm term, int *func, int *arity);

PlTerm *Pl_Rd_Callable(PlTerm term, int *func, int *arity);



void Pl_Check_For_Un_Integer(PlTerm term);

void Pl_Check_For_Un_Positive(PlTerm term);

void Pl_Check_For_Un_Float(PlTerm term);

void Pl_Check_For_Un_Number(PlTerm term);

void Pl_Check_For_Un_Atom(PlTerm term);

void Pl_Check_For_Un_Boolean(PlTerm term);

void Pl_Check_For_Un_Char(PlTerm term);

void Pl_Check_For_Un_In_Char(PlTerm term);

void Pl_Check_For_Un_Code(PlTerm term);

void Pl_Check_For_Un_In_Code(PlTerm term);

void Pl_Check_For_Un_Byte(PlTerm term);

void Pl_Check_For_Un_In_Byte(PlTerm term);

void Pl_Check_For_Un_String(PlTerm term);

void Pl_Check_For_Un_Chars(PlTerm term);

void Pl_Check_For_Un_Codes(PlTerm term);

void Pl_Check_For_Un_List(PlTerm term);

void Pl_Check_For_Un_Compound(PlTerm term);

void Pl_Check_For_Un_Callable(PlTerm term);

void Pl_Check_For_Un_Variable(PlTerm term);



PlBool Pl_Un_Integer_Check(PlLong value, PlTerm term);

PlBool Pl_Un_Integer(PlLong value, PlTerm term);

PlBool Pl_Un_Positive_Check(PlLong value, PlTerm term);

PlBool Pl_Un_Positive(PlLong value, PlTerm term);

PlBool Pl_Un_Float_Check(double value, PlTerm term);

PlBool Pl_Un_Float(double value, PlTerm term);

PlBool Pl_Un_Number_Check(double value, PlTerm term);

PlBool Pl_Un_Number(double value, PlTerm term);

PlBool Pl_Un_Atom_Check(int value, PlTerm term);

PlBool Pl_Un_Atom(int value, PlTerm term);

PlBool Pl_Un_Boolean_Check(int value, PlTerm term);

PlBool Pl_Un_Boolean(int value, PlTerm term);

PlBool Pl_Un_Char_Check(int value, PlTerm term);

PlBool Pl_Un_Char(int value, PlTerm term);

PlBool Pl_Un_In_Char_Check(int value, PlTerm term);

PlBool Pl_Un_In_Char(int value, PlTerm term);

PlBool Pl_Un_Code_Check(int value, PlTerm term);

PlBool Pl_Un_Code(int value, PlTerm term);

PlBool Pl_Un_In_Code_Check(int value, PlTerm term);

PlBool Pl_Un_In_Code(int value, PlTerm term);

PlBool Pl_Un_Byte_Check(int value, PlTerm term);

PlBool Pl_Un_Byte(int value, PlTerm term);

PlBool Pl_Un_In_Byte_Check(int value, PlTerm term);

PlBool Pl_Un_In_Byte(int value, PlTerm term);

PlBool Pl_Un_String_Check(const char *value, PlTerm term);

PlBool Pl_Un_String(const char *value, PlTerm term);

PlBool Pl_Un_Chars_Check(const char *value, PlTerm term);

PlBool Pl_Un_Chars(const char *value, PlTerm term);

PlBool Pl_Un_Codes_Check(const char *value, PlTerm term);

PlBool Pl_Un_Codes(const char *value, PlTerm term);

PlBool Pl_Un_List_Check(PlTerm *arg, PlTerm term);

PlBool Pl_Un_List(PlTerm *arg, PlTerm term);

PlBool Pl_Un_Proper_List_Check(int n, PlTerm *arg, PlTerm term);

PlBool Pl_Un_Proper_List(int n, PlTerm *arg, PlTerm term);

PlBool Pl_Un_Compound_Check(int func, int arity, PlTerm *arg,
		       PlTerm term);

PlBool Pl_Un_Compound(int func, int arity, PlTerm *arg, PlTerm term);

PlBool Pl_Un_Callable_Check(int func, int arity, PlTerm *arg,
		       PlTerm term);

PlBool Pl_Un_Callable(int func, int arity, PlTerm *arg, PlTerm term);

PlBool Pl_Un_Term(PlTerm term1, PlTerm term2);



PlTerm Pl_Mk_Integer(PlLong value);

PlTerm Pl_Mk_Positive(PlLong value);

PlTerm Pl_Mk_Float(double value);

PlTerm Pl_Mk_Number(double value);

PlTerm Pl_Mk_Atom(int value);

PlTerm Pl_Mk_Boolean(int value);

PlTerm Pl_Mk_Char(int value);

PlTerm Pl_Mk_In_Char(int value);

PlTerm Pl_Mk_Code(int value);

PlTerm Pl_Mk_In_Code(int value);

PlTerm Pl_Mk_Byte(int value);

PlTerm Pl_Mk_In_Byte(int value);

PlTerm Pl_Mk_String(const char *value);

PlTerm Pl_Mk_Chars(const char *value);

PlTerm Pl_Mk_Codes(const char *value);

PlTerm Pl_Mk_List(const PlTerm *arg);

PlTerm Pl_Mk_Proper_List(int n, const PlTerm *arg);

PlTerm Pl_Mk_Compound(int func, int arity, const PlTerm *arg);

PlTerm Pl_Mk_Callable(int func, int arity, const PlTerm *arg);

PlTerm Pl_Mk_Variable(void);




int Pl_Type_Of_Term(PlTerm term);

int Pl_List_Length(PlTerm list);

PlLong Pl_Term_Compare(PlTerm term1, PlTerm term2);



PlBool Pl_Builtin_Var(PlTerm term);

PlBool Pl_Builtin_Non_Var(PlTerm term);

PlBool Pl_Builtin_Atom(PlTerm term);

PlBool Pl_Builtin_Integer(PlTerm term);

PlBool Pl_Builtin_Float(PlTerm term);

PlBool Pl_Builtin_Number(PlTerm term);

PlBool Pl_Builtin_Atomic(PlTerm term);

PlBool Pl_Builtin_Compound(PlTerm term);

PlBool Pl_Builtin_Callable(PlTerm term);

PlBool Pl_Builtin_Fd_Var(PlTerm term);

PlBool Pl_Builtin_Non_Fd_Var(PlTerm term);

PlBool Pl_Builtin_Generic_Var(PlTerm term);

PlBool Pl_Builtin_Non_Generic_Var(PlTerm term);

PlBool Pl_Builtin_List(PlTerm term);

PlBool Pl_Builtin_Partial_List(PlTerm term);

PlBool Pl_Builtin_List_Or_Partial_List(PlTerm term);



PlBool Pl_Builtin_Term_Eq(PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Term_Neq(PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Term_Lt(PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Term_Lte(PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Term_Gt(PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Term_Gte(PlTerm term1, PlTerm term2);



PlBool Pl_Builtin_Compare(PlTerm cmp, PlTerm term1, PlTerm term2);

PlBool Pl_Builtin_Arg(PlTerm arg_no, PlTerm term, PlTerm sub_term);

PlBool Pl_Builtin_Functor(PlTerm term, PlTerm functor, PlTerm arity);

PlBool Pl_Builtin_Univ(PlTerm term, PlTerm list);




PlBool Pl_Builtin_Eq(PlTerm expr1, PlTerm expr2);

PlBool Pl_Builtin_Neq(PlTerm expr1, PlTerm expr2);

PlBool Pl_Builtin_Lt(PlTerm expr1, PlTerm expr2);

PlBool Pl_Builtin_Lte(PlTerm expr1, PlTerm expr2);

PlBool Pl_Builtin_Gt(PlTerm expr1, PlTerm expr2);

PlBool Pl_Builtin_Gte(PlTerm expr1, PlTerm expr2);

void Pl_Math_Evaluate(PlTerm expr, PlTerm *result);




int Pl_Term_Size(PlTerm term);

void Pl_Copy_Term(PlTerm *dst_term, PlTerm *src_term);

void Pl_Copy_Contiguous_Term(PlTerm *dst_term, PlTerm *src_term);




void Pl_Set_C_Bip_Name(const char *functor, int arity);

void Pl_Unset_C_Bip_Name(void);





void Pl_Err_Instantiation(void);

void Pl_Err_Type(int atom_type, PlTerm term);

void Pl_Err_Domain(int atom_domain, PlTerm term);

void Pl_Err_Existence(int atom_object, PlTerm term);

void Pl_Err_Permission(int atom_oper, int atom_perm, PlTerm term);

void Pl_Err_Representation(int atom_flag);

void Pl_Err_Evaluation(int pl_atom_error);

void Pl_Err_Resource(int atom_resource);

void Pl_Err_Syntax(int pl_atom_error);

void Pl_Err_System(int pl_atom_error);


void Pl_Emit_Syntax_Error(char *file_name, int err_line, int err_col, char *err_msg);

void Pl_Os_Error(void);




void Pl_Write(PlTerm term);

char *Pl_Write_To_String(PlTerm term);

char *Pl_Writeq_To_String(PlTerm term);

char *Pl_Write_Canonical_To_String(PlTerm term);

char *Pl_Display_To_String(PlTerm term);

PlTerm Pl_Read_From_String(const char *str);




void Pl_Exec_Continuation(int func, int arity, PlTerm *arg_adr);

void Pl_Throw(PlTerm ball);

void Pl_Query_Begin(PlBool recoverable);

int Pl_Query_Call(int func, int arity, PlTerm *arg_adr);

int Pl_Query_Start(int func, int arity, PlTerm *arg_adr, PlBool recoverable);

int Pl_Query_Next_Solution(void);

void Pl_Query_End(int op);

PlTerm Pl_Get_Exception(void);




#define PL_PLV                    PL_REF



/*-------------------------------------------------------------------------*
 * Deprecated API                                                          *
 *                                                                         *
 * For backward compatibility purpose only. New code should use new API.   *
 *-------------------------------------------------------------------------*/

#ifndef __GPROLOG_FOREIGN_STRICT__

#ifdef FALSE

#    if FALSE != 0
#        error "FALSE already defined with a value != 0"
#    endif

#else

#define FALSE 0

#endif

#ifdef TRUE

#    if TRUE != 1
#        error "TRUE already defined with a value != 1"
#    endif

#else

#define TRUE 1

#endif

#ifndef Bool
typedef PlBool Bool;
#endif


typedef PlFIOArg FIOArg;

#define type_atom pl_type_atom
#define type_atomic pl_type_atomic
#define type_byte pl_type_byte
#define type_callable pl_type_callable
#define type_character pl_type_character
#define type_compound pl_type_compound
#define type_evaluable pl_type_evaluable
#define type_float pl_type_float
#define type_boolean pl_type_boolean
#define type_in_byte pl_type_in_byte
#define type_in_character pl_type_in_character
#define type_integer pl_type_integer
#define type_list pl_type_list
#define type_number pl_type_number
#define type_predicate_indicator pl_type_predicate_indicator
#define type_variable pl_type_variable /* deprecated: new code should emit an uninstantiation_error */

#define type_fd_variable pl_type_fd_variable
#define type_fd_evaluable pl_type_fd_evaluable
#define type_fd_bool_evaluable pl_type_fd_bool_evaluable


#define domain_character_code_list pl_domain_character_code_list
#define domain_close_option pl_domain_close_option
#define domain_flag_value pl_domain_flag_value
#define domain_io_mode pl_domain_io_mode
#define domain_non_empty_list pl_domain_non_empty_list
#define domain_not_less_than_zero pl_domain_not_less_than_zero
#define domain_operator_priority pl_domain_operator_priority
#define domain_operator_specifier pl_domain_operator_specifier
#define domain_prolog_flag pl_domain_prolog_flag
#define domain_read_option pl_domain_read_option
#define domain_source_sink pl_domain_source_sink
#define domain_stream pl_domain_stream
#define domain_stream_option pl_domain_stream_option
#define domain_stream_or_alias pl_domain_stream_or_alias
#define domain_stream_position pl_domain_stream_position
#define domain_stream_property pl_domain_stream_property
#define domain_write_option pl_domain_write_option
#define domain_term_stream_or_alias pl_domain_term_stream_or_alias
#define domain_g_array_index pl_domain_g_array_index
#define domain_g_argument_selector pl_domain_g_argument_selector
#define domain_stream_seek_method pl_domain_stream_seek_method
#define domain_format_control_sequence pl_domain_format_control_sequence
#define domain_os_path pl_domain_os_path
#define domain_os_file_permission pl_domain_os_file_permission
#define domain_selectable_item pl_domain_selectable_item
#define domain_date_time pl_domain_date_time


#define existence_procedure pl_existence_procedure
#define existence_source_sink pl_existence_source_sink
#define existence_stream pl_existence_stream
#define existence_sr_descriptor pl_existence_sr_descriptor


#define permission_operation_access pl_permission_operation_access
#define permission_operation_close pl_permission_operation_close
#define permission_operation_create pl_permission_operation_create
#define permission_operation_input pl_permission_operation_input
#define permission_operation_modify pl_permission_operation_modify
#define permission_operation_open pl_permission_operation_open
#define permission_operation_output pl_permission_operation_output
#define permission_operation_reposition pl_permission_operation_reposition


#define permission_type_binary_stream pl_permission_type_binary_stream
#define permission_type_flag pl_permission_type_flag
#define permission_type_operator pl_permission_type_operator
#define permission_type_past_end_of_stream pl_permission_type_past_end_of_stream
#define permission_type_private_procedure pl_permission_type_private_procedure
#define permission_type_static_procedure pl_permission_type_static_procedure
#define permission_type_source_sink pl_permission_type_source_sink
#define permission_type_stream pl_permission_type_stream
#define permission_type_text_stream pl_permission_type_text_stream


#define representation_character pl_representation_character
#define representation_character_code pl_representation_character_code
#define representation_in_character_code pl_representation_in_character_code
#define representation_max_arity pl_representation_max_arity
#define representation_max_integer pl_representation_max_integer
#define representation_min_integer pl_representation_min_integer
#define representation_too_many_variables pl_representation_too_many_variables

#define evluation_float_overflow pl_evluation_float_overflow
#define evluation_int_overflow pl_evluation_int_overflow
#define evluation_undefined pl_evluation_undefined
#define evluation_underflow pl_evluation_underflow
#define evluation_zero_divisor pl_evluation_zero_divisor


#define resource_print_object_not_linked pl_resource_print_object_not_linked
#define resource_too_big_fd_constraint pl_resource_too_big_fd_constraint



#define Start_Prolog(argc, argv) Pl_Start_Prolog(argc, argv)

#define Stop_Prolog() Pl_Stop_Prolog()

#define Reset_Prolog() Pl_Reset_Prolog()

#define Try_Execute_Top_Level() Pl_Try_Execute_Top_Level()





#define Get_Choice_Counter()    Pl_Get_Choice_Counter()

#define Get_Choice_Buffer(t)    Pl_Get_Choice_Buffer(t)

#define No_More_Choice()        Pl_No_More_Choice()


#define Atom_Name(a)            Pl_Atom_Name(a)

#define Atom_Length(a)	        Pl_Atom_Length(a)

#define Atom_Needs_Quote(a)     Pl_Atom_Needs_Quote(a)

#define Atom_Needs_Scan(a)      Pl_Atom_Needs_Scan(a)

#define Is_Valid_Atom(a)        Pl_Is_Valid_Atom(a)

#define Create_Atom(a)          Pl_Create_Atom(a)

#define Create_Allocate_Atom(a) Pl_Create_Allocate_Atom(a)

#define Find_Atom(a)            Pl_Find_Atom(a)

#define ATOM_CHAR(c)            Pl_Atom_Char(c)

#define atom_nil                Pl_Atom_Nil()

#define atom_false              Pl_Atom_False()

#define atom_true               Pl_Atom_True()

#define atom_end_of_file        Pl_Atom_End_Of_File()



#define Rd_Integer_Check(term) Pl_Rd_Integer_Check(term)

#define Rd_Integer(term) Pl_Rd_Integer(term)

#define Rd_Positive_Check(term) Pl_Rd_Positive_Check(term)

#define Rd_Positive(term) Pl_Rd_Positive(term)

#define Rd_Float_Check(term) Pl_Rd_Float_Check(term)

#define Rd_Float(term) Pl_Rd_Float(term)

#define Rd_Number_Check(term) Pl_Rd_Number_Check(term)

#define Rd_Number(term) Pl_Rd_Number(term)

#define Rd_Atom_Check(term) Pl_Rd_Atom_Check(term)

#define Rd_Atom(term) Pl_Rd_Atom(term)

#define Rd_Boolean_Check(term) Pl_Rd_Boolean_Check(term)

#define Rd_Boolean(term) Pl_Rd_Boolean(term)

#define Rd_Char_Check(term) Pl_Rd_Char_Check(term)

#define Rd_Char(term) Pl_Rd_Char(term)

#define Rd_In_Char_Check(term) Pl_Rd_In_Char_Check(term)

#define Rd_In_Char(term) Pl_Rd_In_Char(term)

#define Rd_Code_Check(term) Pl_Rd_Code_Check(term)

#define Rd_Code(term) Pl_Rd_Code(term)

#define Rd_In_Code_Check(term) Pl_Rd_In_Code_Check(term)

#define Rd_In_Code(term) Pl_Rd_In_Code(term)

#define Rd_Byte_Check(term) Pl_Rd_Byte_Check(term)

#define Rd_Byte(term) Pl_Rd_Byte(term)

#define Rd_In_Byte_Check(term) Pl_Rd_In_Byte_Check(term)

#define Rd_In_Byte(term) Pl_Rd_In_Byte(term)

#define Rd_String_Check(term) Pl_Rd_String_Check(term)

#define Rd_String(term) Pl_Rd_String(term)

#define Rd_Chars_Check(term) Pl_Rd_Chars_Check(term)

#define Rd_Chars(term) Pl_Rd_Chars(term)

#define Rd_Codes_Check(term) Pl_Rd_Codes_Check(term)

#define Rd_Codes(term) Pl_Rd_Codes(term)

#define Rd_Chars_Str_Check(term, str) Pl_Rd_Chars_Str_Check(term, str)

#define Rd_Chars_Str(term, str) Pl_Rd_Chars_Str(term, str)

#define Rd_Codes_Str_Check(term, str) Pl_Rd_Codes_Str_Check(term, str)

#define Rd_Codes_Str(term, str) Pl_Rd_Codes_Str(term, str)

#define Rd_List_Check(term) Pl_Rd_List_Check(term)

#define Rd_List(term) Pl_Rd_List(term)

#define Rd_Proper_List_Check(term, arg) Pl_Rd_Proper_List_Check(term, arg)

#define Rd_Proper_List(term, arg) Pl_Rd_Proper_List(term, arg)

#define Rd_Compound_Check(term, func, arity) Pl_Rd_Compound_Check(term, func, arity)

#define Rd_Compound(term, func, arity) Pl_Rd_Compound(term, func, arity)

#define Rd_Callable_Check(term, func, arity) Pl_Rd_Callable_Check(term, func, arity)

#define Rd_Callable(term, func, arity) Pl_Rd_Callable(term, func, arity)



#define Check_For_Un_Integer(term) Pl_Check_For_Un_Integer(term)

#define Check_For_Un_Positive(term) Pl_Check_For_Un_Positive(term)

#define Check_For_Un_Float(term) Pl_Check_For_Un_Float(term)

#define Check_For_Un_Number(term) Pl_Check_For_Un_Number(term)

#define Check_For_Un_Atom(term) Pl_Check_For_Un_Atom(term)

#define Check_For_Un_Boolean(term) Pl_Check_For_Un_Boolean(term)

#define Check_For_Un_Char(term) Pl_Check_For_Un_Char(term)

#define Check_For_Un_In_Char(term) Pl_Check_For_Un_In_Char(term)

#define Check_For_Un_Code(term) Pl_Check_For_Un_Code(term)

#define Check_For_Un_In_Code(term) Pl_Check_For_Un_In_Code(term)

#define Check_For_Un_Byte(term) Pl_Check_For_Un_Byte(term)

#define Check_For_Un_In_Byte(term) Pl_Check_For_Un_In_Byte(term)

#define Check_For_Un_String(term) Pl_Check_For_Un_String(term)

#define Check_For_Un_Chars(term) Pl_Check_For_Un_Chars(term)

#define Check_For_Un_Codes(term) Pl_Check_For_Un_Codes(term)

#define Check_For_Un_List(term) Pl_Check_For_Un_List(term)

#define Check_For_Un_Compound(term) Pl_Check_For_Un_Compound(term)

#define Check_For_Un_Callable(term) Pl_Check_For_Un_Callable(term)

#define Check_For_Un_Variable(term) Pl_Check_For_Un_Variable(term)



#define Un_Integer_Check(value, term) Pl_Un_Integer_Check(value, term)

#define Un_Integer(value, term) Pl_Un_Integer(value, term)

#define Un_Positive_Check(value, term) Pl_Un_Positive_Check(value, term)

#define Un_Positive(value, term) Pl_Un_Positive(value, term)

#define Un_Float_Check(value, term) Pl_Un_Float_Check(value, term)

#define Un_Float(value, term) Pl_Un_Float(value, term)

#define Un_Number_Check(value, term) Pl_Un_Number_Check(value, term)

#define Un_Number(value, term) Pl_Un_Number(value, term)

#define Un_Atom_Check(value, term) Pl_Un_Atom_Check(value, term)

#define Un_Atom(value, term) Pl_Un_Atom(value, term)

#define Un_Boolean_Check(value, term) Pl_Un_Boolean_Check(value, term)

#define Un_Boolean(value, term) Pl_Un_Boolean(value, term)

#define Un_Char_Check(value, term) Pl_Un_Char_Check(value, term)

#define Un_Char(value, term) Pl_Un_Char(value, term)

#define Un_In_Char_Check(value, term) Pl_Un_In_Char_Check(value, term)

#define Un_In_Char(value, term) Pl_Un_In_Char(value, term)

#define Un_Code_Check(value, term) Pl_Un_Code_Check(value, term)

#define Un_Code(value, term) Pl_Un_Code(value, term)

#define Un_In_Code_Check(value, term) Pl_Un_In_Code_Check(value, term)

#define Un_In_Code(value, term) Pl_Un_In_Code(value, term)

#define Un_Byte_Check(value, term) Pl_Un_Byte_Check(value, term)

#define Un_Byte(value, term) Pl_Un_Byte(value, term)

#define Un_In_Byte_Check(value, term) Pl_Un_In_Byte_Check(value, term)

#define Un_In_Byte(value, term) Pl_Un_In_Byte(value, term)

#define Un_String_Check(value, term) Pl_Un_String_Check(value, term)

#define Un_String(value, term) Pl_Un_String(value, term)

#define Un_Chars_Check(value, term) Pl_Un_Chars_Check(value, term)

#define Un_Chars(value, term) Pl_Un_Chars(value, term)

#define Un_Codes_Check(value, term) Pl_Un_Codes_Check(value, term)

#define Un_Codes(value, term) Pl_Un_Codes(value, term)

#define Un_List_Check(arg, term) Pl_Un_List_Check(arg, term)

#define Un_List(arg, term) Pl_Un_List(arg, term)

#define Un_Proper_List_Check(n, arg, term) Pl_Un_Proper_List_Check(n, arg, term)

#define Un_Proper_List(n, arg, term) Pl_Un_Proper_List(n, arg, term)

#define Un_Compound_Check(func, arity, arg, term) Pl_Un_Compound_Check(func, arity, arg, term)

#define Un_Compound(func, arity, arg, term) Pl_Un_Compound(func, arity, arg, term)

#define Un_Callable_Check(func, arity, arg, term) Pl_Un_Callable_Check(func, arity, arg, term)

#define Un_Callable(func, arity, arg, term) Pl_Un_Callable(func, arity, arg, term)

#define Un_Term(term1, term2) Pl_Un_Term(term1, term2)



#define Mk_Integer(value) Pl_Mk_Integer(value)

#define Mk_Positive(value) Pl_Mk_Positive(value)

#define Mk_Float(value) Pl_Mk_Float(value)

#define Mk_Number(value) Pl_Mk_Number(value)

#define Mk_Atom(value) Pl_Mk_Atom(value)

#define Mk_Boolean(value) Pl_Mk_Boolean(value)

#define Mk_Char(value) Pl_Mk_Char(value)

#define Mk_In_Char(value) Pl_Mk_In_Char(value)

#define Mk_Code(value) Pl_Mk_Code(value)

#define Mk_In_Code(value) Pl_Mk_In_Code(value)

#define Mk_Byte(value) Pl_Mk_Byte(value)

#define Mk_In_Byte(value) Pl_Mk_In_Byte(value)

#define Mk_String(value) Pl_Mk_String(value)

#define Mk_Chars(value) Pl_Mk_Chars(value)

#define Mk_Codes(value) Pl_Mk_Codes(value)

#define Mk_List(arg) Pl_Mk_List(arg)

#define Mk_Proper_List(n, arg) Pl_Mk_Proper_List(n, arg)

#define Mk_Compound(func, arity, arg) Pl_Mk_Compound(func, arity, arg)

#define Mk_Callable(func, arity, arg) Pl_Mk_Callable(func, arity, arg)

#define Mk_Variable() Pl_Mk_Variable()




#define Blt_Var(term)  Pl_Builtin_Var(term)

#define Blt_Non_Var(term)  Pl_Builtin_Non_Var(term)

#define Blt_Atom(term)  Pl_Builtin_Atom(term)

#define Blt_Integer(term)  Pl_Builtin_Integer(term)

#define Blt_Float(term)  Pl_Builtin_Float(term)

#define Blt_Number(term)  Pl_Builtin_Number(term)

#define Blt_Atomic(term)  Pl_Builtin_Atomic(term)

#define Blt_Compound(term)  Pl_Builtin_Compound(term)

#define Blt_Callable(term)  Pl_Builtin_Callable(term)

#define Blt_Fd_Var(term)  Pl_Builtin_Fd_Var(term)

#define Blt_Non_Fd_Var(term)  Pl_Builtin_Non_Fd_Var(term)

#define Blt_Generic_Var(term)  Pl_Builtin_Generic_Var(term)

#define Blt_Non_Generic_Var(term)  Pl_Builtin_Non_Generic_Var(term)

#define Blt_List(term)  Pl_Builtin_List(term)

#define Blt_Partial_List(term)  Pl_Builtin_Partial_List(term)

#define Blt_List_Or_Partial_List(term)  Pl_Builtin_List_Or_Partial_List(term)


#define Blt_Term_Eq(term1, term2)  Pl_Builtin_Term_Eq(term1, term2)

#define Blt_Term_Neq(term1, term2)  Pl_Builtin_Term_Neq(term1, term2)

#define Blt_Term_Lt(term1, term2)  Pl_Builtin_Term_Lt(term1, term2)

#define Blt_Term_Lte(term1, term2)  Pl_Builtin_Term_Lte(term1, term2)

#define Blt_Term_Gt(term1, term2)  Pl_Builtin_Term_Gt(term1, term2)

#define Blt_Term_Gte(term1, term2)  Pl_Builtin_Term_Gte(term1, term2)


#define Blt_Eq(expr1, expr2)  Pl_Builtin_Eq(expr1, expr2)

#define Blt_Neq(expr1, expr2)  Pl_Builtin_Neq(expr1, expr2)

#define Blt_Lt(expr1, expr2)  Pl_Builtin_Lt(expr1, expr2)

#define Blt_Lte(expr1, expr2)  Pl_Builtin_Lte(expr1, expr2)

#define Blt_Gt(expr1, expr2)  Pl_Builtin_Gt(expr1, expr2)

#define Blt_Gte(expr1, expr2)  Pl_Builtin_Gte(expr1, expr2)


#define Math_Load_Value(expr, result)   Pl_Math_Evaluate(expr, result)





#define Type_Of_Term(term) Pl_Type_Of_Term(term)



#define Term_Size(term) Pl_Term_Size(term)

#define Copy_Term(dst_term, src_term) Pl_Copy_Term(dst_term, src_term)

#define Copy_Contiguous_Term(dst_term, src_term) Pl_Copy_Contiguous_Term(dst_term, src_term)



#define Set_C_Bip_Name(functor, arity)  Pl_Set_C_Bip_Name(functor, arity)

#define Unset_C_Bip_Name()  Pl_Unset_C_Bip_Name()



#define Emit_Syntax_Error(file_name, err_line, err_col, err_msg) \
  Pl_Emit_Syntax_Error(file_name, err_line, err_col, err_msg)



#define Os_Error()  Pl_Os_Error()

#define PLV   PL_PLV
#define FDV   PL_FDV
#define INT   PL_INT
#define FLT   PL_FLT
#define ATM   PL_ATM
#define LST   PL_LST
#define STC   PL_STC



#define INT_LOWEST_VALUE   PL_MIN_INTEGER
#define INT_GREATEST_VALUE PL_MAX_INTEGER


#define Unify(term1, term2) Pl_Unif(term1, term2)

#define Unify_With_Occurs_Check(term1, term2) Pl_Unif_With_Occurs_Check(term1, term2)


#endif /* !__GPROLOG_FOREIGN_STRICT__ */





#ifdef __cplusplus
}
#endif
#endif /* !_GPROLOG_H */
