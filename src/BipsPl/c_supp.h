/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : c_supp.h                                                        *
 * Descr.: C interface support - header file                               *
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

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

PlLong Pl_Rd_Integer_Check(WamWord start_word);

PlLong Pl_Rd_Integer(WamWord start_word);

PlLong Pl_Rd_Positive_Check(WamWord start_word);

PlLong Pl_Rd_Positive(WamWord start_word);

double Pl_Rd_Float_Check(WamWord start_word);

double Pl_Rd_Float(WamWord start_word);

double Pl_Rd_Number_Check(WamWord start_word);

double Pl_Rd_Number(WamWord start_word);

int Pl_Rd_Atom_Check(WamWord start_word);

int Pl_Rd_Atom(WamWord start_word);

int Pl_Rd_Boolean_Check(WamWord start_word);

int Pl_Rd_Boolean(WamWord start_word);

int Pl_Rd_Char_Check(WamWord start_word);

int Pl_Rd_Char(WamWord start_word);

int Pl_Rd_In_Char_Check(WamWord start_word);

int Pl_Rd_In_Char(WamWord start_word);

int Pl_Rd_Code_Check(WamWord start_word);

int Pl_Rd_Code(WamWord start_word);

int Pl_Rd_In_Code_Check(WamWord start_word);

int Pl_Rd_In_Code(WamWord start_word);

int Pl_Rd_Byte_Check(WamWord start_word);

int Pl_Rd_Byte(WamWord start_word);

int Pl_Rd_In_Byte_Check(WamWord start_word);

int Pl_Rd_In_Byte(WamWord start_word);

char *Pl_Rd_String_Check(WamWord start_word);

char *Pl_Rd_String(WamWord start_word);

char *Pl_Rd_Chars_Check(WamWord start_word);

char *Pl_Rd_Chars(WamWord start_word);

char *Pl_Rd_Codes_Check(WamWord start_word);

char *Pl_Rd_Codes(WamWord start_word);

int Pl_Rd_Chars_Str_Check(WamWord start_word, char *str);

int Pl_Rd_Chars_Str(WamWord start_word, char *str);

int Pl_Rd_Codes_Str_Check(WamWord start_word, char *str);

int Pl_Rd_Codes_Str(WamWord start_word, char *str);

WamWord *Pl_Rd_List_Check(WamWord start_word);

WamWord *Pl_Rd_List(WamWord start_word);

int Pl_Rd_Proper_List_Check(WamWord start_word, WamWord *arg);

int Pl_Rd_Proper_List_Check2(WamWord start_word, WamWord *arg, 
			     WamWord (*elt_fct)(WamWord start_word));

int Pl_Rd_Proper_List(WamWord start_word, WamWord *arg);

WamWord *Pl_Rd_Compound_Check(WamWord start_word, int *func, int *arity);

WamWord *Pl_Rd_Compound(WamWord start_word, int *func, int *arity);

WamWord *Pl_Rd_Callable_Check(WamWord start_word, int *func, int *arity);

WamWord *Pl_Rd_Callable(WamWord start_word, int *func, int *arity);



void Pl_Check_For_Un_Integer(WamWord start_word);

void Pl_Check_For_Un_Positive(WamWord start_word);

void Pl_Check_For_Un_Float(WamWord start_word);

void Pl_Check_For_Un_Number(WamWord start_word);

void Pl_Check_For_Un_Atom(WamWord start_word);

void Pl_Check_For_Un_Boolean(WamWord start_word);

void Pl_Check_For_Un_Char(WamWord start_word);

void Pl_Check_For_Un_In_Char(WamWord start_word);

void Pl_Check_For_Un_Code(WamWord start_word);

void Pl_Check_For_Un_In_Code(WamWord start_word);

void Pl_Check_For_Un_Byte(WamWord start_word);

void Pl_Check_For_Un_In_Byte(WamWord start_word);

void Pl_Check_For_Un_String(WamWord start_word);

void Pl_Check_For_Un_Chars(WamWord start_word);

void Pl_Check_For_Un_Codes(WamWord start_word);

void Pl_Check_For_Un_List(WamWord start_word);

void Pl_Check_For_Un_List2(WamWord start_word, 
			   void (*elt_fct)(WamWord start_word));

void Pl_Check_For_Un_Compound(WamWord start_word);

void Pl_Check_For_Un_Callable(WamWord start_word);

void Pl_Check_For_Un_Variable(WamWord start_word);



Bool Pl_Un_Integer_Check(PlLong value, WamWord start_word);

Bool Pl_Un_Integer(PlLong value, WamWord start_word);

Bool Pl_Un_Positive_Check(PlLong value, WamWord start_word);

Bool Pl_Un_Positive(PlLong value, WamWord start_word);

Bool Pl_Un_Float_Check(double value, WamWord start_word);

Bool Pl_Un_Float(double value, WamWord start_word);

Bool Pl_Un_Number_Check(double value, WamWord start_word);

Bool Pl_Un_Number(double value, WamWord start_word);

Bool Pl_Un_Atom_Check(int value, WamWord start_word);

Bool Pl_Un_Atom(int value, WamWord start_word);

Bool Pl_Un_Boolean_Check(int value, WamWord start_word);

Bool Pl_Un_Boolean(int value, WamWord start_word);

Bool Pl_Un_Char_Check(int value, WamWord start_word);

Bool Pl_Un_Char(int value, WamWord start_word);

Bool Pl_Un_In_Char_Check(int value, WamWord start_word);

Bool Pl_Un_In_Char(int value, WamWord start_word);

Bool Pl_Un_Code_Check(int value, WamWord start_word);

Bool Pl_Un_Code(int value, WamWord start_word);

Bool Pl_Un_In_Code_Check(int value, WamWord start_word);

Bool Pl_Un_In_Code(int value, WamWord start_word);

Bool Pl_Un_Byte_Check(int value, WamWord start_word);

Bool Pl_Un_Byte(int value, WamWord start_word);

Bool Pl_Un_In_Byte_Check(int value, WamWord start_word);

Bool Pl_Un_In_Byte(int value, WamWord start_word);

Bool Pl_Un_String_Check(char *value, WamWord start_word);

Bool Pl_Un_String(char *value, WamWord start_word);

Bool Pl_Un_Chars_Check(char *value, WamWord start_word);

Bool Pl_Un_Chars(char *value, WamWord start_word);

Bool Pl_Un_Codes_Check(char *value, WamWord start_word);

Bool Pl_Un_Codes(char *value, WamWord start_word);

Bool Pl_Un_List_Check(WamWord *arg, WamWord start_word);

Bool Pl_Un_List(WamWord *arg, WamWord start_word);

Bool Pl_Un_Proper_List_Check(int n, WamWord *arg, WamWord start_word);

Bool Pl_Un_Proper_List(int n, WamWord *arg, WamWord start_word);

Bool Pl_Un_Compound_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);

Bool Pl_Un_Compound(int func, int arity, WamWord *arg, WamWord start_word);

Bool Pl_Un_Callable_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);

Bool Pl_Un_Callable(int func, int arity, WamWord *arg, WamWord start_word);

Bool Pl_Un_Term(WamWord term_word, WamWord start_word);



WamWord Pl_Mk_Integer(PlLong value);

WamWord Pl_Mk_Positive(PlLong value);

WamWord Pl_Mk_Float(double value);

WamWord Pl_Mk_Number(double value);

WamWord Pl_Mk_Atom(int value);

WamWord Pl_Mk_Boolean(int value);

WamWord Pl_Mk_Char(int value);

WamWord Pl_Mk_In_Char(int value);

WamWord Pl_Mk_Code(int value);

WamWord Pl_Mk_In_Code(int value);

WamWord Pl_Mk_Byte(int value);

WamWord Pl_Mk_In_Byte(int value);

WamWord Pl_Mk_String(char *value);

WamWord Pl_Mk_Chars(char *value);

WamWord Pl_Mk_Codes(char *value);

WamWord Pl_Mk_List(WamWord *arg);

WamWord Pl_Mk_Proper_List(int n, WamWord *arg);

WamWord Pl_Mk_Compound(int func, int arity, WamWord *arg);

WamWord Pl_Mk_Callable(int func, int arity, WamWord *arg);

WamWord Pl_Mk_Variable(void);
