/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : c_supp.h                                                        *
 * Descr.: C interface support - header file                               *
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

long Rd_Integer_Check(WamWord start_word);

long Rd_Integer(WamWord start_word);

long Rd_Positive_Check(WamWord start_word);

long Rd_Positive(WamWord start_word);

double Rd_Float_Check(WamWord start_word);

double Rd_Float(WamWord start_word);

double Rd_Number_Check(WamWord start_word);

double Rd_Number(WamWord start_word);

int Rd_Atom_Check(WamWord start_word);

int Rd_Atom(WamWord start_word);

int Rd_Boolean_Check(WamWord start_word);

int Rd_Boolean(WamWord start_word);

int Rd_Char_Check(WamWord start_word);

int Rd_Char(WamWord start_word);

int Rd_In_Char_Check(WamWord start_word);

int Rd_In_Char(WamWord start_word);

int Rd_Code_Check(WamWord start_word);

int Rd_Code(WamWord start_word);

int Rd_In_Code_Check(WamWord start_word);

int Rd_In_Code(WamWord start_word);

int Rd_Byte_Check(WamWord start_word);

int Rd_Byte(WamWord start_word);

int Rd_In_Byte_Check(WamWord start_word);

int Rd_In_Byte(WamWord start_word);

char *Rd_String_Check(WamWord start_word);

char *Rd_String(WamWord start_word);

char *Rd_Chars_Check(WamWord start_word);

char *Rd_Chars(WamWord start_word);

char *Rd_Codes_Check(WamWord start_word);

char *Rd_Codes(WamWord start_word);

int Rd_Chars_Str_Check(WamWord start_word, char *str);

int Rd_Chars_Str(WamWord start_word, char *str);

int Rd_Codes_Str_Check(WamWord start_word, char *str);

int Rd_Codes_Str(WamWord start_word, char *str);

WamWord *Rd_List_Check(WamWord start_word);

WamWord *Rd_List(WamWord start_word);

int Rd_Proper_List_Check(WamWord start_word, WamWord *arg);

int Rd_Proper_List(WamWord start_word, WamWord *arg);

WamWord *Rd_Compound_Check(WamWord start_word, int *func, int *arity);

WamWord *Rd_Compound(WamWord start_word, int *func, int *arity);

WamWord *Rd_Callable_Check(WamWord start_word, int *func, int *arity);

WamWord *Rd_Callable(WamWord start_word, int *func, int *arity);



void Check_For_Un_Integer(WamWord start_word);

void Check_For_Un_Positive(WamWord start_word);

void Check_For_Un_Float(WamWord start_word);

void Check_For_Un_Number(WamWord start_word);

void Check_For_Un_Atom(WamWord start_word);

void Check_For_Un_Boolean(WamWord start_word);

void Check_For_Un_Char(WamWord start_word);

void Check_For_Un_In_Char(WamWord start_word);

void Check_For_Un_Code(WamWord start_word);

void Check_For_Un_In_Code(WamWord start_word);

void Check_For_Un_Byte(WamWord start_word);

void Check_For_Un_In_Byte(WamWord start_word);

void Check_For_Un_String(WamWord start_word);

void Check_For_Un_Chars(WamWord start_word);

void Check_For_Un_Codes(WamWord start_word);

void Check_For_Un_List(WamWord start_word);

void Check_For_Un_Compound(WamWord start_word);

void Check_For_Un_Callable(WamWord start_word);

void Check_For_Un_Variable(WamWord start_word);



Bool Un_Integer_Check(long value, WamWord start_word);

Bool Un_Integer(long value, WamWord start_word);

Bool Un_Positive_Check(long value, WamWord start_word);

Bool Un_Positive(long value, WamWord start_word);

Bool Un_Float_Check(double value, WamWord start_word);

Bool Un_Float(double value, WamWord start_word);

Bool Un_Number_Check(double value, WamWord start_word);

Bool Un_Number(double value, WamWord start_word);

Bool Un_Atom_Check(int value, WamWord start_word);

Bool Un_Atom(int value, WamWord start_word);

Bool Un_Boolean_Check(int value, WamWord start_word);

Bool Un_Boolean(int value, WamWord start_word);

Bool Un_Char_Check(int value, WamWord start_word);

Bool Un_Char(int value, WamWord start_word);

Bool Un_In_Char_Check(int value, WamWord start_word);

Bool Un_In_Char(int value, WamWord start_word);

Bool Un_Code_Check(int value, WamWord start_word);

Bool Un_Code(int value, WamWord start_word);

Bool Un_In_Code_Check(int value, WamWord start_word);

Bool Un_In_Code(int value, WamWord start_word);

Bool Un_Byte_Check(int value, WamWord start_word);

Bool Un_Byte(int value, WamWord start_word);

Bool Un_In_Byte_Check(int value, WamWord start_word);

Bool Un_In_Byte(int value, WamWord start_word);

Bool Un_String_Check(char *value, WamWord start_word);

Bool Un_String(char *value, WamWord start_word);

Bool Un_Chars_Check(char *value, WamWord start_word);

Bool Un_Chars(char *value, WamWord start_word);

Bool Un_Codes_Check(char *value, WamWord start_word);

Bool Un_Codes(char *value, WamWord start_word);

Bool Un_List_Check(WamWord *arg, WamWord start_word);

Bool Un_List(WamWord *arg, WamWord start_word);

Bool Un_Proper_List_Check(int n, WamWord *arg, WamWord start_word);

Bool Un_Proper_List(int n, WamWord *arg, WamWord start_word);

Bool Un_Compound_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);

Bool Un_Compound(int func, int arity, WamWord *arg, WamWord start_word);

Bool Un_Callable_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);

Bool Un_Callable(int func, int arity, WamWord *arg, WamWord start_word);



WamWord Mk_Integer(long value);

WamWord Mk_Positive(long value);

WamWord Mk_Float(double value);

WamWord Mk_Number(double value);

WamWord Mk_Atom(int value);

WamWord Mk_Boolean(int value);

WamWord Mk_Char(int value);

WamWord Mk_In_Char(int value);

WamWord Mk_Code(int value);

WamWord Mk_In_Code(int value);

WamWord Mk_Byte(int value);

WamWord Mk_In_Byte(int value);

WamWord Mk_String(char *value);

WamWord Mk_Chars(char *value);

WamWord Mk_Codes(char *value);

WamWord Mk_List(WamWord *arg);

WamWord Mk_Proper_List(int n, WamWord *arg);

WamWord Mk_Compound(int func, int arity, WamWord *arg);

WamWord Mk_Callable(int func, int arity, WamWord *arg);

WamWord Mk_Variable(void);
