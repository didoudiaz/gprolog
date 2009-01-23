/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : oper_supp.h                                                     *
 * Descr.: FD Operation support - header file                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
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

unsigned Pl_Power(unsigned x, unsigned n);

unsigned Pl_Nth_Root_Dn(unsigned y, unsigned n);

unsigned Pl_Nth_Root_Up(unsigned y, unsigned n);

unsigned Pl_Nth_Root_Exact(unsigned y, unsigned n);

unsigned Pl_Sqrt_Dn(unsigned y);

unsigned Pl_Sqrt_Up(unsigned y);

unsigned Pl_Sqrt_Exact(unsigned y);

unsigned Pl_Find_Expon_Dn(unsigned x, unsigned y);

unsigned Pl_Find_Expon_Up(unsigned x, unsigned y);

unsigned Pl_Find_Expon_Exact(unsigned x, unsigned y);

void Pl_Full_Coeff_Power_Var(Range *y, int a, Range *n);

void Pl_Full_Find_Expon(Range *n, int a, Range *y);

void Pl_Full_Var_Power_Coeff(Range *y, Range *x, int a);

void Pl_Full_Nth_Root(Range *x, Range *y, int a);

void Full_Max_Cst_Var(Range *z, int a, Range *x);

void Full_Min_Cst_Var(Range *z, int a, Range *x);
