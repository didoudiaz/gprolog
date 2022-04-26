/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : context.h                                                       *
 * Descr.: context function declarations header file                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 2003 Salvador Abreu                                       *
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

/* $Id: context.h,v 1.2 2006/07/15 19:59:35 spa Exp $ */

extern PredInf *Cxt_Lookup_Pred_With_K (long key, WamWord cxt_call_K) FC;
extern PredInf *Cxt_Lookup_Pred (long key) FC;
extern WamCont Cxt_Call_Tagged(WamWord key, WamWord call_K) FC;
extern WamCont Cxt_Call(int func, int arity, WamWord call_K) FC;
extern void Cxt_Assign_K(WamWord new_K) FC;
extern Bool Cxt_Arg_Load(int arg_no, WamWord t_word, WamWord *st_word) FC;
extern Bool Cxt_Arg_Unify(int arg_no, WamWord t_word, WamWord st_word) FC;
