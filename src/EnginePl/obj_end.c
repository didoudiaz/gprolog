/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_end.c                                                       *
 * Descr.: object chaining management - last object (end of the chain)     *
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

/* $Id: obj_end.c,v 1.1.1.1 2003/04/08 09:42:52 spa Exp $ */

#include "obj_chain.h"


#ifndef _MSC_VER

extern ObjChain obj_chain_begin;

ObjChain *obj_chain_end = &obj_chain_begin;

#else

#pragma data_seg(".INIT$z")

long obj_chain_end = (long) OBJ_CHAIN_MAGIC_2;

#pragma data_seg()

#endif