/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bc_supp.h                                                       *
 * Descr.: byte-Code support - header file                                 *
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

#ifdef BC_SUPP_FILE

int pl_byte_len;

#else

extern int pl_byte_len;

#endif

				/* defined as this to avoid to force the   */
				/* inclusion of bc_supp.o if not needed    */
				/* (dynam_supp.c uses pl_byte_code)        */
unsigned *pl_byte_code;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

WamCont Pl_BC_Emulate_Pred(int func, DynPInf *dyn);

void Pl_BC_Start_Emit_0(void);
void Pl_BC_Stop_Emit_0(void);
void Pl_BC_Emit_Inst_1(WamWord inst_word);
void Pl_BC_Emit_Inst_Execute_Native(int func, int arity, PlLong *codep);

