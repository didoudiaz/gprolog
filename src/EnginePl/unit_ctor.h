/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : unit_ctor.h                                                     *
 * Descr.: Prolog unit initialization - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h"
#include "bool.h"
#include "../Tools/ctor.h"


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

void Pl_Initialize_Units(void);

void Pl_Register_Unit(void (*fct_unit_init)(), void (*fct_exec_system) (),
		      void (*fct_exec_user) ());



#define PL_INITIALIZER(f)		\
  static void f(void);			\
  INITIALIZER(f##_ctor)			\
  {					\
    Pl_Register_Unit(f, NULL, NULL);	\
  }					\
  static void f(void) 




#if (defined(_MSC_VER) || defined(M_darwin)) && !defined(UNIT_CHAIN_REVERSE_ORDER)
#define UNIT_CHAIN_REVERSE_ORDER
#endif

#if 0
#define UNIT_CHAIN_REVERSE_ORDER
#endif

