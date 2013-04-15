/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma2asm_inst.c                                                   *
 * Descr.: translation file                                                *
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


#define MAPPER_FILE

#include "ma_parser.h"
#include "ma_protos.h"

#include "../EnginePl/gp_config.h"
#include "../EnginePl/wam_regs.h"
#define FRAMES_ONLY
#include "../EnginePl/wam_inst.h"
#include "../EnginePl/machine.h"
#include "../EnginePl/pl_params.h"
#include "../EnginePl/obj_chain.h"

#define Y_OFFSET(index)   ((- ENVIR_STATIC_SIZE - 1 - index) * sizeof(PlLong))


#if 0				/* to force the inclusion of a mapper */

#define M_ix86_darwin
#include "ix86_any.c"

#else

	  /* include machine-dependent mapper file */

#if defined(M_ix86)

#include "ix86_any.c"

#elif defined(M_sparc)

#include "sparc_any.c"

#elif defined(M_alpha)

#include "alpha_any.c"

#elif defined(M_mips_irix)

#include "mips_irix.c"

#elif defined(M_powerpc)

#include "powerpc_any.c"

#elif defined(M_x86_64)

#include "x86_64_any.c"

#endif

#endif


