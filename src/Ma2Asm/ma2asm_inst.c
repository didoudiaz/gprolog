/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma2asm_inst.c                                                   *
 * Descr.: translation file                                                *
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

#define MAPPER_FILE

#include "ma_parser.h"
#include "ma_protos.h"

#include "../EnginePl/gp_config.h"
#include "../EnginePl/wam_regs.h"
#include "../EnginePl/machine.h"
#include "../EnginePl/pl_params.h"
#include "../EnginePl/obj_chain.h"


#if 0				/* to force the inclusion of a mapper */

#include "ix86_any.c"

#else

	  /* include machine-dependent mapper file */

#if defined(M_ix86_win32)

#include "ix86_win32.c"

#elif defined(M_ix86)

#include "ix86_any.c"

#elif defined(M_sparc)

#include "sparc_any.c"

#elif defined(M_alpha)

#include "alpha_any.c"

#elif defined(M_mips_irix)

#include "mips_irix.c"

#elif defined(M_powerpc_linux)

#include "ppc_linux.c"

#endif


#endif
