/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : development only                                                *
 * File  : t_c.c                                                           *
 * Descr.: test - C part                                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

/*
 * You can put your own test code in these files (see DEVELOPMENT)
 *    t.pl    (Prolog part)
 *    t_c.c   (C part, eg. foreign code or C code used by your FD constraints)
 *    t_fd.fd (FD constraint part)
 */

#include <stdio.h>
#include <stdlib.h>

#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"




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

/*-------------------------------------------------------------------------*
 *                                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#if 0
void
Dist_LE(Range *s1, long s2, long d, long size_y)
{
  /* NB: due to the new Sparse domain representation, this method might *
   * Not work as expected */
  int x1 = s2 / size_y;
  int y1 = s2 % size_y;
  int x2, y2, n;
  int size_x = size_y;

  for(x2 = x1 - d; x2 <= x1 + d; x2++)
    {
      if (x2 < 0)
	continue;
      if (x2 >= size_x)
	break;

      n = d - abs(x1 - x2);
      for(y2 = y1 - n; y2 <= y1 + n; y2++)
	if (y2 >= 0 && y2 < size_y)
	  Range_Set_Value(s1, x2 * size_y + y2);
    }

}
#endif

