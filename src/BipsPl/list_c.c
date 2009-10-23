/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : list_c.c                                                        * 
 * Descr.: list library  - C part                                          *
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

#include "engine_pl.h"
#include "bips_pl.h"



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
 * Pl_NTH0_3                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Nth0_3(WamWord n_word, WamWord list_word, WamWord res_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int n = Pl_Rd_Integer(n_word);

  if (n < 0)
    return FALSE;

  for(;;)
    {
      DEREF(list_word, word, tag_mask);
      if (tag_mask != TAG_LST_MASK)
	return FALSE;

      adr = UnTag_LST(word);

      if (n == 0)
	return Pl_Unify(Car(adr), res_word);

      n--;
      list_word = Cdr(adr);
    }
}
