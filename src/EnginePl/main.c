/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog engine                                                   */
/* File  : main.c                                                          */
/* Descr.: main                                                            */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>

#include "engine_pl.h"


/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

/*-------------------------------------------------------------------------*/
/* MAIN                                                                    */
/*                                                                         */
/*-------------------------------------------------------------------------*/
int main(int argc,char *argv[])

{
 int  nb_user_directive;
 Bool top_level;

 nb_user_directive=Start_Prolog(argc,argv);

 top_level=Try_Execute_Top_Level();

 Stop_Prolog();

 if (top_level)
     return 0;

 if (nb_user_directive)
     return 0;

 fprintf(stderr,
         "Warning: no initial goal executed\n"
         "   use a directive :- initialization(Goal)\n"
         "   or remove the link option --no-top-level"
         " (or --min-bips or --min-size)\n");

 return 1;
}
