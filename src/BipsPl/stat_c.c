/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stat_c.c                                                        *
 * Descr.: statistics predicate management - C part                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
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

static long last_user_time = 0;
static long last_system_time = 0;
static long last_cpu_time = 0;
static long last_real_time = 0;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Stack_Size(int stack_nb, int *used, int *free);




/*-------------------------------------------------------------------------*
 * STATISTICS_0                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Statistics_0(void)
{
  StmInf *pstm = stm_tbl[stm_stdout];
  int used, free;
  long t[4], l[4];
  static char *n[4] = { "user", "system", "cpu", "real" };
  int i;

  Stream_Printf(pstm,
		"Stacks                size          in use            free\n\n");


  for (i = 0; i < NB_OF_STACKS; i++)
    {
      Stack_Size(i, &used, &free);
      if (used + free == 0)	/* ie. size=0 (e.g. cstr_stack) */
	continue;

      used /= 1024;
      free /= 1024;

      Stream_Printf(pstm,
		    "   %-6s stack %10d Kb   %10d Kb   %10d Kb\n",
		    stk_tbl[i].name, used + free, used, free);
    }

  t[0] = M_User_Time();
  l[0] = t[0] - last_user_time;

  t[1] = M_System_Time();
  l[1] = t[1] - last_system_time;

  t[2] = t[0] + t[1];
  l[2] = t[2] - last_cpu_time;

  t[3] = M_Real_Time();
  l[3] = t[3] - last_real_time;

  last_user_time = t[0];
  last_system_time = t[1];
  last_cpu_time = t[2];
  last_real_time = t[3];

  Stream_Printf(pstm,
		"\nTimes              since start      since last\n\n");

  for (i = 0; i < 4; i++)
    Stream_Printf(pstm, "   %-6s time %11.3f sec %11.3f sec\n",
		  n[i], (double) t[i] / 1000.0, (double) l[i] / 1000.0);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_USER_TIME_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_User_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  long user_time;
  int since_start, since_last;

  user_time = M_User_Time();
  since_start = user_time;
  since_last = user_time - last_user_time;
  last_user_time = user_time;

  return Un_Integer_Check(since_start, since_start_word) &&
    Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_SYSTEM_TIME_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_System_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  long system_time;
  int since_start, since_last;

  system_time = M_System_Time();
  since_start = system_time;
  since_last = system_time - last_system_time;
  last_system_time = system_time;

  return Un_Integer_Check(since_start, since_start_word) &&
    Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_CPU_TIME_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Cpu_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  long cpu_time;
  int since_start, since_last;

  cpu_time = M_User_Time() + M_System_Time();
  since_start = cpu_time;
  since_last = cpu_time - last_cpu_time;
  last_cpu_time = cpu_time;

  return Un_Integer_Check(since_start, since_start_word) &&
    Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_REAL_TIME_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Real_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  long real_time;
  int since_start, since_last;

  real_time = M_Real_Time();
  since_start = real_time;
  since_last = real_time - last_real_time;
  last_real_time = real_time;

  return Un_Integer_Check(since_start, since_start_word) &&
    Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_LOCAL_STACK_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Local_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (stk_tbl[i].stack == Local_Stack)
      Stack_Size(i, &used, &free);

  return Un_Integer_Check(used, used_word) &&
    Un_Integer_Check(free, free_word);
}





/*-------------------------------------------------------------------------*
 * STATISTICS_GLOBAL_STACK_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Global_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (stk_tbl[i].stack == Global_Stack)
      Stack_Size(i, &used, &free);

  return Un_Integer_Check(used, used_word) &&
    Un_Integer_Check(free, free_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_TRAIL_STACK_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Trail_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (stk_tbl[i].stack == Trail_Stack)
      Stack_Size(i, &used, &free);

  return Un_Integer_Check(used, used_word) &&
    Un_Integer_Check(free, free_word);
}




/*-------------------------------------------------------------------------*
 * STATISTICS_CSTR_STACK_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Statistics_Cstr_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (stk_tbl[i].stack == Cstr_Stack)
      Stack_Size(i, &used, &free);

  return Un_Integer_Check(used, used_word) &&
    Un_Integer_Check(free, free_word);
}





/*-------------------------------------------------------------------------*
 * STACK_SIZE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Stack_Size(int stack_nb, int *used, int *free)
{
  *used = Stack_Top(stack_nb) - stk_tbl[stack_nb].stack;
  *free = stk_tbl[stack_nb].size - *used;

  if (stk_tbl[stack_nb].stack == Global_Stack)	/* see Init_Wam_Engine */
    *used += REG_BANK_SIZE;

  *used *= sizeof(WamWord);
  *free *= sizeof(WamWord);
}




/*-------------------------------------------------------------------------*
 * USER_TIME_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
User_Time_1(WamWord since_start_word)
{
  return Un_Integer_Check(M_User_Time(), since_start_word);
}




/*-------------------------------------------------------------------------*
 * SYSTEM_TIME_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
System_Time_1(WamWord since_start_word)
{
  return Un_Integer_Check(M_System_Time(), since_start_word);
}




/*-------------------------------------------------------------------------*
 * CPU_TIME_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Cpu_Time_1(WamWord since_start_word)
{
  return Un_Integer_Check(M_User_Time() + M_System_Time(),
			  since_start_word);
}




/*-------------------------------------------------------------------------*
 * REAL_TIME_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Real_Time_1(WamWord since_start_word)
{
  return Un_Integer_Check(M_Real_Time(), since_start_word);
}
