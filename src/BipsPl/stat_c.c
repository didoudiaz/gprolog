/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stat_c.c                                                        *
 * Descr.: statistics predicate management - C part                        *
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

static PlLong last_user_time = 0;
static PlLong last_system_time = 0;
static PlLong last_cpu_time = 0;
static PlLong last_real_time = 0;

static unsigned long      last_collection_cycles = 0;
static unsigned long long last_collected_bytes = 0;
static PlLong	          last_collection_time = 0;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Stack_Size(int stack_nb, int *used, int *free);




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_0                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Statistics_0(void)
{
  StmInf *pstm = pl_stm_tbl[pl_stm_stdout];
  int used, free;
  PlLong t[4], l[4];
  static char *n[4] = { "user", "system", "cpu", "real" };
  int i;

  Pl_Stream_Printf(pstm, "Memory                  limit         in use            free\n\n");


  for (i = 0; i < NB_OF_STACKS; i++)
    {
      Stack_Size(i, &used, &free);
      if (used + free == 0)	/* ie. size=0 (e.g. cstr_stack) */
	continue;

      used /= 1024;
      free /= 1024;

      Pl_Stream_Printf(pstm, "     %-6s stack  %10d Kb   %10d Kb   %10d Kb\n",
		       pl_stk_tbl[i].name, used + free, used, free);
    }


#if 1
  Pl_Stream_Printf(pstm, "     atom   table  %10d atoms%10d atoms%10d atoms\n",
		   pl_max_atom , pl_nb_atom, pl_max_atom - pl_nb_atom);
#else
  Pl_Stream_Printf(pstm, "\nAtoms: %10d  %10d max\n", pl_nb_atom, pl_max_atom);
#endif
  

  t[0] = Pl_M_User_Time();
  l[0] = t[0] - last_user_time;

  t[1] = Pl_M_System_Time();
  l[1] = t[1] - last_system_time;

  t[2] = t[0] + t[1];
  l[2] = t[2] - last_cpu_time;

  t[3] = Pl_M_Real_Time();
  l[3] = t[3] - last_real_time;

  last_user_time = t[0];
  last_system_time = t[1];
  last_cpu_time = t[2];
  last_real_time = t[3];

  Pl_Stream_Printf(pstm,
		"\nTimes                 since start      since last\n\n");

  for (i = 0; i < 4; i++)
    Pl_Stream_Printf(pstm, "     %-6s  time %11.3f sec %11.3f sec\n",
		  n[i], (double) t[i] / 1000.0, (double) l[i] / 1000.0);

#ifdef BOEHM_GC
  unsigned long      c = Pl_GC_Nb_Collections();
  unsigned long long b = Pl_GC_Collected_Bytes();
  PlLong             m = Pl_GC_Collection_Time_Millis();
  Pl_Stream_Printf(pstm,
                "\nGarbage collection    since start      since last\n\n");
  Pl_Stream_Printf(pstm,
                  "     #collections %11lu     %11lu\n",
                    c, c - last_collection_cycles);
  Pl_Stream_Printf(pstm,
                  "     collected    %11llu Kb  %11llu Kb\n",
                    b/1024, (b - last_collected_bytes)/1024);
  if (m != 0)
    Pl_Stream_Printf(pstm,
                  "     time (cpu)   %11.3f sec %11.3f sec\n",
                    (double)m/ 1000.0, (double)(m - last_collection_time)/ 1000.0);
  last_collection_cycles = c;
  last_collected_bytes = b;
  last_collection_time = m;
#endif // BOEHM_GC
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_USER_TIME_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_User_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  PlLong user_time;
  int since_start, since_last;

  user_time = Pl_M_User_Time();
  since_start = user_time;
  since_last = user_time - last_user_time;
  last_user_time = user_time;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_SYSTEM_TIME_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_System_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  PlLong system_time;
  int since_start, since_last;

  system_time = Pl_M_System_Time();
  since_start = system_time;
  since_last = system_time - last_system_time;
  last_system_time = system_time;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_CPU_TIME_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Cpu_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  PlLong cpu_time;
  int since_start, since_last;

  cpu_time = Pl_M_User_Time() + Pl_M_System_Time();
  since_start = cpu_time;
  since_last = cpu_time - last_cpu_time;
  last_cpu_time = cpu_time;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_REAL_TIME_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Real_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  PlLong real_time;
  int since_start, since_last;

  real_time = Pl_M_Real_Time();
  since_start = real_time;
  since_last = real_time - last_real_time;
  last_real_time = real_time;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_LOCAL_STACK_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Local_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (pl_stk_tbl[i].stack == Local_Stack)
      Stack_Size(i, &used, &free);

  return Pl_Un_Integer_Check(used, used_word) &&
    Pl_Un_Integer_Check(free, free_word);
}





/*-------------------------------------------------------------------------*
 * PL_STATISTICS_GLOBAL_STACK_2                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Global_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (pl_stk_tbl[i].stack == Global_Stack)
      Stack_Size(i, &used, &free);

  return Pl_Un_Integer_Check(used, used_word) &&
    Pl_Un_Integer_Check(free, free_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_TRAIL_STACK_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Trail_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (pl_stk_tbl[i].stack == Trail_Stack)
      Stack_Size(i, &used, &free);

  return Pl_Un_Integer_Check(used, used_word) &&
    Pl_Un_Integer_Check(free, free_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_CSTR_STACK_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Cstr_Stack_2(WamWord used_word, WamWord free_word)
{
  int i, used, free;

  for (i = 0; i < NB_OF_STACKS; i++)
    if (pl_stk_tbl[i].stack == Cstr_Stack)
      Stack_Size(i, &used, &free);

  return Pl_Un_Integer_Check(used, used_word) &&
    Pl_Un_Integer_Check(free, free_word);
}




/*-------------------------------------------------------------------------*
 * PL_STATISTICS_ATOMS_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Atoms_2(WamWord used_word, WamWord free_word)
{
  return Pl_Un_Integer_Check(pl_nb_atom, used_word) &&
    Pl_Un_Integer_Check(pl_max_atom - pl_nb_atom, free_word);
}


#ifdef BOEHM_GC
/*-------------------------------------------------------------------------*
 * Pl_Statistics_Garbage_Collection_2                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Garbage_Collection_3(WamWord nb_collections,
    WamWord nb_freed, WamWord gc_time_millis)
{
  PlLong c = (PlLong)Pl_GC_Nb_Collections();
  PlLong b = (PlLong)Pl_GC_Collected_Bytes();
  PlLong t = (PlLong)Pl_GC_Collection_Time_Millis();
  return Pl_Un_Integer_Check(c, nb_collections) &&
	  Pl_Un_Integer_Check(b, nb_freed) &&
	  Pl_Un_Integer_Check(t, gc_time_millis);
}


/*-------------------------------------------------------------------------*
 * Pl_Statistics_Garbage_Collection_2                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Collection_Cycles_2(WamWord since_start_word, WamWord since_last_word)
{
  unsigned long val;
  PlLong since_start, since_last;

  val = Pl_GC_Nb_Collections();
  since_start = val;
  since_last = val - last_collection_cycles;
  last_collection_cycles = val;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}


/*-------------------------------------------------------------------------*
 * Pl_Statistics_Garbage_Collection_2                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Collected_Bytes_2(WamWord since_start_word, WamWord since_last_word)
{
  size_t val;
  PlLong since_start, since_last;

  val = Pl_GC_Collected_Bytes();
  since_start = val;
  since_last = val - last_collected_bytes;
  last_collected_bytes = val;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}


/*-------------------------------------------------------------------------*
 * Pl_Statistics_Garbage_Collection_2                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Statistics_Collection_Time_2(WamWord since_start_word, WamWord since_last_word)
{
  PlLong val;
  PlLong since_start, since_last;

  val = Pl_GC_Collection_Time_Millis();
  since_start = val;
  since_last = val - last_collection_time;
  last_collection_time = val;

  return Pl_Un_Integer_Check(since_start, since_start_word) &&
    Pl_Un_Integer_Check(since_last, since_last_word);
}
#endif // BOEHM_GC



/*-------------------------------------------------------------------------*
 * STACK_SIZE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Stack_Size(int stack_nb, int *used, int *free)
{
  *used = Stack_Top(stack_nb) - pl_stk_tbl[stack_nb].stack;
  *free = pl_stk_tbl[stack_nb].size - *used;

  if (pl_stk_tbl[stack_nb].stack == Global_Stack)	/* see Init_Wam_Engine */
    *used += REG_BANK_SIZE;

  *used *= sizeof(WamWord);
  *free *= sizeof(WamWord);
}




/*-------------------------------------------------------------------------*
 * PL_USER_TIME_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_User_Time_1(WamWord since_start_word)
{
  return Pl_Un_Integer_Check(Pl_M_User_Time(), since_start_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYSTEM_TIME_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_System_Time_1(WamWord since_start_word)
{
  return Pl_Un_Integer_Check(Pl_M_System_Time(), since_start_word);
}




/*-------------------------------------------------------------------------*
 * PL_CPU_TIME_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Cpu_Time_1(WamWord since_start_word)
{
  return Pl_Un_Integer_Check(Pl_M_User_Time() + Pl_M_System_Time(),
			  since_start_word);
}




/*-------------------------------------------------------------------------*
 * PL_REAL_TIME_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Real_Time_1(WamWord since_start_word)
{
  return Pl_Un_Integer_Check(Pl_M_Real_Time(), since_start_word);
}
