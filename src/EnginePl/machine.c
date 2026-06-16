/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.c                                                       *
 * Descr.: machine dependent features                                      *
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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#include <io.h>
#endif


#if defined(__unix__) || defined(__CYGWIN__)
#include <pwd.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif
#else /* _WIN32 */
#include <process.h>
#include <direct.h>
#endif

#include "engine_pl.h"          /* before netdb.h which declares a function */
                         /* gcc cannot define a global reg var after a fct */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifndef NO_USE_SOCKETS
#  if defined(__unix__) || defined(__CYGWIN__)
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#  endif
#define INET_MANAGEMENT
#endif


#if 0
#define DEBUG
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static PlLong start_user_time = 0;
static PlLong start_system_time = 0;
static PlLong start_real_time = 0;

static unsigned int cur_seed = 1;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#ifdef INET_MANAGEMENT

static char *Host_Name_From_Alias(struct hostent *host_entry);

#endif



#define Round_Up(x, y)          (((x) + (y) - 1) / (y) * (y))

#define Round_Down(x, y)        ((x) / (y) * (y))




/*-------------------------------------------------------------------------*
 * PL_INIT_MACHINE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Machine(void)
{
  tzset();

  start_user_time = Pl_User_Time();
  start_system_time = Pl_System_Time();
  start_real_time = Pl_Real_Time();

#if defined(HAVE_MALLOPT) && defined(M_MMAP_MAX)
  mallopt(M_MMAP_MAX, 0);
#endif
}




/*-------------------------------------------------------------------------*/
/* PL_USER_TIME                                                            */
/*                                                                         */
/* returns the user time used since the start of the process (in ms).      */
/*-------------------------------------------------------------------------*/
PlLong
Pl_User_Time(void)
{
  PlLong user_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  user_time = (rsr_usage.ru_utime.tv_sec * 1000) +
    (rsr_usage.ru_utime.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)
  FILETIME creat_t, exit_t, kernel_t, user_t;

  /* Success on Windows NT */
  if (GetProcessTimes(GetCurrentProcess(),
                      &creat_t, &exit_t, &kernel_t, &user_t))
    user_time = (PlLong) (((__int64) user_t.dwHighDateTime << 32) +
                        (__int64) user_t.dwLowDateTime) / 10000;
  else                          /* not implemented on Windows 95/98 */
    user_time = (PlLong) ((double) clock() * 1000 / CLOCKS_PER_SEC);

#else

  Pl_Fatal_Error("user time not available");
  return 0;

#endif

  return user_time - start_user_time;
}




/*-------------------------------------------------------------------------*
 * PL_SYSTEM_TIME                                                          *
 *                                                                         *
 * returns the system time used since the start of the process (in ms).    *
 *-------------------------------------------------------------------------*/
PlLong
Pl_System_Time(void)
{
  PlLong system_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct rusage rsr_usage;

  getrusage(RUSAGE_SELF, &rsr_usage);

  system_time = (rsr_usage.ru_stime.tv_sec * 1000) +
    (rsr_usage.ru_stime.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)
  FILETIME creat_t, exit_t, kernel_t, user_t;

  /* Success on Windows NT */
  if (GetProcessTimes(GetCurrentProcess(),
                      &creat_t, &exit_t, &kernel_t, &user_t))
    system_time = (PlLong) (((__int64) kernel_t.dwHighDateTime << 32) +
                          (__int64) kernel_t.dwLowDateTime) / 10000;
  else                          /* not implemented on Windows 95/98 */
    system_time = 0;

#else

  Pl_Fatal_Error("system time not available");
  return 0;

#endif

  return system_time - start_system_time;
}




/*-------------------------------------------------------------------------*
 * PL_REAL_TIME                                                            *
 *                                                                         *
 * returns the real time used since the start of the process (in ms).      *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Real_Time(void)
{
  PlLong real_time;

#if defined(__unix__) && !defined(__CYGWIN__)
  struct timeval tv;

  gettimeofday(&tv, NULL);
  real_time = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);

#elif defined(_WIN32) || defined(__CYGWIN__)

  real_time = (PlLong) ((double) clock() * 1000 / CLOCKS_PER_SEC);

#else

  Pl_Fatal_Error("real time not available");
  return 0;

#endif

  return real_time - start_real_time;
}




#ifndef RAND_MAX
#define RAND_MAX  (((unsigned) -1) >> 1)
#endif




/*-------------------------------------------------------------------------*
 * PL_RANDOMIZE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Randomize(void)
{
  static unsigned int count = 0;
  unsigned int seed;
#if defined(_WIN32) || defined(__CYGWIN__)
  seed = GetTickCount();
#else
  struct timeval tv;

  gettimeofday(&tv, NULL);
  seed = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
#endif

  count++;
  seed ^= (unsigned int) getpid();
  seed ^= count * 0x9e3779b1U;	/* diffusion */

#if 1				/* some avalanche (MurmurHash3) */
  seed *= 0x85ebca6bU;
  seed ^= seed >> 13;
  seed *= 0xc2b2ae35U;
  seed ^= seed >> 16;
#endif
  
  seed %= INT_GREATEST_VALUE;
  
  Pl_Set_Seed(seed);
}




/*-------------------------------------------------------------------------*
 * PL_SET_SEED                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Seed(unsigned int n)
{
  cur_seed = n;
  srand(cur_seed);
}




/*-------------------------------------------------------------------------*
 * PL_GET_SEED                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned int
Pl_Get_Seed(void)
{
  return cur_seed;
}




/*-------------------------------------------------------------------------*
 * PL_RANDOM_INTEGER                                                       *
 *                                                                         *
 * return an integer x s.t. 0 <= x < n                                     *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Random_Integer(PlLong n)
{
  return (PlLong) ((double) n * rand() / (RAND_MAX + 1.0));
}




/*-------------------------------------------------------------------------*
 * PL_RANDOM_FLOAT                                                         *
 *                                                                         *
 * return a double x s.t. 0 <= x < n                                       *
 *-------------------------------------------------------------------------*/
double
Pl_Random_Float(double n)
{
  return n * rand() / (RAND_MAX + 1.0);
}




/*-------------------------------------------------------------------------*
 * PL_HOST_NAME_FROM_NAME                                                  *
 *                                                                         *
 * if host_name == NULL use current host name.                             *
 *-------------------------------------------------------------------------*/
char *
Pl_Host_Name_From_Name(char *host_name)
{
  static char buff[4096];

#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
#endif

  if (host_name == NULL)
    {
      PlLong length = sizeof(buff);
      host_name = buff;
#if defined(_WIN32) && !defined(__CYGWIN__) && defined(NO_USE_SOCKETS)
      if (GetComputerName(buff, &length) == 0)
#else
	if (gethostname(buff, (int) length))
#endif
        {
          strcpy(buff, "unknown host name");
          goto finish;
        }
    }

  if (strchr(host_name, '.') != NULL)   /* qualified name */
    goto finish;

#ifdef INET_MANAGEMENT

  host_entry = gethostbyname(host_name);        /* use name server */
  if (host_entry == NULL)
    goto finish;

  host_name = Host_Name_From_Alias(host_entry);

#endif	/* INET_MANAGEMENT */

finish:
  return host_name;
}




/*-------------------------------------------------------------------------*
 * PL_HOST_NAME_FROM_ADR                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Host_Name_From_Adr(char *host_address)
{
#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
  struct in_addr iadr;

#if defined(M_sparc32_sunos) || defined(M_sparc32_solaris) || defined(M_ix86_solaris) || \
    defined(_WIN32) || defined(__CYGWIN__)
  if ((iadr.s_addr = inet_addr(host_address)) == (uint32_t) (-1))
#else
  if (inet_aton(host_address, &iadr) == 0)
#endif
    return FALSE;

  host_entry = gethostbyaddr((char *) &iadr, sizeof(iadr), AF_INET);
  if (host_entry == NULL)
    return NULL;

  return Host_Name_From_Alias(host_entry);
#else   /* !INET_MANAGEMENT */
  return NULL;
#endif	/* !INET_MANAGEMENT */
}




#ifdef INET_MANAGEMENT

/*-------------------------------------------------------------------------*
 * HOST_NAME_FROM_ALIAS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Host_Name_From_Alias(struct hostent *host_entry)
{
  char *name;
  char **alias;
  char *p;

  name = (char *) host_entry->h_name;
  alias = host_entry->h_aliases;
  while ((p = strchr(name, '.')) == NULL && *alias)
    name = *alias++;

  if (p)
    return name;

  return (char *) host_entry->h_name;
}

#endif	/* INET_MANAGEMENT */
