/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.c                                                       *
 * Descr.: machine dependent features                                      *
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

#include "gp_config.h"          /* ensure __unix__ defined if not Win32 */

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>            /* warning: windows.h defines _WIN32 */
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

#define M_MAGIC1                   0x12345678
#define M_MAGIC2                   0xdeadbeef

#define UNKNOWN_SYS_ERRNO          "Unknown error (%d)"




          /* Error Messages */

#define ERR_STACKS_ALLOCATION      "Memory allocation fault"

#define ERR_CANNOT_OPEN_DEV0       "Cannot open /dev/zero : %s"
#define ERR_CANNOT_UNMAP           "unmap failed : %s"

#define ERR_CANNOT_FREE            "VirtualFree failed : %" PL_FMT_u
#define ERR_CANNOT_PROTECT         "VirtualProtect failed : %" PL_FMT_u

#define ERR_CANNOT_EXEC_GETCWD     "cannot execute getcwd"


#define ERR_STACK_OVERFLOW_ENV     "%s stack overflow (size: %d Kb, reached: %d Kb, environment variable used: %s)"

#define ERR_STACK_OVERFLOW_NO_ENV  "%s stack overflow (size: %d Kb, reached: %d Kb - fixed size)"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static PlLong start_user_time = 0;
static PlLong start_system_time = 0;
static PlLong start_real_time = 0;

static int cur_seed = 1;




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

  start_user_time = Pl_M_User_Time();
  start_system_time = Pl_M_System_Time();
  start_real_time = Pl_M_Real_Time();

#if defined(HAVE_MALLOPT) && defined(M_MMAP_MAX)
  mallopt(M_MMAP_MAX, 0);
#endif

  Pl_Init_Machine1();
}




/*-------------------------------------------------------------------------*
 * PL_M_SYS_ERR_STRING                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Sys_Err_String(int ret_val)
{
#ifdef M_sparc_sunos
  extern char *sys_errlist[];
  extern int sys_nerr;
#endif

  char *str;
  static char buff[64];

#if defined(_WIN32) || defined(__CYGWIN__)
  if (ret_val == M_ERROR_WIN32)
    {
      int status =  GetLastError();

      if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, status, 0, 
			buff, sizeof(buff), NULL) == 0)
	sprintf(buff, "Windows " UNKNOWN_SYS_ERRNO, status);
      else
	{			/* windows adds a ".\r\n" at end - remove it */
	  char *p = buff + strlen(buff);
	  while (--p > buff && (isspace(*p) || *p == '.'))
	    {
	    }
	  p[1] = '\0';
	}

      return buff;
    }
#endif


#if defined(M_sparc_sunos)
  str = (errno >= 0 && errno < sys_nerr) ? sys_errlist[errno] : NULL;
#else
  str = strerror(errno);
#endif

  if (str)
    return str;

  sprintf(buff, UNKNOWN_SYS_ERRNO, errno);
  return buff;
}




/*-------------------------------------------------------------------------*/
/* M_USER_TIME                                                             */
/*                                                                         */
/* returns the user time used since the start of the process (in ms).      */
/*-------------------------------------------------------------------------*/
PlLong
Pl_M_User_Time(void)
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
 * PL_M_SYSTEM_TIME                                                        *
 *                                                                         *
 * returns the system time used since the start of the process (in ms).    *
 *-------------------------------------------------------------------------*/
PlLong
Pl_M_System_Time(void)
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
 * PL_M_REAL_TIME                                                          *
 *                                                                         *
 * returns the real time used since the start of the process (in ms).      *
 *-------------------------------------------------------------------------*/
PlLong
Pl_M_Real_Time(void)
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
 * PL_M_RANDOMIZE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_M_Randomize(void)
{
  static int count = 0;
#if defined(_WIN32) || defined(__CYGWIN__)
  int seed = GetTickCount();
#else
  struct timeval tv;
  int seed;

  gettimeofday(&tv, NULL);
  seed = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
#endif
  count = (count + rand()) % 0xFFFF;
  seed = seed ^ (getpid() << (seed & 0xFF));
  seed *= count;
  seed = seed & 0xFFFFFF;

  Pl_M_Set_Seed(seed);
}




/*-------------------------------------------------------------------------*
 * PL_M_SET_SEED                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_M_Set_Seed(int n)
{
  cur_seed = n;
  srand(cur_seed);
}




/*-------------------------------------------------------------------------*
 * PL_M_GET_SEED                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_M_Get_Seed(void)
{
  return cur_seed;
}




/*-------------------------------------------------------------------------*
 * PL_M_RANDOM_INTEGER                                                     *
 *                                                                         *
 * return an integer x s.t. 0 <= x < n                                     *
 *-------------------------------------------------------------------------*/
int
Pl_M_Random_Integer(int n)
{
  return (int) ((double) n * rand() / (RAND_MAX + 1.0));
}




/*-------------------------------------------------------------------------*
 * PL_M_RANDOM_FLOAT                                                       *
 *                                                                         *
 * return a double x s.t. 0 <= x < n                                       *
 *-------------------------------------------------------------------------*/
double
Pl_M_Random_Float(double n)
{
  return n * rand() / (RAND_MAX + 1.0);
}




/*-------------------------------------------------------------------------*
 * PL_M_HOST_NAME_FROM_NAME                                                *
 *                                                                         *
 * if host_name == NULL use current host name.                             *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Host_Name_From_Name(char *host_name)
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
      if (gethostname(buff, length))
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

#endif

finish:
  return host_name;
}




/*-------------------------------------------------------------------------*
 * PL_M_HOST_NAME_FROM_ADR                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Host_Name_From_Adr(char *host_address)
{
#ifdef INET_MANAGEMENT
  struct hostent *host_entry;
  struct in_addr iadr;

#if defined(M_sparc_sunos) || defined(M_sparc_solaris) || \
    defined(M_ix86_cygwin) || defined(M_ix86_solaris)  || \
    defined(_WIN32)
  if ((iadr.s_addr = inet_addr(host_address)) == -1)
#else
  if (inet_aton(host_address, &iadr) == 0)
#endif
    return FALSE;

  host_entry = gethostbyaddr((char *) &iadr, sizeof(iadr), AF_INET);
  if (host_entry == NULL)
    return NULL;

  return Host_Name_From_Alias(host_entry);
#else
  return NULL;
#endif
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

#endif



/*-------------------------------------------------------------------------*
 * PL_M_SET_WORKING_DIR                                                    *
 *                                                                         *
 * must preserve errno if fails (used in os_interf_c.c)                    *
 *-------------------------------------------------------------------------*/
Bool
Pl_M_Set_Working_Dir(char *path)
{
  char *new_path = Pl_M_Absolute_Path_Name(path);

  return (new_path != NULL && chdir(new_path) == 0);
}




/*-------------------------------------------------------------------------*
 * PL_M_GET_WORKING_DIR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Get_Working_Dir(void)
{
  static char cur_work_dir[MAXPATHLEN];

  if (getcwd(cur_work_dir, sizeof(cur_work_dir) - 1) == NULL)
    strcpy(cur_work_dir, ".");
  return cur_work_dir;
}




/*-------------------------------------------------------------------------*
 * PL_M_ABSOLUTE_PATH_NAME                                                 *
 *                                                                         *
 * returns an absolute file name.                                          *
 *-------------------------------------------------------------------------*/
char *
Pl_M_Absolute_Path_Name(char *src)
{
  static char buff[2][MAXPATHLEN];
  int res = 0;
  char *dst;
  char *p, *q;
  char c;

  dst = buff[res];
  while ((*dst++ = *src))       /* expand $VARNAME and %VARNAME% (Win32) */
    {
      c = *src++;
      if (c == '$'
#if defined(_WIN32) || defined(__CYGWIN__)
          || c == '%'
#endif
        )
        {
          p = dst;
          while (isalnum(*src) || *src == '_')
            *dst++ = *src++;
#if defined(_WIN32) || defined(__CYGWIN__)
          if (c == '%' && *src != '%')
            continue;
#endif
          *dst = '\0';
          q = getenv(p);
          if (q)
            {
              p--;
              strcpy(p, q);
              dst = p + strlen(p);
#if defined(_WIN32) || defined(__CYGWIN__)
              if (c == '%')
                src++;
#endif
            }
#if defined(_WIN32) || defined(__CYGWIN__)
          else if (c == '%')
            *dst++ = *src++;
#endif
        }
    }
  *dst = '\0';

  if (buff[res][0] == '~')
    {
      if (Is_Dir_Sep(buff[res][1]) || buff[res][1] == '\0') /* ~/... cf $HOME */
        {
	  q = NULL;;
          if ((p = getenv("HOME")) == NULL)
	    {
#if defined(_WIN32) || defined(__CYGWIN__)
	      if ((p = getenv("HOMEPATH")) == NULL)
		return NULL;
	      q = getenv("HOMEDRIVE");
#else
	      return NULL;
#endif
	    }
	  if (q == NULL)
	    q = "";
          sprintf(buff[1 - res], "%s%s/%s", q, p, buff[res] + 1);
          res = 1 - res;
        }
#if defined(__unix__) || defined(__CYGWIN__)
      else                      /* ~user/... read passwd */
        {
          struct passwd *pw;

          p = buff[res] + 1;
          while (*p && !Is_Dir_Sep(*p))
            p++;

          buff[res][0] = *p;
          *p = '\0';
          if ((pw = getpwnam(buff[res] + 1)) == NULL)
            return NULL;

          *p = buff[res][0];

          sprintf(buff[1 - res], "%s/%s", pw->pw_dir, p);
          res = 1 - res;
        }
#endif
    }

  if (strcmp(buff[res], "user") == 0)   /* prolog special file 'user' */
    return buff[res];

#if defined(_WIN32) && !defined(__CYGWIN__)

  if (_fullpath(buff[1 - res], buff[res], MAXPATHLEN) == NULL)
    return NULL;

  res = 1 - res;
  for (dst = buff[res]; *dst; dst++)    /* \ becomes / */
    if (*dst == '\\')
      *dst = '/';

  /* dst points the \0 */

#else  /* __unix__ || __CYGWIN__ */

#if defined(__CYGWIN__)

  cygwin_conv_to_full_posix_path(buff[res], buff[1 - res]);
  res = 1 - res;

#endif

  if (buff[res][0] != '/')      /* add current directory */
    {
      sprintf(buff[1 - res], "%s/%s", Pl_M_Get_Working_Dir(), buff[res]);
      res = 1 - res;
    }

  src = buff[res];
  res = 1 - res;
  dst = buff[res];

  while ((*dst++ = *src))
    {
      if (*src++ != '/')
        continue;

    collapse:
      while (*src == '/')       /* collapse /////... as / */
        src++;

      if (*src != '.')
        continue;

      if (src[1] == '/' || src[1] == '\0')      /* /./ removed */
        {
          src++;
          goto collapse;
        }

      if (src[1] != '.' || (src[2] != '/' && src[2] != '\0'))
        continue;
      /* case /../ */
      src += 2;
      p = dst - 2;
      while (p >= buff[res] && *p != '/')
        p--;

      if (p < buff[res])
        return NULL;

      dst = p;
    }

  dst--;                        /* dst points the \0 */

#endif


#if defined(_WIN32) && !defined(__CYGWIN__)
#define MIN_PREFIX 3            /* win32 minimal path c:\  */
#else
#define MIN_PREFIX 1            /* unix  minimal path /    */
#endif

  if (dst - buff[res] > MIN_PREFIX && Is_Dir_Sep(dst[-1]))
    dst[-1] = '\0';             /* remove last / or \ */

  return buff[res];
}




/*-------------------------------------------------------------------------*
 * PL_M_IS_ABSOLUTE_FILE_NAME                                              *
 *                                                                         *
 * Test if a path name is absolute (i.e. not relative).                    *
 *-------------------------------------------------------------------------*/
Bool
Pl_M_Is_Absolute_File_Name(char *path)
{
  if (Is_Dir_Sep(*path))
    return TRUE;


  /* Windows: path strating with a drive specif is considered as absolute
   * (even if not followed by an antislash, e.g. c:foo is absolute).
   * Indeed, for a relative path, it is always 
   * possible to add before it the current working directory and it is not 
   * possible before a drive specif.
   * This is the behavior of Win32 PathIsRelative() function.
   * (to use it #include <shlwapi.h> and link with shlwapi.dll
   */
#if defined(_WIN32) || defined(__CYGWIN__)

  if (Has_Drive_Specif(path))
    return TRUE;

#endif

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_M_DECOMPOSE_FILE_NAME                                                *
 *                                                                         *
 * Decompose a path name into the dir, base and suffix (extension).        *
 *                                                                         *
 * path:   the path to decompose                                           *
 * del_trail_slashes: see below                                            *
 * base:   points the buffer which will receive the basename               *
 *         (or "" if none. It includes the suffix                          *
 * suffix: points inside base to the suffix part                           *
 *         (or "" if none, i.e. at the end of base)                        *
 * returns the dirname part (or "" if none)                                *
 *                                                                         *
 * Returned pointers are on 2 static buffers (dir and base) which can be   *
 * written.                                                                *
 *                                                                         *
 * del_trail_slashes: delete trailing slashes from dir ?                   *
 *  FALSE: nothing is done (path is simply split into dir and base).       *
 *         Concatenating dir and base yields the complete pathname.        *
 *  TRUE:  trailing slashes of the dir part are removed (similarly to      *
 *         dirname(3) except that initial trailing slashes of path are not *
 *         removed). If the dir part is empty then "." is returned.        *
 *         Concatenating dir "/" and base yields a complete pathname.      *
 *                                                                         *
 * To remove the extension from base simply do *suffix = '\0'              *
 * To add/change the suffix simply do strcpy(suffix, ".txt");              *
 *-------------------------------------------------------------------------*/

char *
Pl_M_Decompose_File_Name(char *path, Bool del_trail_slashes, char **base, char **suffix)
{
  static char buff_dir[MAXPATHLEN];
  static char buff_base[MAXPATHLEN];
  int dir_start_pos = 0;	/* on _WIN32 maybe there is a drive specif */

#if 0 && defined(_WIN32)	/* uncomment to explicitely use _splitpath() on Windows */

  char direct[_MAX_DIR];
  char ext[_MAX_EXT];
  
  _splitpath(path, buff_dir, direct, buff_base, ext); /* buff_dir contains the drive */
  dir_start_pos = strlen(buff_dir);
  strcat(buff_dir, direct);	/* concat the dirname */

  *suffix = buff_base + strlen(buff_base); /* buff_base contains the basename */
  strcpy(*suffix, ext);		     /* concat the suffix */
    
#else

  /* This version works for both Windows and Unix */

  char *p;

  strcpy(buff_dir, path);
#if defined(_WIN32) || defined(__CYGWIN__)
  if (Has_Drive_Specif(buff_dir))
    dir_start_pos = 2;
#endif

  Find_Last_Dir_Sep(p, buff_dir);

  p = (p == NULL) ? buff_dir + dir_start_pos : p + 1;

  strcpy(buff_base, p);
  *p = '\0';

  if ((p = strrchr(buff_base, '.')) != NULL)
    *suffix = p;      
  else
    *suffix = buff_base + strlen(buff_base); /* i.e. suffix = "" */

#endif

  if (del_trail_slashes)
    {
      if (buff_dir[dir_start_pos] == '\0') /* if dir is empty it becomes "." */
	strcat(buff_dir, ".");
      else
	{
	  int len = strlen(buff_dir);		/* remove all trailing / */
	  while(--len >= dir_start_pos && Is_Dir_Sep(buff_dir[len]))
	    ;

	  if (len < dir_start_pos)		/* if all are / keep one */
	    len = dir_start_pos;
	  buff_dir[len + 1] = '\0';
	}
    }

#if 0			 /* uncomment to avoid extension with only one '.' */
  if ((*suffix)[0] == '.' && (*suffix)[1] == '\0') /* not really a suffix: undo it */
    (*suffix)++;		     /* points the \0 */
#endif

  *base = buff_base;

  return buff_dir;
}
