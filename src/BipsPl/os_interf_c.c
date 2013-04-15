/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : os_interf_c.c                                                   *
 * Descr.: operating system interface management - C part                  *
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
#include <math.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "gp_config.h"

#ifdef _WIN32
#include <process.h>
#include <direct.h>
#include <io.h>
#include <winsock.h>
#include <fcntl.h>
#else
#define _XOPEN_SOURCE_EXTENDED	/* for alpha/OSF (usleep prototype) */
#include <dirent.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/wait.h>
#endif

#define OBJ_INIT Os_Interf_Initializer

#include "engine_pl.h"
#include "bips_pl.h"


#if 0
#define DEBUG
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_SIGNALS                255
#define MAX_SPAWN_ARGS             1024




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int atom;
  int sig;
}
InfSig;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_dt;

/* pl_atom_write is already defined in the set of often used atoms */
static int atom_execute;
static int atom_search;

static int atom_regular;
static int atom_directory;
static int atom_fifo;
static int atom_socket;
static int atom_character_device;
static int atom_block_device;
static int atom_unknown;

static InfSig tsig[MAX_SIGNALS];
static int nb_sig;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Flag_Of_Permission(WamWord perm_word, Bool is_a_directory);

static char *Get_Path_Name(WamWord path_name_word);

static Bool Date_Time_To_Prolog(time_t *t, WamWord date_time_word);

static int Select_Init_Set(WamWord list_word, fd_set *set, int check);

static Bool Select_Init_Ready_List(WamWord list_word, fd_set *set,
				   WamWord ready_list_word);




/*-------------------------------------------------------------------------*
 * OS_INTERF_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Os_Interf_Initializer(void)
{
  atom_dt = Pl_Create_Atom("dt");

  atom_execute = Pl_Create_Atom("execute");
  atom_search = Pl_Create_Atom("search");

  atom_regular = Pl_Create_Atom("regular");
  atom_directory = Pl_Create_Atom("directory");
  atom_fifo = Pl_Create_Atom("fifo");
  atom_socket = Pl_Create_Atom("socket");
  atom_character_device = Pl_Create_Atom("character_device");
  atom_block_device = Pl_Create_Atom("block_device");
  atom_unknown = Pl_Create_Atom("unknown");

  nb_sig = 0;
#if defined(__unix__) || defined(__CYGWIN__)
  tsig[nb_sig].atom = Pl_Create_Atom("SIGHUP");
  tsig[nb_sig++].sig = SIGHUP;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGINT");
  tsig[nb_sig++].sig = SIGINT;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGQUIT");
  tsig[nb_sig++].sig = SIGQUIT;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGILL");
  tsig[nb_sig++].sig = SIGILL;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGTRAP");
  tsig[nb_sig++].sig = SIGTRAP;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGABRT");
  tsig[nb_sig++].sig = SIGABRT;
#ifndef M_ix86_cygwin
  tsig[nb_sig].atom = Pl_Create_Atom("SIGIOT");
  tsig[nb_sig++].sig = SIGIOT;
#endif
  tsig[nb_sig].atom = Pl_Create_Atom("SIGBUS");
  tsig[nb_sig++].sig = SIGBUS;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGFPE");
  tsig[nb_sig++].sig = SIGFPE;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGKILL");
  tsig[nb_sig++].sig = SIGKILL;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGUSR1");
  tsig[nb_sig++].sig = SIGUSR1;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGSEGV");
  tsig[nb_sig++].sig = SIGSEGV;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGUSR2");
  tsig[nb_sig++].sig = SIGUSR2;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGPIPE");
  tsig[nb_sig++].sig = SIGPIPE;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGALRM");
  tsig[nb_sig++].sig = SIGALRM;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGTERM");
  tsig[nb_sig++].sig = SIGTERM;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGCHLD");
  tsig[nb_sig++].sig = SIGCHLD;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGCONT");
  tsig[nb_sig++].sig = SIGCONT;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGSTOP");
  tsig[nb_sig++].sig = SIGSTOP;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGTSTP");
  tsig[nb_sig++].sig = SIGTSTP;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGTTIN");
  tsig[nb_sig++].sig = SIGTTIN;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGTTOU");
  tsig[nb_sig++].sig = SIGTTOU;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGURG");
  tsig[nb_sig++].sig = SIGURG;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGXCPU");
  tsig[nb_sig++].sig = SIGXCPU;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGXFSZ");
  tsig[nb_sig++].sig = SIGXFSZ;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGVTALRM");
  tsig[nb_sig++].sig = SIGVTALRM;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGPROF");
  tsig[nb_sig++].sig = SIGPROF;
  tsig[nb_sig].atom = Pl_Create_Atom("SIGWINCH");
  tsig[nb_sig++].sig = SIGWINCH;
#ifndef M_ix86_sco
  tsig[nb_sig].atom = Pl_Create_Atom("SIGIO");
  tsig[nb_sig++].sig = SIGIO;
#endif
#if !defined(M_bsd) && !defined(M_darwin)
  tsig[nb_sig].atom = Pl_Create_Atom("SIGPOLL");
  tsig[nb_sig++].sig = SIGPOLL;
#endif
#endif


#if defined(__unix__) || defined(__CYGWIN__)
  signal(SIGPIPE, SIG_IGN);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_MAKE_DIRECTORY_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Make_Directory_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

#ifdef _WIN32
  Os_Test_Error(_mkdir(path_name));
#else
  Os_Test_Error(mkdir(path_name, 0777));
#endif

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_DIRECTORY_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Delete_Directory_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(rmdir(path_name));

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_WORKING_DIRECTORY_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Working_Directory_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Pl_M_Get_Working_Dir();

  return Pl_Un_String_Check(path_name, path_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_CHANGE_DIRECTORY_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Change_Directory_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  errno = -1;
  if (!Pl_M_Set_Working_Dir(path_name))
    Os_Test_Error(-1);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_DIRECTORY_FILES_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Directory_Files_2(WamWord path_name_word, WamWord list_word)
{
  char *path_name;
  Bool res;
  char *name;

#ifdef _WIN32
  PlLong h;
  struct _finddata_t d;
  static char buff[MAXPATHLEN];
#else
  DIR *dir;
  struct dirent *cur_entry;
#endif


  Pl_Check_For_Un_List(list_word);

  path_name = Get_Path_Name(path_name_word);

#ifdef _WIN32
  sprintf(buff, "%s\\*.*", path_name);
  h = _findfirst(buff, &d);	/* instead of Win32 FindFirstFile since uses errno */
  Os_Test_Error(h);
#else
  dir = opendir(path_name);
  Os_Test_Error_Null(dir);
#endif

#ifdef _WIN32
  do
    {
      name = d.name;
#else
  while ((cur_entry = readdir(dir)) != NULL)
    {
      name = cur_entry->d_name;
#endif
      if (!Pl_Get_List(list_word) || !Pl_Unify_Atom(Pl_Create_Allocate_Atom(name)))
	{
	  res = FALSE;
	  goto finish;
	}

      list_word = Pl_Unify_Variable();
    }
#ifdef _WIN32
  while (_findnext(h, &d) == 0);
#endif

  res = Pl_Get_Nil(list_word);

finish:
#ifdef _WIN32
  _findclose(h);
#else
  closedir(dir);
#endif

  return res;
}




/*-------------------------------------------------------------------------*
 * PL_RENAME_FILE_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Rename_File_2(WamWord path_name1_word, WamWord path_name2_word)
{
  char path_name1[MAXPATHLEN];
  char *path_name2;

  strcpy(path_name1, Get_Path_Name(path_name1_word));
  path_name2 = Get_Path_Name(path_name2_word);

  Os_Test_Error(rename(path_name1, path_name2));

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UNLINK_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Unlink_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  unlink(path_name);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_FILE_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Delete_File_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(unlink(path_name));

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FILE_EXISTS_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Exists_1(WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  if (access(path_name, F_OK))
    {
      if (errno == ENOENT || errno == ENOTDIR)
	return FALSE;

      Os_Test_Error(-1);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PERMISSION_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Permission_2(WamWord path_name_word, WamWord perm_list_word)
{
  WamWord word, tag_mask;
  WamWord save_perm_list_word;
  WamWord *lst_adr;
  char *path_name;
  int mode, perm = 0;
  struct stat file_info;
  int res;
  Bool is_a_directory;

  path_name = Get_Path_Name(path_name_word);

  res = stat(path_name, &file_info);
  if (res == -1 && errno != ENOENT && errno != ENOTDIR)
    Os_Test_Error(-1);

  mode = file_info.st_mode;

  is_a_directory = (res == 0) && S_ISDIR(mode);

  DEREF(perm_list_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK && word != NIL_WORD)
    perm |= Flag_Of_Permission(word, is_a_directory);
  else
    {
      save_perm_list_word = perm_list_word;

      for (;;)
	{
	  DEREF(perm_list_word, word, tag_mask);

	  if (tag_mask == TAG_REF_MASK)
	    Pl_Err_Instantiation();

	  if (word == NIL_WORD)
	    break;

	  if (tag_mask != TAG_LST_MASK)
	    Pl_Err_Type(pl_type_list, save_perm_list_word);

	  lst_adr = UnTag_LST(word);
	  perm |= Flag_Of_Permission(Car(lst_adr), is_a_directory);

	  perm_list_word = Cdr(lst_adr);
	}
    }

  return (res == 0) && perm > 0 && ((mode | perm) == mode);
}




/*-------------------------------------------------------------------------*
 * FLAG_OF_PERMISSION                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Flag_Of_Permission(WamWord perm_word, Bool is_a_directory)
{
  int atom;

  atom = Pl_Rd_Atom_Check(perm_word);

  if (atom == pl_atom_read)
    return S_IRUSR;

  if (atom == pl_atom_write)
    return S_IWUSR;

  if (atom == atom_execute)
    return (is_a_directory) ? -1 : S_IXUSR;

  if (atom == atom_search)
    return (is_a_directory) ? S_IXUSR : -1;

  Pl_Err_Domain(pl_domain_os_file_permission, perm_word);
  return 0;			/* anything for the compiler */
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PROP_ABSOLUTE_FILE_NAME_2                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Prop_Absolute_File_Name_2(WamWord absolute_path_name_word,
			       WamWord path_name_word)
{
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(access(path_name, F_OK));	/* test if file exists */

  return Pl_Un_String_Check(path_name, absolute_path_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PROP_REAL_FILE_NAME_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Prop_Real_File_Name_2(WamWord real_path_name_word,
			   WamWord path_name_word)
{
  char *path_name = Get_Path_Name(path_name_word);

#ifndef _WIN32
  char real_path_name[MAXPATHLEN];

  Os_Test_Error_Null(realpath(path_name, real_path_name));
#else
  char *real_path_name = path_name;
#endif

  return Pl_Un_String_Check(real_path_name, real_path_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PROP_TYPE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Prop_Type_2(WamWord type_word, WamWord path_name_word)
{
  char *path_name;
  struct stat file_info;
  int atom;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(stat(path_name, &file_info));

  if (S_ISREG(file_info.st_mode))
    atom = atom_regular;
  else if (S_ISDIR(file_info.st_mode))
    atom = atom_directory;
#ifdef S_ISFIFO
  else if (S_ISFIFO(file_info.st_mode))
    atom = atom_fifo;
#endif
#ifdef S_ISSOCK
  else if (S_ISSOCK(file_info.st_mode))
    atom = atom_socket;
#endif
#ifdef S_ISCHR
  else if (S_ISCHR(file_info.st_mode))
    atom = atom_character_device;
#endif
#ifdef S_ISBLK
  else if (S_ISBLK(file_info.st_mode))
    atom = atom_block_device;
#endif
  else
    atom = atom_unknown;

  return Pl_Un_Atom_Check(atom, type_word);
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PROP_SIZE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Prop_Size_2(WamWord size_word, WamWord path_name_word)
{
  char *path_name;
  struct stat file_info;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(stat(path_name, &file_info));

  return Pl_Un_Positive_Check((int) file_info.st_size, size_word);
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_PROP_PERM_AND_FILE_2                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Check_Prop_Perm_And_File_2(WamWord perm_word, WamWord path_name_word)
{
  WamWord word, tag_mask;
  char *path_name;

  path_name = Get_Path_Name(path_name_word);

  DEREF(perm_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    Flag_Of_Permission(perm_word, FALSE);	/* to check perm validity */

  Os_Test_Error(access(path_name, F_OK));	/* to check file existence */

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FILE_PROP_DATE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_File_Prop_Date_2(WamWord date_time_word, WamWord path_name_word)
{
  char *path_name;
  struct stat file_info;
  time_t *t;

  path_name = Get_Path_Name(path_name_word);

  Os_Test_Error(stat(path_name, &file_info));

  switch (pl_sys_var[0])
    {
    case 0:
      t = &(file_info.st_ctime);
      break;

    case 1:
      t = &(file_info.st_atime);
      break;

    default:
      t = &(file_info.st_mtime);
      break;
    }

  return Date_Time_To_Prolog(t, date_time_word);
}




/*-------------------------------------------------------------------------*
 * PL_TEMPORARY_NAME_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Temporary_Name_2(WamWord template_word, WamWord path_name_word)
{
  char *template;
  char *path_name;

  template = Get_Path_Name(template_word);

  path_name = Pl_M_Mktemp(template);
  Os_Test_Error_Null(path_name);

  return path_name && Pl_Un_String_Check(path_name, path_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_TEMPORARY_FILE_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Temporary_File_3(WamWord dir_word, WamWord prefix_word,
		 WamWord path_name_word)
{
  char *dir;
  char *prefix;
  char *path_name;

  dir = Pl_Rd_String_Check(dir_word);
  if (*dir == '\0')
    dir = NULL;
  else
    dir = Get_Path_Name(dir_word);

  prefix = Pl_Rd_String_Check(prefix_word);
  if (*prefix == '\0')
    prefix = NULL;

  path_name = Pl_M_Tempnam(dir, prefix);
  Os_Test_Error_Null(path_name);

  return path_name && Pl_Un_String_Check(path_name, path_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_DATE_TIME_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Date_Time_1(WamWord date_time_word)
{
  time_t t;

  t = time(NULL);

  return Date_Time_To_Prolog(&t, date_time_word);
}




/*-------------------------------------------------------------------------*
 * PL_HOST_NAME_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Host_Name_1(WamWord host_name_word)
{
  WamWord word, tag_mask;
  int atom;
  static int atom_host_name = -1;	/* not created in an init since */
				        /* establishes a connection */
				        /* (ifndef NO_USE_SOCKETS) */

  if (atom_host_name < 0)
    atom_host_name = Pl_Create_Allocate_Atom(Pl_M_Host_Name_From_Name(NULL));

  DEREF(host_name_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    return Pl_Get_Atom(atom_host_name, host_name_word);

  atom = Pl_Rd_Atom_Check(word);

  return atom == atom_host_name ||
    strcmp(Pl_M_Host_Name_From_Name(pl_atom_tbl[atom].name),
	   pl_atom_tbl[atom_host_name].name) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_OS_VERSION_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Os_Version_1(WamWord os_version_word)
{
  return Pl_Un_String_Check(pl_m_os_version, os_version_word);
}




/*-------------------------------------------------------------------------*
 * PL_ARCHITECTURE_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Architecture_1(WamWord architecture_word)
{
  return Pl_Un_String_Check(pl_m_architecture, architecture_word);
}




/*-------------------------------------------------------------------------*
 * PL_SLEEP_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sleep_1(WamWord seconds_word)
{
#ifdef _WIN32
  DWORD ms;

  ms = (DWORD) (Pl_Rd_Number_Check(seconds_word) * 1000);

  if (ms < 0)
    Pl_Err_Domain(pl_domain_not_less_than_zero, seconds_word);

  Sleep(ms);
#else
  PlLong us;

  us = (PlLong) (Pl_Rd_Number_Check(seconds_word) * 1000000);

  if (us < 0)
    Pl_Err_Domain(pl_domain_not_less_than_zero, seconds_word);

  usleep(us);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_SHELL_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Shell_2(WamWord cmd_word, WamWord status_word)
{
  char *cmd;
  int status;

  cmd = Pl_Rd_String_Check(cmd_word);
  if (*cmd == '\0')
    cmd = NULL;
  Pl_Check_For_Un_Integer(status_word);

  Pl_Flush_All_Streams();
  status = Pl_M_Shell(cmd);

  return Pl_Get_Integer(status, status_word);
}




/*-------------------------------------------------------------------------*
 * PL_SYSTEM_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_System_2(WamWord cmd_word, WamWord status_word)
{
  char *cmd;
  int status;

  cmd = Pl_Rd_String_Check(cmd_word);
  Pl_Check_For_Un_Integer(status_word);

#ifdef _WIN32
  _flushall();
#endif

  Pl_Flush_All_Streams();
  status = system(cmd);

  return Pl_Get_Integer(status, status_word);
}




/*-------------------------------------------------------------------------*
 * PL_SPAWN_3                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Spawn_3(WamWord cmd_word, WamWord list_word, WamWord status_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  char *arg[MAX_SPAWN_ARGS];
  char **p = arg;
  char err[64];
  int status;

  save_list_word = list_word;

  *p++ = Pl_Rd_String_Check(cmd_word);

  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);

      *p++ = Pl_Rd_String_Check(Car(lst_adr));

      list_word = Cdr(lst_adr);
    }

  *p = NULL;
  Pl_Check_For_Un_Integer(status_word);

  Pl_Flush_All_Streams();
  status = Pl_M_Spawn(arg);

  if (status == -1)
    Os_Test_Error(status);
  else if (status == -2)
    {
      sprintf(err, "error trying to execute %s", arg[0]);
      Pl_Err_System(Pl_Create_Allocate_Atom(err));
      return FALSE;
    }

  return Pl_Get_Integer(status, status_word);
}




/*-------------------------------------------------------------------------*
 * PL_POPEN_3                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Popen_3(WamWord cmd_word, WamWord mode_word, WamWord stm_word)
{
  char *cmd;
  int atom;
  int mode;
  int stm;
  FILE *f;
  char open_str[10];

  cmd = Pl_Rd_String_Check(cmd_word);

  atom = Pl_Rd_Atom_Check(mode_word);
  if (atom == pl_atom_read)
    {
      mode = STREAM_MODE_READ;
      strcpy(open_str, "r");
    }
  else if (atom == pl_atom_write)
    {
      mode = STREAM_MODE_WRITE;
      strcpy(open_str, "w");
    }
  else
    Pl_Err_Domain(pl_domain_io_mode, mode_word);


  Pl_Flush_All_Streams();
  f = popen(cmd, open_str);
  Os_Test_Error_Null(f);

  sprintf(pl_glob_buff, "popen_stream('%.1024s')", cmd);
  atom = Pl_Create_Allocate_Atom(pl_glob_buff);
  stm = Pl_Add_Stream_For_Stdio_Desc(f, atom, mode, TRUE);
  pl_stm_tbl[stm]->fct_close = (StmFct) pclose;

  return Pl_Get_Integer(stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_EXEC_5                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Exec_5(WamWord cmd_word, WamWord stm_in_word, WamWord stm_out_word,
	  WamWord stm_err_word, WamWord pid_word)
{
  char *cmd;
  char **arg;
  int stm;
  FILE *f_in, *f_out, *f_err;
  int pid;
  int mask = SYS_VAR_OPTION_MASK;
  int atom;
  char err[1024];

  cmd = Pl_Rd_String_Check(cmd_word);
  arg = Pl_M_Create_Shell_Command(cmd);

  Pl_Flush_All_Streams();
  pid = Pl_M_Spawn_Redirect(arg, (mask & 1) == 0, &f_in, &f_out, &f_err);

  /* If the command is not found we get ENOENT under Windows. 
   * Under Unix the information is only obtained at Pl_M_Get_Status(). */

  if (pid == -1 && errno != ENOENT)
    Os_Test_Error(pid); /* ENOENT is for Windows */
  if (pid < 0)
    {
      sprintf(err, "error trying to execute %s (maybe not found)", cmd);
      Pl_Err_System(Pl_Create_Allocate_Atom(err));
      return FALSE;
    }

  if (mask & 1)			/* pid needed ? */
    Pl_Get_Integer(pid, pid_word);

  sprintf(pl_glob_buff, "exec_stream('%.1024s')", cmd);
  atom = Pl_Create_Allocate_Atom(pl_glob_buff);

  stm = Pl_Add_Stream_For_Stdio_Desc(f_in, atom, STREAM_MODE_WRITE, TRUE);
  Pl_Get_Integer(stm, stm_in_word);
#ifdef DEBUG
  DBGPRINTF("Added Stream Input: %d\n", stm);
#endif

  stm = Pl_Add_Stream_For_Stdio_Desc(f_out, atom, STREAM_MODE_READ, TRUE);
  pl_stm_tbl[stm]->prop.eof_action = STREAM_EOF_ACTION_RESET;
  Pl_Get_Integer(stm, stm_out_word);

#ifdef DEBUG
  DBGPRINTF("Added Stream Output: %d\n", stm);
#endif

  stm = Pl_Add_Stream_For_Stdio_Desc(f_err, atom, STREAM_MODE_READ, TRUE);
  pl_stm_tbl[stm]->prop.eof_action = STREAM_EOF_ACTION_RESET;
  Pl_Get_Integer(stm, stm_err_word);
#ifdef DEBUG
  DBGPRINTF("Added Stream Error: %d\n", stm);
#endif

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_PIPE_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Create_Pipe_2(WamWord stm_in_word, WamWord stm_out_word)
{
  int p[2];
  int stm;
  FILE *f_in, *f_out;
  int atom;

#ifdef _WIN32
  Os_Test_Error(_pipe(p, 4096, O_TEXT));
#else
  Os_Test_Error(pipe(p));
#endif

  Os_Test_Error_Null((f_in = fdopen(p[0], "rt")));
  sprintf(pl_glob_buff, "pipe_stream_in");
  atom = Pl_Create_Allocate_Atom(pl_glob_buff);
  stm = Pl_Add_Stream_For_Stdio_Desc(f_in, atom, STREAM_MODE_READ, TRUE);
  pl_stm_tbl[stm]->prop.eof_action = STREAM_EOF_ACTION_RESET;
  Pl_Get_Integer(stm, stm_in_word);

  Os_Test_Error_Null((f_out = fdopen(p[1], "wt")));
  sprintf(pl_glob_buff, "pipe_stream_out");
  atom = Pl_Create_Allocate_Atom(pl_glob_buff);
  stm = Pl_Add_Stream_For_Stdio_Desc(f_out, atom, STREAM_MODE_WRITE, TRUE);
  Pl_Get_Integer(stm, stm_out_word);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FORK_PROLOG_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fork_Prolog_1(WamWord pid_word)

{
#ifdef _WIN32

  Pl_Err_Resource(Pl_Create_Atom("not implemented"));
  return FALSE;

#else

  int pid;

  pid = fork();
  Os_Test_Error(pid);

  return Pl_Get_Integer(pid, pid_word);

#endif
}




/*-------------------------------------------------------------------------*
 * PL_SELECT_5                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Select_5(WamWord reads_word, WamWord ready_reads_word,
	 WamWord writes_word, WamWord ready_writes_word,
	 WamWord time_out_word)
{
#if defined(_WIN32) && defined(NO_USE_SOCKETS)

  Pl_Err_Resource(Pl_Create_Atom("not implemented"));
  return FALSE;

#else

  double time_out;
  struct timeval *p, t;
  fd_set read_set, write_set;
  int max, n;

  max = Select_Init_Set(reads_word, &read_set, STREAM_CHECK_INPUT);
  Pl_Check_For_Un_List(ready_reads_word);
  n = Select_Init_Set(writes_word, &write_set, STREAM_CHECK_OUTPUT);
  if (n > max)
    max = n;

  Pl_Check_For_Un_List(ready_writes_word);


  time_out = Pl_Rd_Number_Check(time_out_word);
  if (time_out <= 0)
    p = NULL;
  else
    {
      t.tv_sec = (PlLong) (time_out / 1000);
      t.tv_usec = (PlLong) (fmod(time_out, 1000) * 1000);
      p = &t;
    }


  Os_Test_Error(select(max + 1, &read_set, &write_set, NULL, p));


  return Select_Init_Ready_List(reads_word, &read_set, ready_reads_word) &&
    Select_Init_Ready_List(writes_word, &write_set, ready_writes_word);

#endif
}




/*-------------------------------------------------------------------------*
 * SELECT_INIT_SET                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Select_Init_Set(WamWord list_word, fd_set *set, int check)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  int stm;
  int fd, max = 0;

  FD_ZERO(set);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);
      if (tag_mask == TAG_INT_MASK)
	fd = Pl_Rd_Positive_Check(word);
      else
	{
	  stm = Pl_Get_Stream_Or_Alias(word, check);

	  fd = Pl_Io_Fileno_Of_Stream(stm);
	  if (fd < 0)
	    Pl_Err_Domain(pl_domain_selectable_item, word);
	}

#ifdef FD_SETSIZE
      if (fd >= FD_SETSIZE)
	{
	  errno = EBADF;
	  Os_Test_Error(-1);
	}
#endif

      FD_SET(fd, set);
      if (fd > max)
	max = fd;
      list_word = Cdr(lst_adr);
    }

  return max;
}




/*-------------------------------------------------------------------------*
 * SELECT_INIT_READY_LIST                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Select_Init_Ready_List(WamWord list_word, fd_set *set,
		       WamWord ready_list_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int stm;
  int fd;

  for (;;)
    {
      DEREF(list_word, word, tag_mask);
      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);

      if (tag_mask == TAG_INT_MASK)
	fd = UnTag_INT(word);
      else
	{
	  stm = Pl_Get_Stream_Or_Alias(word, STREAM_CHECK_VALID);
	  fd = (stm < 0) ? -1 : Pl_Io_Fileno_Of_Stream(stm);
	}

      if (FD_ISSET(fd, set))
	{
	  if (!Pl_Get_List(ready_list_word) || !Pl_Unify_Value(word))
	    return FALSE;

	  ready_list_word = Pl_Unify_Variable();
	}

      list_word = Cdr(lst_adr);
    }

  return Pl_Get_Nil(ready_list_word);
}




/*-------------------------------------------------------------------------*
 * PL_PROLOG_PID_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Prolog_Pid_1(WamWord prolog_pid_word)
{
  int prolog_pid;

  prolog_pid = (int) getpid();

  return Pl_Un_Integer_Check(prolog_pid, prolog_pid_word);
}




/*-------------------------------------------------------------------------*
 * PL_SEND_SIGNAL_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Send_Signal_2(WamWord pid_word, WamWord signal_word)
{
  WamWord word, tag_mask;
  int pid;
  int sig;
  int atom;
  int i;

  pid = Pl_Rd_Integer_Check(pid_word);

  DEREF(signal_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)
    {
      atom = UnTag_ATM(word);
      sig = -1;
      for (i = 0; i < nb_sig; i++)
	if (tsig[i].atom == atom)
	  {
	    sig = tsig[i].sig;
	    break;
	  }
    }
  else
    sig = Pl_Rd_Integer_Check(word);

#ifdef _WIN32
  {
    int ret;

    if (pid != _getpid())
      {
	errno = EINVAL;
	ret = -1;
      }
    else
      {
	errno = 0;
	ret = raise(sig);
      }

    Os_Test_Error(ret);
  }
#else
  Os_Test_Error(kill(pid, sig));
#endif

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_WAIT_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Wait_2(WamWord pid_word, WamWord status_word)
{
  int pid;
  int status;

  pid = Pl_Rd_Integer_Check(pid_word);
  Pl_Check_For_Un_Integer(status_word);

  status  = Pl_M_Get_Status(pid);
  Os_Test_Error(status);

  return Pl_Get_Integer(status, status_word);
}




/*-------------------------------------------------------------------------*
 * GET_PATH_NAME                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Get_Path_Name(WamWord path_name_word)
{
  char *path_name;

  path_name = Pl_Rd_String_Check(path_name_word);
  if ((path_name = Pl_M_Absolute_Path_Name(path_name)) == NULL)
    Pl_Err_Domain(pl_domain_os_path, path_name_word);

  return path_name;
}




/*-------------------------------------------------------------------------*
 * DATE_TIME_TO_PROLOG                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Date_Time_To_Prolog(time_t *t, WamWord date_time_word)
{
  WamWord word, tag_mask;
  WamWord year_word, month_word, day_word;
  WamWord hour_word, minute_word, second_word;
  struct tm *tm;
  int day, month, year;
  int hour, minute, second;

  tm = localtime(t);

  year = tm->tm_year + 1900;
  month = tm->tm_mon + 1;
  day = tm->tm_mday;
  hour = tm->tm_hour;
  minute = tm->tm_min;
  second = tm->tm_sec;


  DEREF(date_time_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_LST_MASK &&
      tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_compound, word);

  if (!Pl_Get_Structure(atom_dt, 6, word))
    Pl_Err_Domain(pl_domain_date_time, word);

  year_word = Pl_Unify_Variable();
  month_word = Pl_Unify_Variable();
  day_word = Pl_Unify_Variable();
  hour_word = Pl_Unify_Variable();
  minute_word = Pl_Unify_Variable();
  second_word = Pl_Unify_Variable();

  Pl_Check_For_Un_Integer(year_word);
  Pl_Check_For_Un_Integer(month_word);
  Pl_Check_For_Un_Integer(day_word);
  Pl_Check_For_Un_Integer(hour_word);
  Pl_Check_For_Un_Integer(minute_word);
  Pl_Check_For_Un_Integer(second_word);

  return Pl_Get_Integer(year, year_word) &&
    Pl_Get_Integer(month, month_word) &&
    Pl_Get_Integer(day, day_word) &&
    Pl_Get_Integer(hour, hour_word) &&
    Pl_Get_Integer(minute, minute_word) && Pl_Get_Integer(second, second_word);
}
