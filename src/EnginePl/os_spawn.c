/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : os_spawn.c                                                      *
 * Descr.: OS process spawning implementation                              *
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
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include "bool.h"

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#else
#define WIN32_NO_STATUS
#include <windows.h>
#undef WIN32_NO_STATUS
#include <ntstatus.h>
#include <process.h>
#include <io.h>
#endif


#ifdef __CYGWIN__
#include <process.h>
#endif


#include "pl_long.h"
#define OS_SPAWN_FILE
#include "os_spawn.h"
#include "os_error.h"

#ifdef USE_W32_GUI_CONSOLE
#include "../Linedit/linedit.h"
#define printf LE_Printf
#endif


#if 0

#define USE_ALONE
#define DEBUG
#if 0
#define USE_W32_GUI_CONSOLE
#endif

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

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#if defined(__unix__) || defined(__CYGWIN__)
#define TypeFD int
#define NOT_AN_FD  -1
#else
#define TypeFD HANDLE
#define NOT_AN_FD  INVALID_HANDLE_VALUE /* actually (void *) -1 */
#endif

static Bool Safe_Close(TypeFD fd);
static TypeFD Open_Null_Device(Bool for_output);




/*-------------------------------------------------------------------------*
 * PL_CREATE_SHELL_COMMAND                                                 *
 *                                                                         *
 * Create a shell command if != NULL (or else a shell invocation)          *
 *-------------------------------------------------------------------------*/
char **
Pl_Create_Shell_Command(char *cmd)
{
  static char *arg[4];
  char *p;

  /* first test SHELL env. var. (works under windows with msys2, ...) */
  if ((p = getenv("SHELL")) != NULL)
    {
      arg[0] = p;
      arg[1] = "-c";
    }
  else
    {
#if defined(__unix__) || defined(__CYGWIN__)

      arg[0] = "/bin/sh";
      arg[1] = "-c";

#else

      arg[0] = ((p = getenv("COMSPEC")) != NULL) ? p : "cmd.exe";
      arg[1] = "/c";

#endif
    }
  
  if (cmd)
    {
      arg[2] = cmd;
      arg[3] = NULL;
    }
  else
    arg[1] = NULL;

  return arg;
}




/*-------------------------------------------------------------------------*
 * PL_CMD_LINE_TO_ARGV                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char **
Pl_Cmd_Line_To_Argv(char *cmd, int *argc)
{
  static char **arg = NULL;
  static int nb_arg = 0;
  char *p = cmd;
  int i = 0;

  for (;;)
    {
      while (*p == ' ' || *p == '\t')
	p++;

      if (*p == '\0')
	break;

      if (i >= nb_arg)
	{
	  nb_arg += 64;
	  arg = (arg == NULL) ? malloc(nb_arg * sizeof(char *))
	    : realloc(arg, nb_arg * sizeof(char *));
	}
      arg[i++] = p;

      while (*p != ' ' && *p != '\t' && *p != '\0')
	{
	  if (*p == '"')
	  {
	    do
	      p++;
	    while (*p != '"' && *p != '\0');
	    if (*p == '"')
	      p++;
	  }
	  else
	    p++;
	}

      if (*p == '\0')
	break;

      *p++ = '\0';
    }

  arg[i] = NULL;
  if (argc != NULL)
    *argc = i;
  return arg;
}




/*-------------------------------------------------------------------------*
 * PL_SHELL                                                                *
 *                                                                         *
 * Invoke a shell (eventually passing a cmd if != NULL)                    *
 *-------------------------------------------------------------------------*/
int
Pl_Shell(char *cmd)
{
  return Pl_Spawn(Pl_Create_Shell_Command(cmd));
}

#ifdef _WIN32
#define LOOKS_LIKE_NT_STATUS(s) (((s) & 0x80000000U) != 0) /* high bit set? */

static void
Print_Win_Error_Message(const char *argv0, LONG status)
{
  LONG win_err = status;
  Bool free_msg = FALSE;
  char *msg = NULL;

  
  if (LOOKS_LIKE_NT_STATUS(status))
    {			/* convert NT status to Win32 error */
      typedef ULONG (WINAPI *NT2DosErr)(LONG status);
 
      HMODULE ntdll = GetModuleHandleA("ntdll.dll");
      NT2DosErr p = (NT2DosErr) GetProcAddress(ntdll, "RtlNtStatusToDosError");

      win_err = p(status);	/* get Win32 error */
    }
  
  switch(status)		/* add other frequent cases here */
    {
    case STATUS_DLL_NOT_FOUND:
      msg = "DLL not found (missing dependency)";
      break;

    case STATUS_DLL_INIT_FAILED:
      msg = "DLL initialization failed";
      break;

    case STATUS_ACCESS_VIOLATION:
      msg = "Access violation";
      break;

    case STATUS_INVALID_IMAGE_FORMAT:
      msg = "Invalid image format";
      break;

    case STATUS_ILLEGAL_INSTRUCTION:
      msg = "Illegal instruction (check the CPU)";
      break;
    }

  /*
   * TODO: generalize Pl_Sys_Err_String() rename it Pl_Get_Error_Msg()
   * and move it here to keep this file standalone. Windows errors:
   * - NT errors (high-bit set) - OS NT (kernel + NTDLL) and loader errors
   * - Win32 errors (Win32 API, see GetLastError/SetLastError)
   * - errno (CRT errors)
   */
  if (msg == NULL)
    {
      FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		     FORMAT_MESSAGE_FROM_SYSTEM |
		     FORMAT_MESSAGE_IGNORE_INSERTS,
		     NULL,
		     win_err,
		     MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
		     (LPSTR)&msg, 0, NULL);
      free_msg = TRUE;
    }

  fprintf(stderr, "Executing %s: status=0x%08lx, win_err=%ld: %s\n",
	  argv0, (long) status, (long) win_err, msg ? msg : "");

  if (free_msg)
    LocalFree(msg);
}
#endif




/*-------------------------------------------------------------------------*
 * PL_SPAWN                                                                *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * Return the status or -1 if cannot execute (errno is set) or -2 else     *
 * (errno is not set).                                                     *
 *-------------------------------------------------------------------------*/
int
Pl_Spawn(char *arg[])
{
#if defined(__unix__) || defined(__CYGWIN__)
  int pid;

  fflush(stdout);
  fflush(stderr);

  if (arg[1] == (char *) 1)
    arg = Pl_Cmd_Line_To_Argv(arg[0], NULL);

  pid = fork();

  if (pid == -1)
    return -1;

  if (pid == 0)			/* child process */
    {
      execvp(arg[0], arg);	/* only returns on error */
      exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
    }

  return Pl_Get_Status(pid);

#else  /* WINDOWS */

  long status;
#if defined(_MSC_VER)
  _flushall();
#endif

  /*  printf("COMMAND: <%s>\n", arg[0]); */
  if (arg[1] == (char *) 1)
    arg = Pl_Cmd_Line_To_Argv(arg[0], NULL);
  /*
  {
    int i;
    for(i = 0; arg[i] != NULL; i++)
      printf("Arg :%d: <%s>\n", i, arg[i]);
  }
  */
  
  status = (long) spawnvp(_P_WAIT, arg[0], (char *const *) arg);
  if (status < -1)		/* -1: errno is already set */
    {  /* try to give more info on error */
      Print_Win_Error_Message(arg[0], status); 
      errno = EACCES;
      status = -1;
    }
  return (int) status;

#endif /* WINDOWS */
}




/*-------------------------------------------------------------------------*
 * SAFE_CLOSE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Safe_Close(TypeFD fd)
{
  int ret = 0;
  
  if (fd != NOT_AN_FD)
    {
#if defined(__unix__) || defined(__CYGWIN__)
      while((ret = close(fd)) && errno == EINTR)
	;
#else
      ret = !CloseHandle(fd); /* windows: 0 on error, !=0 on success - negate */
#endif
    }

  return ret >= 0;	      /* unix convention: 0 on success, -1 on error */
}




/*-------------------------------------------------------------------------*
 * OPEN_NULL_DEVICE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static TypeFD
Open_Null_Device(Bool for_output)
{
  static TypeFD fd_null[2] = {NOT_AN_FD, NOT_AN_FD};
  TypeFD fd = fd_null[for_output];

  if (fd == NOT_AN_FD)
    {
#if defined(__unix__) || defined(__CYGWIN__)
      fd = open("/dev/null", (for_output) ? O_WRONLY : O_RDONLY);
#else
      SECURITY_ATTRIBUTES sa;

      sa.nLength = sizeof(sa);
      sa.bInheritHandle = TRUE;	/* child inherits the handle */
      sa.lpSecurityDescriptor = NULL;

      fd = CreateFile("NUL", (for_output) ? GENERIC_WRITE : GENERIC_READ,
		      FILE_SHARE_READ|FILE_SHARE_WRITE, &sa, OPEN_EXISTING, 0, NULL);

#endif
      fd_null[for_output] = fd;
      
    }

  return fd;
}




/*-------------------------------------------------------------------------*
 * PL_SPAWN_REDIRECT                                                       *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * detach: 1 for a detached process (cannot obtain its status later).      *
 * f_in, f_out, f_err: ptrs to FILE * vars. if NULL not redirected,        *
 *  *f_xxx can be:                                                         *
 *    - a FILE * (existing file connected to child process)                *
 *    - M_SPAWN_REDIRECT_NULL (to associate /dev/zero)                     *
 *    - M_SPAWN_REDIRECT_CREATE (new file is created and *f_xxx is filled) *
 * if *f_out != M_SPAWN_REDIRECT_CREATE && *f_out == *f_err the 2 output   *
 *    streams are merged in f_out.                                         *
 * To merge out and err where both are M_SPAWN_REDIRECT_CREATE, pass same  *
 *    FILE * address (f_err == f_out).                                     *
 * In case of error return -1 if errno is set or else -2 (win32).          *
 * In case of success, return 0 if detached or the pid else (the function  *
 * Pl_Get_Status() should be called later to avoid zombie processes).      *
 *-------------------------------------------------------------------------*/
int
Pl_Spawn_Redirect(char *arg[], Bool detach, FILE **f_in, FILE **f_out,
		  FILE **f_err)
{
  FILE **file3[3] = { f_in, f_out, f_err };
  Bool merge_out_err = (file3[2] && file3[1] &&
			(file3[2] == file3[1] ||
			 (*file3[2] != M_SPAWN_REDIRECT_CREATE &&
			  *file3[2] == *file3[1])));
#if defined(__unix__) || defined(__CYGWIN__)

  TypeFD pipe3[3][2];
  TypeFD fd;
  int pid, status;
  int i;

  if (arg[1] == (char *) 1)
    arg = Pl_Cmd_Line_To_Argv(arg[0], NULL);

  for(i = 0; i < 3; i++)
    {
      if (file3[i] && (i != 2 || !merge_out_err))
	{
	  if (*file3[i] == M_SPAWN_REDIRECT_CREATE)
	    {
	      if (pipe(pipe3[i]))
		goto err;
	    }
	  else
	    {
	      if (*file3[i] == M_SPAWN_REDIRECT_NULL)
		fd = Open_Null_Device(i != 0);
	      else
		fd = fileno(*file3[i]);

	      if (fd < 0)
		goto err;
	      
	      /* below NOT_AN_FD can be replaced by fd
	       * since will be close after dup2
	       */
	      pipe3[i][0] = (i == 0) ? fd : NOT_AN_FD;
	      pipe3[i][1] = (i == 0) ? NOT_AN_FD : fd;	
	    }
	}
    }

  pid = (int) fork();
  if (pid == -1)
    goto err;

  if (pid == 0)			/* in the child process */
    {
      if (detach)		/* detach ? (yes if pid is needed) */
	setsid();		/* detach to avoid zombie process */
#if 0		 /* uncomment to ensure the child will not acquire a terminal */
      { int pid1;
	if ((pid1 = fork()) < 0) /* create grandchild to ensure no terminal */
	  goto err;
	if (pid1 != 0)
	  exit(0);    /* terminate child (only remains parent and grandchild) */
      }
#endif
      for(i = 0; i < 3; i++)
	{
	  if (file3[i])
	    {
	      if (i != 2 || !merge_out_err)
		{
		  /* if i==0 use pipe[0] else pipe[1] (close the other) */
		  TypeFD fd_for_me = pipe3[i][i != 0];
		  TypeFD fd_unused = pipe3[i][i == 0];
		      
		  if ((fd_for_me != i &&
		       (dup2(fd_for_me, i) == -1 || !Safe_Close(fd_for_me)))
		      || !Safe_Close(fd_unused))
		    goto err;
		}
	      else		/* here if redirect err on out */
		{
		  if (dup2(1, 2) == -1) 
		    goto err;
		}
	    }
	}

      execvp(arg[0], arg);	/* only returns on error */
#ifdef DEBUG
#if 1 /* deactivate to see errors even if stdout is redirected */
      DBGPRINTF("ERROR EXEC, command (without args)=<%s>  errno=%d\n", arg[0],
		errno);
#else      /* do not USE DBGPRINTF since can be redirected */
      char buff[4096];
      sprintf(buff, "echo 'ERROR EXEC, command (without args)=<%s>  errno=%d'",
	      arg[0], errno);
      system(buff);
#endif	
#endif
      exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
    }

  /* in the parent */
  if (detach)
    {
      if (waitpid(pid, &status, 0) < 0)	/* wait child termination */
	goto err;
      pid = 0;
    }

  for(i = 0; i < 3; i++)
    {
      if (file3[i] && *file3[i] == M_SPAWN_REDIRECT_CREATE &&
	  (i != 2 || !merge_out_err))
	{
	  TypeFD fd_for_me = pipe3[i][i == 0];
	  TypeFD fd_unused = pipe3[i][i != 0];
	  char *mode = (i == 0) ? "wt" : "rt";

	  if (!Safe_Close(fd_unused) ||
	      (*file3[i] = fdopen(fd_for_me, mode)) == NULL)
	    goto err;
	}
    }

  return pid;			/* NB: if detach: pid = 0 */

 err:
  return -1;

#else  /* WINDOWS */

  TypeFD pipe3[3][2] = { {NULL, NULL}, {NULL, NULL}, {NULL, NULL} };
  TypeFD fd;
  int pid, status;
  SECURITY_ATTRIBUTES sa = { 0 };
  STARTUPINFO si = { 0 };
  PROCESS_INFORMATION pi = { 0 };
  static char buff[4096];
  char *cmd, *p;
  static char delim[2] = { '\0', '\0' };
  int i, n;

  if (arg[1] == NULL || arg[1] == (char *) 1)
    cmd = arg[0];
  else
    {
      for (n = i = 0; arg[i]; i++)
	{
	  *delim = '\0';
	  for (p = arg[i]; *p; p++)
	    if (*p == ' ' || *p == '\t')
	      {
		*delim = '"';
		break;
	      }
	  n += sprintf(buff + n, "%s%s%s ", delim, arg[i], delim);
	}
      buff[n - 1] = '\0';
      cmd = buff;
    }

#ifdef DEBUG
  DBGPRINTF("   cmd=<%s>\n", cmd);
#endif

  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;	/* child inherits the handle */
  sa.lpSecurityDescriptor = NULL;

  for(i = 0; i < 3; i++)
    {
      if (file3[i] && (i != 2 || !merge_out_err))
	{
	  if (*file3[i] == M_SPAWN_REDIRECT_CREATE)
	    {
	      if (!CreatePipe(&(pipe3[i][0]), &(pipe3[i][1]), &sa, 0))
		goto windows_err;

	      /* Ensure the write (resp. read) handle to the pipe 
	       * for STDIN (resp. STDOUT/STDERR) is not inherited. */
	      if (!SetHandleInformation(pipe3[i][i == 0], HANDLE_FLAG_INHERIT,
					FALSE))
		goto windows_err;
	    }
	  else
	    {
	      if (*file3[i] == M_SPAWN_REDIRECT_NULL)
		fd = Open_Null_Device(i != 0);
	      else
		fd = (TypeFD) _get_osfhandle(fileno(*file3[i]));

	      if (fd == NOT_AN_FD)
		goto err;
	      
	      /* below NOT_AN_FD can be replaced by fd
	       * since will be close after dup2
	       */
	      pipe3[i][0] = (i == 0) ? fd : NOT_AN_FD;
	      pipe3[i][1] = (i == 0) ? NOT_AN_FD : fd;	
	    }
	}
    }

  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;/* add STARTF_USESHOWWINDOW ? */
  si.wShowWindow = SW_HIDE;	    /* if STARTF_USESHOWWINDOW is defined */
  si.hStdInput = (f_in) ? pipe3[0][0] : GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = (f_out) ? pipe3[1][1] : GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdError = (f_err) ? ((merge_out_err) ? pipe3[1][1] : pipe3[2][1]) :
    GetStdHandle(STD_ERROR_HANDLE);


  /*
   * Initially, used the flag DETACHED_PROCESS but under Win32 this creates 
   * a console window. With DETACHED_PROCESS, the child does not inherit 
   * parent console. If it is a console app, it creates a new (visible) console.
   * A possibility is to use CREATE_NO_WINDOW, a new console (conhost.exe) 
   * that doesn't have a window is created. This is OK in some cases but not
   * OK with process needing a terminal (interactive).
   *
   * Now uses CREATE_BREAKAWAY_FROM_JOB + .
   */
  if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE,
		     (detach) ? CREATE_BREAKAWAY_FROM_JOB : 0, NULL, NULL,
		     &si, &pi))
    {
      status = GetLastError();
#ifdef DEBUG
      DBGPRINTF("ERROR from Create_Process=%d\n", status);
#endif
      if (status == ERROR_FILE_NOT_FOUND || status == ERROR_PATH_NOT_FOUND)
	{
	  errno = ENOENT;
	  goto err;
	}
      goto windows_err;
    }

  for(i = 0; i < 3; i++)
    {
      if (file3[i] && *file3[i] == M_SPAWN_REDIRECT_CREATE &&
	  (i != 2 || !merge_out_err))
	{
	  TypeFD fd_for_me = pipe3[i][i == 0];
	  TypeFD fd_unused = pipe3[i][i != 0];
	  char *mode = (i == 0) ? "wt" : "rt";

	  if (!Safe_Close(fd_unused))
	    goto windows_err;

	  if ((*file3[i] = fdopen(_open_osfhandle((PlLong) fd_for_me, _O_TEXT),
				  mode)) == NULL)
	    goto err;
	}
    }

  /*
   * Use id (pi.hProcess) rather than handle (pi.hProcess) which is 64 bit.
   * OpenProcess can be used to get handle back
   */
  pid = (int) pi.dwProcessId;

  if (detach)
    {
      HANDLE h_job = CreateJobObject(NULL, NULL);
      AssignProcessToJobObject(h_job, pi.hProcess);
	/* wait for child termination */
#if 0
      status = Pl_Get_Status(pid);
      if (status == OS_ERROR_WIN32)
	goto windows_err;
#else
      if (WaitForSingleObject(pi.hProcess, INFINITE) == WAIT_FAILED ||
	  !GetExitCodeProcess(pi.hProcess, (LPDWORD) &status))
	goto windows_err;
#endif
      CloseHandle(pi.hThread);
      CloseHandle(pi.hProcess);
      pid = 0;
    }

  return pid;			/* NB: if detach: pid = 0 */
  
 err:
  return -1;

 windows_err:
  return OS_ERROR_WIN32;
#endif
}




/*-------------------------------------------------------------------------*
 * PL_GET_STATUS                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Get_Status(int pid)
{
  int status = 0;

#if defined(__unix__) || defined(__CYGWIN__)

  if (waitpid(pid, &status, 0) < 0)
    return -1;

  if (WIFEXITED(status))
    {
      status = WEXITSTATUS(status);
      if (status == 127)
	status = -2;
      else if (status == 126)
	{
	  status = -1;
	  errno = ENOENT;
	}
    }

#elif defined(_WIN32)

  /* Use OpenProcess to get back handle from process id (see above).
   * (bug on XP/Vista) HANDLE phandle = OpenProcess(PROCESS_ALL_ACCESS, 1, pid);
   */
  HANDLE phandle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, 1, pid);

  if (phandle == 0)
    {
#ifdef DEBUG
      printf("ERROR from OpenProcess: %d\n", (int) GetLastError());
#endif
      status = OS_ERROR_WIN32;
      return status;
    }
  if (WaitForSingleObject(phandle, INFINITE) == WAIT_FAILED) 
    {
#ifdef DEBUG
      printf("ERROR from WaitForSingleObject: %d\n", (int) GetLastError());
#endif
      status = OS_ERROR_WIN32;
    } 
  else if (!GetExitCodeProcess(phandle, (LPDWORD) &status))
    {
#ifdef DEBUG
      printf("ERROR from GetExitCodeProcess: %d\n", (int) GetLastError());
#endif
      status = OS_ERROR_WIN32;
    }

  CloseHandle(phandle);
#endif

  return status;
}




#ifdef USE_ALONE

/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 * Under Win32 to also test with the GUI Console active USE_W32_GUI_CONSOLE*
 * and compile with gplc os_spawn.c or gplc os_spawn.c --gui-console       *
 * WIN32 WARNING: it seems that the executable file name must be at least 2*
 * characters long (e.g. x.exe is not OK but xx.exe yes).                  *
 *                                                                         *
 * execute with ./os_spawn PROG ARG1 ARG2...                               *
 *-------------------------------------------------------------------------*/

#if defined(_WIN32) || defined(__CYGWIN__)
#include <io.h>
#endif

#if defined(__unix__) || defined(__CYGWIN__)
#define PREFIX_DIR
#else
#define PREFIX_DIR "c:\\msys64\\usr\\bin\\"
#endif


#define READ(str, f)					\
{							\
  char buff[1024];					\
 							\
  DBGPRINTF("\n   Reading redirected %s\n", str);	\
  while (fgets(buff, sizeof(buff), f))			\
    {							\
      if (buff[strlen(buff) - 1] == '\n')		\
        buff[strlen(buff) - 1] = '\0';			\
      DBGPRINTF("   <%s>\n", buff);			\
      if (feof(f))					\
        break;						\
    }							\
  fclose(f);						\
  DBGPRINTF("   End reading redirected %s\n", str);	\
}


#define CHECK(pid)						\
{								\
  if (pid == -1)						\
    {								\
      DBGPRINTF("   ERROR executing Spawn: errno=%d\n", errno);	\
      exit(1);							\
    }								\
  if (pid == -2)						\
    {								\
      DBGPRINTF("ERROR executing Spawn: unknown error\n");	\
      exit(1);							\
    }								\
  DBGPRINTF("   pid=%d (%x)\n", pid, pid);			\
}


#define STAT(pid)				\
{						\
  int status = Pl_Get_Status(pid);		\
  STATUS(status)				\
}

#define STATUS(status)				\
{						\
  DBGPRINTF("   status=%d", status);		\
  if (status == -1)				\
    DBGPRINTF(" errno=%d", errno);		\
  DBGPRINTF("\n\n");				\
}


#if 1
#define COMMAND								\
  i = M_SPAWN_REDIRECT_CREATE;						\
  o = M_SPAWN_REDIRECT_CREATE;						\
  e = M_SPAWN_REDIRECT_CREATE;						\
  strcpy(buff, PREFIX_DIR "bc -q");    /* should be modifiable */	\
  arg[0] = buff;							\
  arg[1] = (char *) 1;
#else
#define COMMAND
  i = M_SPAWN_REDIRECT_CREATE;						\
  o = M_SPAWN_REDIRECT_CREATE;						\
  e = M_SPAWN_REDIRECT_CREATE;						\
 arg[0] = PREFIX_DIR "bc";						\
 arg[1] = "-q";								\
 arg[2] = NULL;
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
#define SET_MODE_BIN(f) _setmode(fileno(f), O_BINARY)
#else
#define SET_MODE_BIN(f)
#endif

#define CDE_STRING "1+255\n$foo\n2^10\nquit\n"
#define CDE_INPUT  SET_MODE_BIN(i); fprintf(i, CDE_STRING); fclose(i);


#if 0
#define POLL
#include <sys/poll.h>
#endif
#ifdef POLL
  {
    int fd = fileno(i);
    struct pollfd ufd = { fd, 7, 0 };
    int r = poll(&ufd, 1, 100);
    DBGPRINTF("poll ret:%d on fd %d returned events :%x\n", r, fd, ufd.revents);
    return 0;
  }
#endif

int
main(int argc, char *argv[])
{
  FILE *i, *o, *e;
  int pid, status;
  char *arg[10];
  char buff[256];


#if defined(_MSC_VER)
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {
    char buff[100];

    DBGPRINTF("HELLO World\n");
    Pl_LE_Gets(buff);
  }
#endif

#if 1
  if (argc > 1)
    {
      DBGPRINTF("1- Executing from argv[1]...=%s... no redirect\n", argv[1]);
      pid = Pl_Spawn_Redirect(argv + 1, FALSE, NULL, NULL, NULL);
      CHECK(pid);
      STAT(pid);

      DBGPRINTF("1b- Executing from argv[1]...=%s... Spawn\n", argv[1]);
      status = Pl_Spawn(argv + 1);
      STATUS(status);
    }
  else
    DBGPRINTF("1- Executing from argv[1] - ignored\n");

#endif

#if 1
  DBGPRINTF("2- Executing uname -a with redirected output\n");
  strcpy(buff, PREFIX_DIR "uname -a");	/* should be modifiable */
  arg[0] = buff;
  arg[1] = (char *) 1;
  o = M_SPAWN_REDIRECT_CREATE;
  pid = Pl_Spawn_Redirect(arg, FALSE, NULL, &o, NULL);
  CHECK(pid);
  READ("output", o);
  STAT(pid);
#endif

  COMMAND;
  DBGPRINTF("Command is: %s with following input:\n" CDE_STRING, arg[0]);
  DBGPRINTF("--- end of input\n");

#if 1
  DBGPRINTF("3- command with redirected input\n");
  COMMAND;
  pid = Pl_Spawn_Redirect(arg, FALSE, &i, NULL, NULL);
  CHECK(pid);

  CDE_INPUT;
  STAT(pid);
#endif

#if 1
  DBGPRINTF("4- command with redirected input and output\n");
  COMMAND;
  pid = Pl_Spawn_Redirect(arg, FALSE, &i, &o, NULL);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("5- command with redirected input output and error\n");
  COMMAND;
  pid = Pl_Spawn_Redirect(arg, FALSE, &i, &o, &e);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  READ("error", e);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("6- command with redirected input and output=error\n");
  COMMAND;
  pid = Pl_Spawn_Redirect(arg, FALSE, &i, &o, &o);
  CHECK(pid);
  CDE_INPUT;
  READ("output/error", o);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("7- command with redirected input and output, error=/dev/null\n");
  COMMAND;
  e = M_SPAWN_REDIRECT_NULL;
  pid = Pl_Spawn_Redirect(arg, FALSE, &i, &o, &e);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  STAT(pid);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {				/* for W32GUICons */
    char buff[100];

    DBGPRINTF("Terminated - press ENTER\n");
    Pl_LE_Gets(buff);
  }
#endif

  return 0;
}

#endif /* USE_ALONE */
