/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : machine1.c                                                      *
 * Descr.: machine dependent features                                      *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "gp_config.h"

#if 0

#define USE_ALONE
#define DEBUG
#if 0
#define USE_W32_GUI_CONSOLE
#endif

#endif



#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/param.h>
#else
#include <windows.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#endif


#ifdef __CYGWIN__
#include <process.h>
#endif


#define MACHINE1_FILE
#include "machine1.h"

#ifdef USE_W32_GUI_CONSOLE
#include "../Linedit/linedit.h"
#define printf LE_Printf
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




/*-------------------------------------------------------------------------*
 * INIT_MACHINE1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Machine1(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

  struct utsname uname_info;

  m_os_type = M_OS_UNIX;

  if (uname(&uname_info) < 0)
    {
      strcpy(m_architecture, "architecture unknown");
      strcpy(m_os_version, "OS version unknown");
      return;
    }

  strcpy(m_architecture, uname_info.machine);

  sprintf(m_os_version, "%s %s", uname_info.sysname, uname_info.release);

#else

  SYSTEM_INFO si;
  char *p;
  OSVERSIONINFO osvi;

  GetSystemInfo(&si);
  if (si.wProcessorLevel >= 3 && si.wProcessorLevel < 10)
    sprintf(m_architecture, "i%c86", si.wProcessorLevel + '0');
  else
    sprintf(m_architecture, "i%d", si.dwProcessorType);

  m_os_type = M_OS_WINDOWS;

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  if (!GetVersionEx(&osvi))
    {
      strcpy(m_os_version, "OS version unknown");
      return;
    }

  switch (osvi.dwPlatformId)
    {
    case VER_PLATFORM_WIN32_WINDOWS:
      sprintf(m_os_version, "Win%s %d.%d (%d)",
	      (osvi.dwMinorVersion == 0) ? "95" : "98",
	      osvi.dwMajorVersion, osvi.dwMinorVersion,
	      LOWORD(osvi.dwBuildNumber));
      break;

    case VER_PLATFORM_WIN32_NT:
      m_os_type = M_OS_WINDOWS_NT;
      sprintf(m_os_version, "WinNT %d.%d (%d)",
	      osvi.dwMajorVersion, osvi.dwMinorVersion, osvi.dwBuildNumber);
      break;

    default:
      strcpy(m_os_version, "unknown windows OS version");
      return;
    }

  for (p = osvi.szCSDVersion; *p; p++)
    if (!isspace(*p))
      break;

  if (*p)
    sprintf(m_os_version + strlen(m_os_version), " - %s", p);

#endif
}




/*-------------------------------------------------------------------------*
 * M_CREATE_SHELL_COMMAND                                                  *
 *                                                                         *
 * Create a shell command if != NULL (or else a shell invocation)          *
 *-------------------------------------------------------------------------*/
char **
M_Create_Shell_Command(char *cmd)
{
  static char *arg[4];
  char *p;

#if defined(__unix__) || defined(__CYGWIN__)

  arg[0] = ((p = getenv("SHELL")) != NULL) ? p : "/bin/sh";
  arg[1] = "-c";

#else

  arg[0] = ((p = getenv("COMSPEC")) != NULL)
    ? p : (m_os_type == M_OS_WINDOWS_NT) ? "cmd.exe" : "c:\\command.com";
  arg[1] = "/c";

#endif

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
 * M_CMD_LINE_TO_ARGV                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char **
M_Cmd_Line_To_Argv(char *cmd, int *argc)
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

#if defined(_WIN32) && !defined(__CYGWIN__)
      if (*p == '"')
	{
	  while (*p != '"' && *p != '\0')
	    p++;
	  if (*p == '"')
	    p++;
	}
      else
#endif
	while (*p != ' ' && *p != '\t' && *p != '\0')
	  p++;

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
 * M_SHELL                                                                 *
 *                                                                         *
 * Invoke a shell (eventually passing a cmd if != NULL)                    *
 *-------------------------------------------------------------------------*/
int
M_Shell(char *cmd)
{
  return M_Spawn(M_Create_Shell_Command(cmd));
}




/*-------------------------------------------------------------------------*
 * M_SPAWN                                                                 *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * return the status or -1 if cannot execute (errno is set) or -2 else     *
 * (errno is not set).                                                     *
 *-------------------------------------------------------------------------*/
int
M_Spawn(char *arg[])
{
#if defined(__unix__)
  int pid;

  fflush(stdout);
  fflush(stderr);

  if (arg[1] == (char *) 1)
    arg = M_Cmd_Line_To_Argv(arg[0], NULL);

  pid = fork();

  if (pid == -1)
    return -1;

  if (pid == 0)			/* child process */
    {
      execvp(arg[0], arg);	/* only returns on error */
      exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
    }

  return M_Get_Status(pid);

#else

#if defined(_MSC_VER)
  _flushall();
#endif

  if (arg[1] == (char *) 1)
    arg = M_Cmd_Line_To_Argv(arg[0], NULL);

  return spawnvp(_P_WAIT, arg[0], (const char *const *) arg);
#endif
}




/*-------------------------------------------------------------------------*
 * M_SPAWN_REDIRECT                                                        *
 *                                                                         *
 * Execute a command with arguments in arg[], (arg[0]=the name of the cmd) *
 * a NULL must follow the last argument.                                   *
 * if arg[1]==(char *) 1 then arg[0] is considered as a command-line.      *
 * detach: 1 for a detached process (cannot obtain its status then).       *
 * f_in, f_out, f_err: ptrs to FILE * vars. if NULL not redirected,        *
 * f_out==f_err the 2 output streams are merged in f_out.                  *
 * In case of error return -1 if errno is set or else -2.                  *
 * In case of success, return 0 if detached or the pid else (the function  *
 * M_Get_Status() should be called later to avoid zombie processes).       *
 *-------------------------------------------------------------------------*/
int
M_Spawn_Redirect(char *arg[], int detach,
		 FILE **f_in, FILE **f_out, FILE **f_err)
{
#if defined(__unix__ ) || defined(__CYGWIN__)
  int pipe_in[2], pipe_out[2], pipe_err[2];
  int pid, status;

  fflush(stdout);
  fflush(stderr);

  if (arg[1] == (char *) 1)
    arg = M_Cmd_Line_To_Argv(arg[0], NULL);

  if ((f_in && pipe(pipe_in)) ||
      (f_out && pipe(pipe_out)) ||
      (f_err && f_err != f_out && pipe(pipe_err)))
    goto err;

  pid = (int) fork();
  if (pid == -1)
    goto err;

  if (pid == 0)			/* the child process */
    {
      if (!detach || fork() == 0)	/* pid needed ? */
	{			/* nested fork to detach exec process to avoid zombie process */
	  if (f_in && (close(pipe_in[1]) ||
		       (pipe_in[0] != 0 &&
			(dup2(pipe_in[0], 0) == -1 || close(pipe_in[0])))))
	    goto err;

	  if (f_out && (close(pipe_out[0]) ||
			(pipe_out[1] != 1 &&
			 (dup2(pipe_out[1], 1) == -1
			  || close(pipe_out[1])))))
	    goto err;

	  if (f_err)
	    {
	      if (f_err != f_out)
		{
		  if (close(pipe_err[0]) ||
		      (pipe_err[1] != 2 &&
		       (dup2(pipe_err[1], 2) == -1 || close(pipe_err[1]))))
		    goto err;
		}
	      else if (dup2(1, 2) == -1)
		goto err;
	    }

	  execvp(arg[0], arg);	/* only returns on error */
#ifdef DEBUG
	  DBGPRINTF("ERROR EXEC errno=%d\n", errno);
#endif
	  exit((errno == ENOENT || errno == ENOTDIR) ? 126 : 127);
	}
      else
	exit(0);		/* detatch: terminate child */
    }

  if (detach)			/* wait child termination */
    {
      if (waitpid(pid, &status, 0) < 0)
	goto err;
      pid = 0;
    }

  if (f_in && (close(pipe_in[0]) ||
	       (*f_in = fdopen(pipe_in[1], "wt")) == NULL))
    goto err;

  if (f_out && (close(pipe_out[1]) ||
		(*f_out = fdopen(pipe_out[0], "rt")) == NULL))
    goto err;

  if (f_err && f_err != f_out &&
      (close(pipe_err[1]) || (*f_err = fdopen(pipe_err[0], "rt")) == NULL))
    goto err;

  return pid;			/* NB: if detach: pid = 0 */

err:
  return -1;

#else

  int pid = 0, status;
  SECURITY_ATTRIBUTES sa = { 0 };
  STARTUPINFO si = { 0 };
  PROCESS_INFORMATION pi = { 0 };
  HANDLE pipe_in_r = NULL;
  HANDLE pipe_in_w = NULL;
  HANDLE pipe_out_r = NULL;
  HANDLE pipe_out_w = NULL;
  HANDLE pipe_err_r = NULL;
  HANDLE pipe_err_w = NULL;
  static char buff[4096];
  char *cmd, *p;
  static char delim[2] = { '\0', '\0' };
  int i, n;

  sa.nLength = sizeof(sa);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  if ((f_in && !CreatePipe(&pipe_in_r, &pipe_in_w, &sa, 0)) ||
      (f_out && !CreatePipe(&pipe_out_r, &pipe_out_w, &sa, 0)) ||
      (f_err && f_err != f_out
       && !CreatePipe(&pipe_err_r, &pipe_err_w, &sa, 0)))
    goto unknown_err;

  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  si.wShowWindow = SW_HIDE;
  si.hStdInput = (f_in) ? pipe_in_r : GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = (f_out) ? pipe_out_w : GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdError = (f_err) ? ((f_err == f_out) ? pipe_out_w : pipe_err_w)
    : GetStdHandle(STD_ERROR_HANDLE);
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
  if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE,
		     (detach) ? DETACHED_PROCESS : 0, NULL, NULL, &si, &pi))
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
      goto unknown_err;
    }

  if ((f_in && !CloseHandle(pipe_in_r)) ||
      (f_out && !CloseHandle(pipe_out_w)) ||
      (f_err && f_err != f_out && !CloseHandle(pipe_err_w)))
    goto unknown_err;

  if (f_in &&
      (*f_in =
       fdopen(_open_osfhandle((long) pipe_in_w, _O_TEXT), "wt")) == NULL)
    goto err;

  if (f_out &&
      (*f_out =
       fdopen(_open_osfhandle((long) pipe_out_r, _O_TEXT), "rt")) == NULL)
    goto err;

  if (f_err && f_err != f_out &&
      (*f_err =
       fdopen(_open_osfhandle((long) pipe_err_r, _O_TEXT), "rt")) == NULL)
    goto err;

  return (detach) ? 0 : (int) pi.hProcess;

err:
  return -1;

unknown_err:
  return -2;
#endif
}




/*-------------------------------------------------------------------------*
 * M_GET_STATUS                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
M_Get_Status(int pid)
{
  int status;

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

  WaitForSingleObject((HANDLE) pid, INFINITE);
  if (!GetExitCodeProcess((HANDLE) pid, &status))
    status = -2;

  CloseHandle((HANDLE) pid);
#endif

  return status;
}




/*-------------------------------------------------------------------------*
 * M_MKTEMP                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
M_Mktemp(char *tmp_template)
{
#ifdef HAVE_MKSTEMP

  int fd = mkstemp(tmp_template);
  if (fd == -1)
    return NULL;

  close(fd);
  unlink(tmp_template);		/* don't leave it sitting around... */
  return tmp_template;

#else

  errno = 0;
  return mktemp(tmp_template);

#endif
}




/*-------------------------------------------------------------------------*
 * M_TEMPNAM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
M_Tempnam(char *dir, char *pfx)
{
#ifdef HAVE_MKSTEMP
  char tmpl[MAXPATHLEN];
  char *d;
  int dlen, plen;
  int fd;
  struct stat buf;

#define Dir_Exists(dir) (stat(dir, &buf) == 0 && S_ISDIR (buf.st_mode))


				/* this code comes from glibc */
  if (!pfx || !pfx[0])
    {
      pfx = "file";
      plen = 4;
    }
  else
    {
      plen = strlen(pfx);
      if (plen > 5)
        plen = 5;
    }
 
  d = getenv("TMPDIR");
  if (d != NULL && Dir_Exists(d))
    dir = d;
  else if (dir != NULL && Dir_Exists(dir))
    /* nothing */ ;
  else
    dir = NULL;

  if (dir == NULL)
    {
      if (Dir_Exists(P_tmpdir))
        dir = P_tmpdir;
      else if (strcmp(P_tmpdir, "/tmp") != 0 && Dir_Exists("/tmp"))
        dir = "/tmp";
      else
        {
          errno = ENOENT;
          return NULL;
        }
    }
 
  dlen = strlen(dir);
  while (dlen > 1 && dir[dlen - 1] == '/')
    dlen--;                     /* remove trailing slashes */
 
  /* check we have room for "${dir}/${pfx}XXXXXX\0" */
  if (MAXPATHLEN < dlen + 1 + plen + 6 + 1)
    {
      errno = EINVAL;
      return NULL;
    }

  sprintf(tmpl, "%.*s/%.*sXXXXXX", dlen, dir, plen, pfx);

  fd = mkstemp(tmpl);
  if (fd == -1)
     return NULL;

  close(fd);
  unlink(tmpl);			/* don't leave it sitting around... */
  return strdup(tmpl);

#else

  errno = 0;
  return tempnam(dir, pfx);

#endif
}



#ifdef USE_ALONE

/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 * Under Win32 to also test with the GUI Console active USE_W32_GUI_CONSOLE*
 * and compile with gplc machine1.c or gplc machine1.c --gui-console       *
 * WIN32 WARNING: it seems that the executable file name must be at least 2*
 * characters long (e.g. x.exe is not OK but xx.exe yes).                  *
 *-------------------------------------------------------------------------*/

#if defined(__unix__) || defined(__CYGWIN__)
#define PREFIX_DIR
#else
#define PREFIX_DIR "c:\\cygwin\\bin\\"
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
  int status = M_Get_Status(pid);		\
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
  strcpy(buff, PREFIX_DIR "bc --q");    /* should be modifiable */	\
  arg[0] = buff;							\
  arg[1] = (char *) 1;
#else
#define COMMAND
 arg[0]=PREFIX_DIR "bc";			\
 arg[1]="-q";					\
 arg[2]=NULL;
#endif

#define CDE_STRING "1+255\n$foo\n2^10\nquit\n"
#define CDE_INPUT  fprintf(i, CDE_STRING); fclose(i);


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

  Init_Machine1();
  printf("OS used:%s\n", m_os_version);


#if defined(_MSC_VER)
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {
    char buff[100];

    DBGPRINTF("HELLO World\n");
    LE_Gets(buff);
  }
#endif

#if 1
  if (argc > 1)
    {
      DBGPRINTF("1- Executing from argv[1]...=%s... no redirect\n",
		argv[1]);
      pid = M_Spawn_Redirect(argv + 1, 0, NULL, NULL, NULL);
      CHECK(pid);
      STAT(pid);

      DBGPRINTF("1b- Executing from argv[1]...=%s... Spawn\n", argv[1]);
      status = M_Spawn(argv + 1);
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
  pid = M_Spawn_Redirect(arg, 0, NULL, &o, NULL);
  CHECK(pid);
  READ("output", o);
  STAT(pid);
#endif

  COMMAND;
  DBGPRINTF("Command is: %s with following input:\n" CDE_STRING,
	    arg[0]);
  DBGPRINTF("--- end of input\n");



#if 1
  DBGPRINTF("3- command with redirected input\n");
  COMMAND;
  pid = M_Spawn_Redirect(arg, 0, &i, NULL, NULL);
  CHECK(pid);
    
  CDE_INPUT;
  STAT(pid);
#endif

#if 1
  DBGPRINTF("4- command with redirected input and output\n");
  COMMAND;
  pid = M_Spawn_Redirect(arg, 0, &i, &o, NULL);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("5- command with redirected input output and error\n");
  COMMAND;
  pid = M_Spawn_Redirect(arg, 0, &i, &o, &e);
  CHECK(pid);
  CDE_INPUT;
  READ("output", o);
  READ("error", e);
  STAT(pid);
#endif

#if 1
  DBGPRINTF("6- command with redirected input and output=error\n");
  COMMAND;
  pid = M_Spawn_Redirect(arg, 0, &i, &o, &o);
  CHECK(pid);
  CDE_INPUT;
  READ("output/error", o);
  STAT(pid);
#endif

#ifdef USE_W32_GUI_CONSOLE
  {				/* for W32GUICons */
    char buff[100];

    DBGPRINTF("Terminated - press ENTER\n");
    LE_Gets(buff);
  }
#endif

  return 0;
}

#endif /* USE_ALONE */
