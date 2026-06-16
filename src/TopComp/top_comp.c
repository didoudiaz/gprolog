/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : top_comp.c                                                      *
 * Descr.: compiler main (shell) program                                   *
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

#include "../EnginePl/gp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>

#ifdef _WIN32
#include <windows.h>
#include <process.h>
#include <fcntl.h>
#include <io.h>
#else
#include <dirent.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/wait.h>
#endif

#include "../EnginePl/bool.h"
#include "../EnginePl/pl_params.h"
#include "../EnginePl/wam_regs.h"
#include "../EnginePl/os_path.h"
#include "../EnginePl/os_spawn.h"

#include "mangling.h"
#include "copying.h"


#if 0
#define DEBUG
#endif

/*
 * Windows64 : allows for LARGEADDRESSAWARE (LAA)
 * if yes, needs RIP-related addressing (see x86_64_any.c)
 * if no, pass option /LARGEADDRESSAWARE:NO to cl
 */
#if 1 
#define LARGE_ADDRESS_AWARE 1
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define CMD_LINE_MAX_OPT           8192
#define CMD_LINE_LENGTH            (MAXPATHLEN + CMD_LINE_MAX_OPT + 1)

#define TEMP_FILE_PREFIX           GPLC

#define OBJ_FILE_ALL_PL_BIPS       "all_pl_bips"
#define OBJ_FILE_ALL_FD_BIPS       "all_fd_bips"
#define OBJ_FILE_TOP_LEVEL         "top_level"
#define OBJ_FILE_TOP_LEVEL_MAIN    "top_level_main"
#define OBJ_FILE_DEBUGGER          "debugger"

#define EXE_FILE_PL2WAM            "pl2wam"
#define EXE_FILE_WAM2MA            "wam2ma"
#define EXE_FILE_MA2ASM            "ma2asm"
#define EXE_FILE_ASM               AS
#define EXE_FILE_FD2C              "fd2c"
#define EXE_FILE_CC                CC
#define EXE_FILE_LINK              CC
#define EXE_FILE_STRIP             STRIP




#define FILE_PL                    0
#define FILE_WAM                   1
#define FILE_MA                    2
#define FILE_ASM                   3
#define FILE_OBJ                   4

#define FILE_FD                    5
#define FILE_C                     6
#define FILE_LINK                  7

#define LINK_OPTION                8




#define PL_SUFFIX                  PROLOG_FILE_SUFFIX
#define PL_SUFFIX_ALTERNATE        PROLOG_FILE_SUFFIXES_ALT
#define WAM_SUFFIX                 ".wam"
#define WBC_SUFFIX                 ".wbc"
#define MA_SUFFIX                  ".ma"
#define FD_SUFFIX                  ".fd"
#define C_SUFFIX                   ".c"
#ifndef _MSC_VER /* gcc/clang can handle .S files - remove if separate cpp wanted */
#define C_SUFFIX_ALTERNATE         "|.C|.cc|.CC|.cxx|.CXX|.c++|.C++|.cpp|.CPP|.S|"
#else
#define C_SUFFIX_ALTERNATE         "|.C|.cc|.CC|.cxx|.CXX|.c++|.C++|.cpp|.CPP|"
#endif

#define CC_COMPILE_OPT             "-c "
#define CC_INCLUDE_OPT             "-I"



#define CMD_FILTER_NONE            ((char *) 0)
#define CMD_FILTER_DEMANGLE     ((char *) 1)
/* else CMD_FILTER_WRITE_FILE */



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  char *name;			/* path given on command-line */
  char *suffix;			/* suffix part */
  char *file_part;		/* file name part */
  int type;			/* see FILE_xxx macros */
  char *work_name1;		/* source to compile */
  char *work_name2;		/* target to produce */
}
FileInf;


typedef struct
{
  char *exe_name;
  char opt[CMD_LINE_MAX_OPT];
  char *out_opt;
}
CmdInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char *start_path;

Bool devel_mode = FALSE;
char *devel_dir[] = {"EnginePl", "BipsPl", "EngineFD", "BipsFD", "Linedit",
		     "W32GUICons", "TopComp", NULL };


FileInf *file_lopt;		/* files on command-line (and link options) */
int nb_file_lopt = 0;

int stop_after = FILE_LINK;
Bool verbose = FALSE;
char *file_name_out = NULL;

int def_local_size = -1;
int def_global_size = -1;
int def_trail_size = -1;
int def_cstr_size = -1;
int def_max_atom = -1;
Bool fixed_sizes = FALSE;
Bool needs_stack_file = FALSE;

Bool bc_mode = FALSE;
Bool gui_console = FALSE;
Bool new_top_level = FALSE;
Bool no_top_level = FALSE;
Bool min_pl_bips = FALSE;
Bool min_fd_bips = FALSE;
Bool no_debugger = FALSE;
Bool no_pl_lib = FALSE;
Bool no_fd_lib = FALSE;
Bool no_fd_lib_warn = FALSE;
Bool strip = FALSE;

Bool no_demangle = FALSE;

char warn_str[1024] = "";

char *temp_dir = NULL;
Bool no_del_temp_files = FALSE;

/*
 * Almost each string ends with a space. However, executable names
 * EXE_XXX_NAME do not end with a space (so they can be used in Pl_Search_Path)
 * thus options must begin with a space (and end with a space too).
 */

CmdInf cmd_pl2wam = { EXE_FILE_PL2WAM, " ",                    "-o " };
CmdInf cmd_wam2ma = { EXE_FILE_WAM2MA, " ",                    "-o " };
CmdInf cmd_ma2asm = { EXE_FILE_MA2ASM, " ",                    "-o " };
CmdInf cmd_asm =    { EXE_FILE_ASM,    " " ASFLAGS " ",        "-o " };
CmdInf cmd_fd2c =   { EXE_FILE_FD2C,   " ",                    "-o " };
CmdInf cmd_cc =     { EXE_FILE_CC,     " ",                    CC_OBJ_NAME_OPT }; /* see below for others flags */
CmdInf cmd_link =   { EXE_FILE_LINK,   " " CFLAGS_MACHINE " ", CC_EXE_NAME_OPT };

char *cc_fd2c_flags = CFLAGS " ";



char *suffixes[] = { PL_SUFFIX, WAM_SUFFIX, MA_SUFFIX, ASM_SUFFIX,
		     OBJ_SUFFIX, FD_SUFFIX, C_SUFFIX, NULL };




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Determine_Pathnames(void);

void Compile_Files(void);

void Create_Output_File_Name(FileInf *f, char *buff);

void New_Work_File(FileInf *f, int stage, int stop_after);

void Release_Work_File2(FileInf *f);

void Compile_Cmd(CmdInf *c, FileInf *f);

void Link_Cmd(void);

void Exec_One_Cmd(char *str, char *cmd_filter);

int Spawn_And_Filter(char *arg[], char *cmd_filter);

void Delete_Temp_File(char *name);

int Find_File(char *file, char *suff, char *file_path, int ignore_error);

char *Find_Suffix(char *suffixes, char *suffix);

void Pl_Fatal_Error(char *format, ...) ATTR_PRINTF(1);

void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);



#define Record_Link_Warn_Option(i) \
  sprintf(warn_str + strlen(warn_str), "%s ", argv[i])




#define Before_Cmd(cmd)                         \
  if (verbose)                                  \
    fprintf(stderr, "%s\n", cmd)


#define After_Cmd(error)                        \
  if (error)                                    \
    Pl_Fatal_Error("compilation failed")
/*    Pl_Fatal_Error("compilation failed (returned status: %d hexa: %x)",
      status, status) */



char *last_opt;
Bool last_is_wx_opt;

#define Check_Arg(i, opt) \
  (last_opt = opt, strncmp(argv[i], opt, strlen(argv[i])) == 0)

	/* recognize option aliases like -L <string> and -Wl,<string> */
#define Check_Arg_MinusWx(i, opt, wx_opt)		\
  ((last_is_wx_opt = FALSE, Check_Arg(i, opt)) ||	\
   (last_is_wx_opt = TRUE, last_opt = argv[i] + 4,	\
    strncmp(argv[i], wx_opt, 4) == 0))

#define Add_Last_Option(opt)       sprintf(opt + strlen(opt), "%s ", last_opt)

#define Add_Option(i, opt)         sprintf(opt + strlen(opt), "%s ", argv[i])




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  char **pdev;

#ifdef _WIN32
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

  file_lopt = (FileInf *) calloc(argc + 1, sizeof(FileInf));
  if (file_lopt == NULL)
    Pl_Fatal_Error("memory allocation fault");

  Parse_Arguments(argc, argv);
  if (verbose)
    fprintf(stderr, "\n");

  start_path = Pl_Get_Prolog_Path(argv[0], &devel_mode);
  if (start_path == NULL)
    Pl_Fatal_Error("cannot find the path for %s, set environment variable %s",
		   PROLOG_NAME, ENV_VARIABLE);

  strcat(cmd_cc.opt, CFLAGS_MACHINE " " CFLAGS_REGS CC_COMPILE_OPT);

#ifdef WIN_AS_COPY
  /* see comment about WIN_AS_COPY in configure.in */
  if (Pl_Search_Path(WIN_AS_COPY) != NULL)
    cmd_asm.exe_name = WIN_AS_COPY;
#endif

  if (devel_mode)
    for (pdev = devel_dir; *pdev; pdev++)
      sprintf(cmd_cc.opt + strlen(cmd_cc.opt), "%s%s" DIR_SEP_S "%s ",
              CC_INCLUDE_OPT, start_path, *pdev);
  else
    sprintf(cmd_cc.opt + strlen(cmd_cc.opt), "%s%s" DIR_SEP_S "include ",
            CC_INCLUDE_OPT, start_path);

  strcat(cmd_link.opt, LDFLAGS " ");

  if (verbose)
    fprintf(stderr, "Path used: %s %s\n", start_path,
            (devel_mode) ? "(development mode)" : "");

  Compile_Files();

  return 0;
}




/*-------------------------------------------------------------------------*
 * COMPILE_FILES                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Compile_Files(void)
{
  FileInf *f;
  int stage;
  int stage_end;
  size_t l;
  FILE *fd;


  if (stop_after < FILE_LINK)
    {
      if (*warn_str)
        fprintf(stderr, "link not done - ignored option(s): %s\n", warn_str);

      stage_end = stop_after;
      needs_stack_file = FALSE;

      if (bc_mode)
        {
          suffixes[FILE_WAM] = WBC_SUFFIX;
          strcat(cmd_pl2wam.opt, "--wam-for-byte-code ");
        }
    }
  else
    stage_end = FILE_ASM;

  if (needs_stack_file)
    {
      f = file_lopt + nb_file_lopt;

      f->work_name2 = NULL;
      New_Work_File(f, FILE_WAM, 10000);        /* to create work_name2 */
      f->name = f->work_name2;
      f->suffix = f->name + strlen(f->name) - strlen(suffixes[FILE_MA]);
      f->type = FILE_MA;
      f->work_name1 = f->name;
      f->work_name2 = NULL;

      if (verbose)
        fprintf(stderr, "creating stack size file: %s\n", f->name);

      if ((fd = fopen(f->name, "wt")) == NULL)
        Pl_Fatal_Error("cannot open stack size file (%s)", f->name);

      if (def_local_size >= 0)
        fprintf(fd, "long global pl_def_local_size = %d\n", def_local_size);
      if (def_global_size >= 0)
        fprintf(fd, "long global pl_def_global_size = %d\n", def_global_size);
      if (def_trail_size >= 0)
        fprintf(fd, "long global pl_def_trail_size = %d\n", def_trail_size);
      if (def_cstr_size >= 0)
        fprintf(fd, "long global pl_def_cstr_size = %d\n", def_cstr_size);

      if (def_max_atom >= 0)
        fprintf(fd, "long global pl_def_max_atom = %d\n", def_max_atom);

      if (fixed_sizes)
        fprintf(fd, "long global pl_fixed_sizes = 1\n");

      fclose(fd);
    }

  if (verbose)
    fprintf(stderr, "\n*** Compiling\n");


  for (f = file_lopt; f->name; f++)
    {
      if (f->type == LINK_OPTION)
        continue;

      if (verbose &&
          (f->type == FILE_FD || f->type == FILE_C || f->type <= stage_end))
        fprintf(stderr, "\n--- file: %s\n", f->name);

      if (f->type == FILE_FD && stop_after >= FILE_ASM)
        {
          stage = FILE_FD;      /* to generate the correct C suffix */
          New_Work_File(f, stage, (stop_after == FILE_FD) ? stop_after : 10000);
          Compile_Cmd(&cmd_fd2c, f);
          if (stop_after != FILE_FD)
            {
              stage = FILE_ASM; /* to generate the correct obj suffix */
              New_Work_File(f, stage, stop_after);
              l = strlen(cmd_cc.opt);   /* add fd2c C options */
              strcpy(cmd_cc.opt + l, cc_fd2c_flags);
              Compile_Cmd(&cmd_cc, f);
              cmd_cc.opt[l] = '\0';     /* remove them */
            }
          goto release_work_file;
        }

      if (f->type == FILE_C && stop_after >= FILE_ASM && stop_after != FILE_FD)
        {
          stage = FILE_ASM;     /* to generate the correct obj suffix */
          New_Work_File(f, stage, stop_after);
          Compile_Cmd(&cmd_cc, f);
          goto release_work_file;
        }

      if (f->type == FILE_FD || f->type == FILE_C ||
          stop_after == FILE_FD || f->type > stop_after)
        {
          fprintf(stderr, "unused input file: %s\n", f->name);
          continue;
        }

      for (stage = f->type; stage <= stage_end; stage++)
        {
          New_Work_File(f, stage, stop_after);
          switch (stage)
            {
            case FILE_PL:
              Compile_Cmd(&cmd_pl2wam, f);
              break;

            case FILE_WAM:
              Compile_Cmd(&cmd_wam2ma, f);
              break;

            case FILE_MA:
	      if (strcmp(f->suffix, ".S") == 0)	/* preprocess .S via CPP */
		{
		  l = strlen(cmd_cc.opt); /* add C options for preprocessing */
		  strcpy(cmd_cc.opt + l, "-E "); /* actually /E for _MSC_VER */
		  Compile_Cmd(&cmd_cc, f);
		  cmd_cc.opt[l] = '\0';     /* remove them */
		  break;
		}
	      
              Compile_Cmd(&cmd_ma2asm, f);
              if (needs_stack_file && f == file_lopt + nb_file_lopt &&
                  !no_del_temp_files)
                {
                  if (verbose)
                    fprintf(stderr, "deleting stack size file\n");
                  Delete_Temp_File(f->name);
                }
              break;

            case FILE_ASM:
              Compile_Cmd(&cmd_asm, f);
              break;
            }
        }

    release_work_file:
      Release_Work_File2(f);       /* to suppress last useless temp file */
    }

  if (stop_after < FILE_LINK)
    return;

  if (verbose)
    fprintf(stderr, "\n*** Linking\n\n");

  Link_Cmd();

  /* removing temp files after link */
  for (f = file_lopt; f->name; f++)
    if (f->work_name1 != f->name) /* also ok if f->type == LINK_OPTION */
      Delete_Temp_File(f->work_name1);
}




/*-------------------------------------------------------------------------*
 * CREATE_OUTPUT_FILE_NAME                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Create_Output_File_Name(FileInf *f, char *buff)
{
  char *p;
  size_t l;
  static int counter = 0;

  for(p = file_name_out; *p; p++)
    {
      if (*p != '%')
        *buff++ = *p;
      else
        switch(* ++p)
          {
          case 'd':             /* %d = the directory part */
            strcpy(buff, f->name);
            l = f->file_part - f->name;
            buff += l;
            break;

          case 'f':             /* %f = the whole file name */
            strcpy(buff, f->name);
            buff += strlen(buff);
            break;

          case 'F':             /* %F = the whole file name (without dir) */
            strcpy(buff, f->file_part);
            buff += strlen(buff);
            break;

          case 'p':             /* %p = the prefix file name */
            strcpy(buff, f->name);
            l = f->suffix - f->name;
            buff += l;
            break;

          case 'P':             /* %P = the prefix file name (without dir) */
            strcpy(buff, f->file_part);
            l = f->suffix - f->file_part;
            buff += l;
            break;

          case 's':             /* %s = the suffix */
            strcpy(buff, f->suffix);
            buff += strlen(buff);
            break;

          case 'c':             /* %c = a counter */
            sprintf(buff, "%d", ++counter);
            buff += strlen(buff);
            break;

          default:
            *buff++ = '%';              /* no special % sequence */
            *buff++ = *p;
          }
    }
  *buff = '\0';
}




/*-------------------------------------------------------------------------*
 * NEW_WORK_FILE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
New_Work_File(FileInf *f, int stage, int stop_after)
{
  static char buff[MAXPATHLEN];
  char *p;

  if (stage < stop_after)       /* intermediate stage */
    {
      p = Pl_Tempnam(temp_dir, TEMP_FILE_PREFIX);
      sprintf(buff, "%s%s", p, suffixes[stage + 1]);
      free(p);
    }
  else                          /* final stage */
    if (file_name_out)          /* specified output filename */
      Create_Output_File_Name(f, buff);
    else
      {
        strcpy(buff, f->name);
        strcpy(buff + (f->suffix - f->name), suffixes[stage + 1]);
      }

  Release_Work_File2(f);
  f->work_name2 = strdup(buff);
}




/*-------------------------------------------------------------------------*
 * RELEASE_WORK_FILE2                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Release_Work_File2(FileInf *f)
{
  if (f->work_name2 != NULL)
    {
      if (f->work_name1 != f->name)
        Delete_Temp_File(f->work_name1);

      f->work_name1 = f->work_name2;
    }
}




/*-------------------------------------------------------------------------*
 * COMPILE_CMD                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Compile_Cmd(CmdInf *c, FileInf *f)
{
  static char buff[CMD_LINE_LENGTH];
  char *cmd_filter = CMD_FILTER_NONE;

  sprintf(buff, "%s%s%s%s %s", c->exe_name, c->opt, c->out_opt,
          f->work_name2, f->work_name1);

  /* check if it is an asm .S file given to C preprocessor */
  if (c == &cmd_cc && f->type == FILE_MA)
    {
      /*
       * MSVC /E always write to stdout (needs redirect with a filter
       * do it also for gcc/clang
       */
      sprintf(buff, "%s%s %s", c->exe_name, c->opt, f->work_name1);
      cmd_filter = f->work_name2; /* filter = CMD_FILTER_WRITE_FILE */
    }
  else
    {
      sprintf(buff, "%s%s%s%s %s", c->exe_name, c->opt, c->out_opt,
	      f->work_name2, f->work_name1);
    }
  
  Exec_One_Cmd(buff, cmd_filter);
}




/*-------------------------------------------------------------------------*
 * LINK_CMD                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Link_Cmd(void)
{
  static char file_out[MAXPATHLEN];
  static char buff[CMD_LINE_LENGTH];
  FileInf *f;
#ifdef _MSC_VER
  Bool has_gui_console = FALSE;
  char *unused;
#endif
  Bool use_cl = FALSE;

  if (!no_fd_lib && no_fd_lib_warn)
    {
      if (!Find_File(LIB_BIPS_FD, "", buff, 1) ||
          !Find_File(LIB_ENGINE_FD, "", buff + strlen(buff), 1))
        no_fd_lib = min_fd_bips = TRUE;
    }

  if (file_name_out == NULL)
    file_name_out = "%p";       /* will reuse first file name */

  for (f = file_lopt; f->type == LINK_OPTION; f++)
    ;                           /* use first file name by default */

  Create_Output_File_Name(f, file_out);
  file_name_out = file_out;

  /*
   * With MSVC: if cl.exe is not found, we try to use link.exe (MS linker).
   * It is a workaround for users who have installed a binary version
   * (from a setup.exe) compiled with cl.exe and do not have cl.exe.
   */
#ifdef _MSC_VER
  use_cl = (SearchPath(NULL, cmd_link.exe_name, ".exe", CMD_LINE_LENGTH, buff, 
	    &unused) && getenv("USE_LINKER") == NULL);

  if (!use_cl)
    {
      if (verbose)
	  {
	    printf("%s.exe not found ! we use link.exe\n", cmd_link.exe_name);
	    printf("In case of error check it is really the MS link.exe ");
	    printf("which is used and not the cygwin/msys link.exe utility\n");
	  }

      cmd_link.exe_name = "link";
      cmd_link.out_opt = "/out:";
      strcat(cmd_link.opt, "/nologo /stack:8000000 ");
    }
#endif	/* _MSC_VER */

  sprintf(buff, "%s%s%s%s ", cmd_link.exe_name, cmd_link.opt,
	  cmd_link.out_opt, file_name_out);

  /* f->work_name1 is OK for LINK_OPTION */
  for (f = file_lopt; f->name; f++)
    sprintf(buff + strlen(buff), "%s ", f->work_name1);

  if (!min_pl_bips)
    {
      Find_File(OBJ_FILE_ALL_PL_BIPS, OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
    }

#ifndef NO_USE_FD_SOLVER
  if (!min_fd_bips)
    {
      Find_File(OBJ_FILE_ALL_FD_BIPS, OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
    }
#endif

  if (new_top_level)
    {
      Find_File(OBJ_FILE_TOP_LEVEL_MAIN, OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
    }

  if (!no_top_level)
    {
      Find_File(OBJ_FILE_TOP_LEVEL, OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
    }

  if (!no_debugger)
    {
      Find_File(OBJ_FILE_DEBUGGER, OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
    }

#ifndef NO_USE_FD_SOLVER
  if (!no_fd_lib)
    {
      Find_File(LIB_BIPS_FD, "", buff + strlen(buff), 0);
      strcat(buff, " ");

      Find_File(LIB_ENGINE_FD, "", buff + strlen(buff), 0);
      strcat(buff, " ");
    }
#endif

  if (!no_pl_lib)
    {
      Find_File(LIB_BIPS_PL, "", buff + strlen(buff), 0);
      strcat(buff, " ");
    }

  Find_File(LIB_ENGINE_PL, "", buff + strlen(buff), 0);
  strcat(buff, " ");

#ifndef NO_USE_LINEDIT
  Find_File(LIB_LINEDIT, "", buff + strlen(buff), 0);
  strcat(buff, " ");
#endif

  strcat(buff, LDLIBS " ");

  if (!no_pl_lib && gui_console)
    {    /* modify Linedit/Makefile.in to follow this list of ld objects */
      Find_File("w32gc_interf", OBJ_SUFFIX, buff + strlen(buff), 0);
      strcat(buff, " ");
      Find_File("win_exe_icon", ".res", buff + strlen(buff), 1);
      strcat(buff, " ");
#ifdef _MSC_VER
      has_gui_console = TRUE;
#endif
    }

  if (use_cl)
    strcat(buff, "/link ");
  strcat(buff, LDFLAGS_END " ");
#ifdef _MSC_VER
  strcat(buff, "/ignore:4089 ");
  if (!has_gui_console)
    strcat(buff, "/subsystem:console ");
#if defined(M_x86_64) && !defined(LARGE_ADDRESS_AWARE)
  strcat(buff, "/LARGEADDRESSAWARE:NO ");
#endif
#endif

  Exec_One_Cmd(buff, no_demangle ? CMD_FILTER_NONE : CMD_FILTER_DEMANGLE);

  if (strip && *EXE_FILE_STRIP != ':' && *EXE_FILE_STRIP != '\0')
    {
      sprintf(buff, "%s %s%s", EXE_FILE_STRIP, file_name_out, EXE_SUFFIX);
      Exec_One_Cmd(buff, CMD_FILTER_NONE);
    }
}




/*-------------------------------------------------------------------------*
 * EXEC_ONE_CMD                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Exec_One_Cmd(char *cmd, char *cmd_filter)
{
#if 1
  int status;
  static char *arg[2] = { NULL, (char *) 1 };

  arg[0] = cmd;

  Before_Cmd(cmd);

  if (cmd_filter == CMD_FILTER_NONE)
    status = Pl_Spawn(arg);
  else
    status = Spawn_And_Filter(arg, cmd_filter);

  if (status == -1)
    {
      fprintf(stderr, "error trying to execute ");
      perror(arg[0]);
      exit(1);
    }

  if (status == -2)
    fprintf(stderr, "error trying to execute %s: unknown error", arg[0]);

  After_Cmd(status);

#else

  int status;

  Before_Cmd(cmd);
#ifdef DEBUG
  fprintf(stderr, "executing system() for: %s\n", cmd);
#endif
  status = system(cmd);
  status >>= 8;
  if (status == -1 || status == 127)
    Pl_Fatal_Error("error trying to execute %s", cmd);

  After_Cmd(status);
#endif
}




/*-------------------------------------------------------------------------*
 * SPAWN_AND_FILTER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Spawn_And_Filter(char *arg[], char *cmd_filter)
{
  int pid, status;
  FILE *f_out = M_SPAWN_REDIRECT_CREATE;
  FILE **pf_err = &f_out;	/* merge out and err for demangle */
  FILE *f_res = NULL;
  static char buff[CMD_LINE_LENGTH];

  f_out = M_SPAWN_REDIRECT_CREATE;

  if (cmd_filter != CMD_FILTER_DEMANGLE) /* redirect output of CPP */
    {
      pf_err = NULL; 		/* err not redirected */
      f_res = fopen(cmd_filter, "w");
      if (f_res == NULL)
	{
	  perror(cmd_filter);
	  exit(1);
	}
    }
    
  pid = Pl_Spawn_Redirect(arg, FALSE, NULL, &f_out, pf_err);
  if (pid == -1 || pid == -2)
    return pid;

  for (;;)
    {
      if (fgets(buff, sizeof(buff), f_out)) /* to avoid gcc warning warn_unused_result */
	{
	}

      if (feof(f_out))
	break;

      if (cmd_filter == CMD_FILTER_DEMANGLE)
	{
#ifndef DEBUG
	  fputs(Demangle_Line(buff, "predicate(%s)", 1, 1, 1), stderr);
#else
	  fprintf(stderr, "piped line:%s",
		  Demangle_Line(buff, "predicate(%s)", 1, 1, 1));
#endif
	}
      else			/* redirect CPP to res file */
	{
	  /* ignore blank lines */
	  if (*buff && *buff != '\r' && *buff != '\n')
	    {
	      char *p = buff;
	      /*
	       * MSVC preprocess output: #line LINENO "FILENAME"
	       * gcc output format:      # LINENO "FILENAME"
	       * remove 'line '
	       */
	      if (strncmp(buff, "#line", 5) == 0)
		{
		  p = buff + 4;
		  *p = '#';
		}
	      fputs(p, f_res);
	    }
	}
    }

  if (fclose(f_out))
    return -1;

  if (f_res != NULL && fclose(f_res))
    return -1;
  
  status = Pl_Get_Status(pid);

#ifdef DEBUG
  fprintf(stderr, "error status: %d\n", status);
#endif

  return status;
}




/*-------------------------------------------------------------------------*
 * DELETE_TEMP_FILE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Delete_Temp_File(char *name)
{
  if (no_del_temp_files)
    return;

#if 1
  if (verbose)
    fprintf(stderr, "delete %s\n", name);
#endif

  unlink(name);
}




/*-------------------------------------------------------------------------*
 * FIND_FILE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Find_File(char *file, char *suff, char *file_path, int ignore_error)
{
  char name[MAXPATHLEN];
  char **pdev;
  char *cur_end = file_path;

  sprintf(name, "%s%s", file, suff);
  if (!devel_mode)
    {
      sprintf(file_path, "%s" DIR_SEP_S "lib" DIR_SEP_S "%s", start_path, name);
      if (access(file_path, F_OK) == 0)
	return 1;
    }
  else
    for (pdev = devel_dir; *pdev; pdev++)
      {
	sprintf(file_path, "%s" DIR_SEP_S "%s" DIR_SEP_S "%s",
		start_path, *pdev, name);
	if (access(file_path, F_OK) == 0)
	  return 1;
      }


  if (!ignore_error)
    Pl_Fatal_Error("cannot locate file %s", name);

  *cur_end = '\0';
  return 0;
}




/*-------------------------------------------------------------------------*
 * FIND_SUFFIX                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Find_Suffix(char *suffixes, char *suffix)

{
  char *p;

  if ((p = strstr(suffixes, suffix)) && p[-1] == '|' &&
      p[strlen(suffix)] == '|')
    return p;

  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_FATAL_ERROR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fatal_Error(char *format, ...)
{
  FileInf *f;
  va_list arg_ptr;

  va_start(arg_ptr, format);
  vfprintf(stderr, format, arg_ptr);
  va_end(arg_ptr);

  fprintf(stderr, "\n");

  if (no_del_temp_files)
    exit(1);

  if (verbose)
    fprintf(stderr, "deleting temporary files before exit\n");

  for (f = file_lopt; f->name; f++)
    {
				/* also ok if f->type == LINK_OPTION */
      if (f->work_name1 && f->work_name1 != f->name &&
	  (file_name_out == NULL
	   || strcasecmp(f->work_name1, file_name_out) != 0))
	Delete_Temp_File(f->work_name1);

      if (f->work_name2 && f->work_name2 != f->name &&
	  (file_name_out == NULL
	   || strcasecmp(f->work_name2, file_name_out) != 0))
	Delete_Temp_File(f->work_name2);
    }

  exit(1);
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGUMENTS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Parse_Arguments(int argc, char *argv[])
{
  int i, file_name_out_i = 0;	/* init for the compiler */
  char **p, *q;
  FileInf *f = file_lopt;
  int nb_file = 0;


  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "-o") || Check_Arg(i, "--output"))
	    {
	      file_name_out_i = i;
	      if (++i >= argc)
		Pl_Fatal_Error("FILE missing after %s option", last_opt);

	      file_name_out = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "-W") || Check_Arg(i, "--wam-for-native"))
	    {
	      stop_after = FILE_PL;
	      bc_mode = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "-w") || Check_Arg(i, "--wam-for-byte-code"))
	    {
	      stop_after = FILE_PL;
	      bc_mode = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "-M") || Check_Arg(i, "--mini-assembly"))
	    {
	      stop_after = FILE_WAM;
	      bc_mode = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "-S") || Check_Arg(i, "--assembly"))
	    {
	      stop_after = FILE_MA;
	      bc_mode = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "-c") || Check_Arg(i, "--object"))
	    {
	      stop_after = FILE_ASM;
	      bc_mode = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "-F") || Check_Arg(i, "--fd-to-c"))
	    {
	      stop_after = FILE_FD;
	      bc_mode = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "--comment"))
	    {
	      Add_Last_Option(cmd_wam2ma.opt);
	      Add_Last_Option(cmd_ma2asm.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--pic") || Check_Arg(i, "--PIC") ||
	      Check_Arg(i, "-fPIC") ||
	      Check_Arg(i, "--dynamic")) 
	    {	   /* TODO pass --pic to gcc as -fPIC for C code ?, and PIE ? */
	      Add_Last_Option(cmd_ma2asm.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--temp-dir"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("PATH missing after %s option", last_opt);

	      temp_dir = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--no-del-temp-files"))
	    {
	      no_del_temp_files = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--no-demangling"))
	    {
	      no_demangle = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--version") || Check_Arg(i, "-v") ||
	      Check_Arg(i, "--verbose"))
	    {
	      Display_Copying("Prolog compiler");
	      if (Check_Arg(i, "--version"))
		exit(0);

	      verbose = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }

	  if (Check_Arg(i, "-i") || Check_Arg(i, "--include"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("FILE missing after %s option", last_opt);

	      if (access(argv[i], R_OK) != 0)
		{
		  perror(argv[i]);
		  exit(1);
		}

	      Add_Last_Option(cmd_pl2wam.opt);
	      Add_Option(i, cmd_pl2wam.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--wam-comment"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("COMMENT missing after %s option", last_opt);

	      Add_Last_Option(cmd_pl2wam.opt);
	      Add_Option(i, cmd_pl2wam.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--no-susp-warn") ||
	      Check_Arg(i, "--no-singl-warn") ||
	      Check_Arg(i, "--no-redef-error") ||
	      Check_Arg(i, "--foreign-only") ||
	      Check_Arg(i, "--no-call-c") ||
	      Check_Arg(i, "--no-inline") ||
	      Check_Arg(i, "--no-reorder") ||
	      Check_Arg(i, "--no-reg-opt") ||
	      Check_Arg(i, "--min-reg-opt") ||
	      Check_Arg(i, "--no-opt-last-subterm") ||
	      Check_Arg(i, "--fast-math") ||
	      Check_Arg(i, "--keep-void-inst") ||
	      Check_Arg(i, "--compile-msg") ||
	      Check_Arg(i, "--statistics"))
	    {
	      Add_Last_Option(cmd_pl2wam.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--c-compiler"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("FILE missing after %s option", last_opt);

	      cmd_cc.exe_name = argv[i];
	      if (strcmp(cmd_link.exe_name, EXE_FILE_LINK) == 0)
		cmd_link.exe_name = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--linker"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("FILE missing after %s option", last_opt);

	      cmd_link.exe_name = argv[i];
	      continue;
	    }

	  if (Check_Arg_MinusWx(i, "-C", "-Wc,"))
	    {
	      if (!last_is_wx_opt)
		{
		  if (++i >= argc)
		    Pl_Fatal_Error("OPTION missing after %s option", last_opt);
		  last_opt = argv[i];
		}

	      Add_Last_Option(cmd_cc.opt);
	      /* if C options specified ignore fd2c default C options */
	      cc_fd2c_flags = "";
	      continue;
	    }

	  if (Check_Arg_MinusWx(i, "-A", "-Wa,"))
	    {
	      if (!last_is_wx_opt)
		{
		  if (++i >= argc)
		    Pl_Fatal_Error("OPTION missing after %s option", last_opt);
		  last_opt = argv[i];
		}

	      Add_Last_Option(cmd_asm.opt);
	      continue;
	    }

	  if (Check_Arg(i, "--local-size"))
	    {
	      Record_Link_Warn_Option(i);
	      if (++i >= argc)
		Pl_Fatal_Error("SIZE missing after %s option", last_opt);
	      def_local_size = strtol(argv[i], &q, 10);
	      if (*q || def_local_size < 0)
		Pl_Fatal_Error("invalid stack size (%s)", argv[i]);
	      Record_Link_Warn_Option(i);
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--global-size"))
	    {
	      Record_Link_Warn_Option(i);
	      if (++i >= argc)
		Pl_Fatal_Error("SIZE missing after %s option", last_opt);
	      def_global_size = strtol(argv[i], &q, 10);
	      if (*q || def_global_size < 0)
		Pl_Fatal_Error("invalid stack size (%s)", argv[i]);
	      Record_Link_Warn_Option(i);
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--trail-size"))
	    {
	      Record_Link_Warn_Option(i);
	      if (++i >= argc)
		Pl_Fatal_Error("SIZE missing after %s option", last_opt);
	      def_trail_size = strtol(argv[i], &q, 10);
	      if (*q || def_trail_size < 0)
		Pl_Fatal_Error("invalid stack size (%s)", argv[i]);
	      Record_Link_Warn_Option(i);
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--cstr-size"))
	    {
	      Record_Link_Warn_Option(i);
	      if (++i >= argc)
		Pl_Fatal_Error("SIZE missing after %s option", last_opt);
	      def_cstr_size = strtol(argv[i], &q, 10);
	      if (*q || def_cstr_size < 0)
		Pl_Fatal_Error("invalid stack size (%s)", argv[i]);
	      Record_Link_Warn_Option(i);
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--max-atom"))
	    {
	      Record_Link_Warn_Option(i);
	      if (++i >= argc)
		Pl_Fatal_Error("SIZE missing after %s option", last_opt);
	      def_max_atom = strtol(argv[i], &q, 10);
	      if (*q || def_max_atom < 0)
		Pl_Fatal_Error("invalid max atom (%s)", argv[i]);
	      Record_Link_Warn_Option(i);
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--fixed-sizes"))
	    {
	      Record_Link_Warn_Option(i);
	      fixed_sizes = TRUE;
	      needs_stack_file = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--new-top-level"))
	    {
	      Record_Link_Warn_Option(i);
	      no_top_level = FALSE;
	      no_debugger = FALSE;
	      new_top_level = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--no-top-level"))
	    {
	      Record_Link_Warn_Option(i);
	      no_top_level = TRUE;
	      no_debugger = TRUE;
	      new_top_level = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "--gui-console"))
	    {
#ifdef W32_GUI_CONSOLE
	      Record_Link_Warn_Option(i);
	      gui_console = TRUE;
#else
	      fprintf(stderr, "Warning: Win32 GUI Console not available\n");
#endif
	      continue;
	    }

	  if (Check_Arg(i, "--no-debugger"))
	    {
	      Record_Link_Warn_Option(i);
	      no_debugger = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--min-pl-bips"))
	    {
	      Record_Link_Warn_Option(i);
	      min_pl_bips = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--min-fd-bips"))
	    {
	      Record_Link_Warn_Option(i);
	      min_fd_bips = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--min-bips") || Check_Arg(i, "--min-size"))
	    {
	      Record_Link_Warn_Option(i);
	      no_top_level = no_debugger = min_pl_bips = min_fd_bips = TRUE;
	      new_top_level = FALSE;
	      if (Check_Arg(i, "--min-size"))
		strip = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--no-pl-lib"))
	    {
	      Record_Link_Warn_Option(i);
	      no_pl_lib = no_fd_lib = TRUE;
	      no_top_level = no_debugger = min_pl_bips = min_fd_bips = TRUE;
	      new_top_level = FALSE;
	      continue;
	    }

	  if (Check_Arg(i, "--no-fd-lib"))
	    {
	      Record_Link_Warn_Option(i);
	      no_fd_lib = min_fd_bips = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--no-fd-lib-warn"))
	    {
	      Record_Link_Warn_Option(i);
	      no_fd_lib_warn = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "-s") || Check_Arg(i, "--strip"))
	    {
	      Record_Link_Warn_Option(i);
	      strip = TRUE;
	      continue;
	    }

	  if (Check_Arg_MinusWx(i, "-L", "-Wl,"))
	    {
	      Record_Link_Warn_Option(i);

	      if (!last_is_wx_opt)
		{
		  if (++i >= argc)
		    Pl_Fatal_Error("OPTION missing after %s option", last_opt);
		  last_opt = argv[i];
		  Record_Link_Warn_Option(i);
		}

	      /*
	       * The position of link flags can be relevant, we do not use
	       * append a linker flag to with Add_Last_Option(cmd_link.opt);
	       * but instead consider a linker flag as a special input file
	       */
	      f->name = f->work_name1 = last_opt;
	      f->type = LINK_OPTION;
	      nb_file_lopt++;
	      f++;
	      continue;
	    }

	  Pl_Fatal_Error("unknown option %s - try %s --help", argv[i], GPLC);
	}


      f->name = argv[i];

      if ((f->suffix = strrchr(argv[i], '.')) == NULL)
	f->suffix = argv[i] + strlen(argv[i]);

      for(q = f->suffix; q >= f->name; q--)
	if (*q == '/'
#ifdef _WIN32
	    || *q == '\\'
#endif
	    )
	  break;
      f->file_part = q + 1;

      if (Find_Suffix(PL_SUFFIX_ALTERNATE, f->suffix))
	f->type = FILE_PL;
      else if (Find_Suffix(C_SUFFIX_ALTERNATE, f->suffix))
	f->type = FILE_C;
      else if (strcmp(f->suffix, ".S") == 0)
	f->type = FILE_MA; /* handled on stage before ASM for preprocess */
      else
	{
	  f->type = FILE_LINK;
	  for (p = suffixes; *p; p++)
	    if (strcasecmp(*p, f->suffix) == 0)
	      {
		f->type = (int) (p - suffixes);
		break;
	      }
	}

      f->work_name1 = f->name;
      f->work_name2 = NULL;

      if (f->type != FILE_LINK && access(f->name, R_OK) != 0)
	{
	  perror(f->name);
	  exit(1);
	}

      nb_file_lopt++;
      nb_file++;
      f++;
    }

  if (no_top_level)
    new_top_level = FALSE;


  if (f == file_lopt)
    {
      if (verbose)
	exit(0);		/* --verbose with no files same as --version */
      else
	Pl_Fatal_Error("no input file specified");
    }

  f->name = NULL;

  if (nb_file > 1 && stop_after < FILE_LINK && file_name_out &&
      strchr(file_name_out, '%') == NULL)
    {
      fprintf(stderr, "named output file ignored with multiples output ");
      fprintf(stderr, "(or use meta-characters, e.g. %%p)\n");
      Record_Link_Warn_Option(file_name_out_i);
      Record_Link_Warn_Option(file_name_out_i + 1);
      file_name_out = NULL;
    }
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Help(void)
#define L(msg)  fprintf(stderr, "%s\n", msg)
{
  fprintf(stderr, "Usage: %s [OPTION]... FILE...\n", GPLC);
  L(" ");
  L("General options:");
  L("  -o FILE, --output FILE      set output file name (see below)");
  L("  -W, --wam-for-native        stop after producing WAM file(s)");
  L("  -w, --wam-for-byte-code     stop after producing WAM for byte-code file(s) (force --no-call-c)");
  L("  -M, --mini-assembly         stop after producing mini-assembly file(s)");
  L("  -S, --assembly              stop after producing assembly file(s)");
  L("  -F, --fd-to-c               stop after producing C file(s) from FD file(s)");
  L("  -c, --object                stop after producing object file(s)");
  L("  --temp-dir PATH             use PATH as directory for temporary files");
  L("  --no-del-temp-files         do not delete temporary files");
  L("  --no-demangling             do not demangle predicate names");
  L("  -v, --verbose               print executed commands");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L(" ");
  L("Prolog to WAM compiler options:");
  L("  -i FILE, --include FILE     include FILE at the beginning of the compilation");
  L("  --wam-comment COMMENT       emit COMMENT as a comment in the WAM file (can be repeated)");
  L("  --no-susp-warn              do not show warnings for suspicious predicates");
  L("  --no-singl-warn             do not show warnings for named singleton variables");
  L("  --no-redef-error            do not show errors for built-in redefinitions");
  L("  --foreign-only              only compile foreign/1-2 directives");
  L("  --no-call-c                 do not allow the use of fd_tell, '$call_c',...");
  L("  --no-inline                 do not inline predicates");
  L("  --no-reorder                do not reorder predicate arguments");
  L("  --no-reg-opt                do not optimize registers");
  L("  --min-reg-opt               minimally optimize registers");
  L("  --no-opt-last-subterm       do not optimize last subterm compilation");
  L("  --fast-math                 fast mathematical mode (assume integer arithmetics)");
  L("  --keep-void-inst            keep void instructions in the output file");
  L("  --compile-msg               print a compile message");
  L("  --statistics                print statistics information");
  L(" ");
  L("WAM to mini-assembly translator options:");
  L("  --comment                   include comments in the output file");
  L(" ");
  L("Mini-assembly to assembly translator options:");
  L("  --comment                   include comments in the output file");
  L("  --pic, -fPIC, --dynamic     produce position independent code (PIC) - (not yet fully implemented)");
  L(" ");
  L("C Compiler options:");
  L("  --c-compiler FILE           use FILE as C compiler/linker");
  L("  -C OPTION, -Wc,OPTION       pass OPTION to the C compiler");
  L(" ");
  L("Assembler options:");
  L("  -A OPTION, -Wa,OPTION       pass OPTION to the assembler");
  L(" ");
  L("Linker options:");
  L("  --linker FILE               use FILE as linker");
  L("  --local-size N              set default local  stack size to N Kb");
  L("  --global-size N             set default global stack size to N Kb");
  L("  --trail-size N              set default trail  stack size to N Kb");
  L("  --cstr-size N               set default cstr   stack size to N Kb");
  L("  --max-atom N                set default atom   table size to N atoms");
  L("  --fixed-sizes               do not consult environment variables at run-time");
  L("  --gui-console               link the Win32 GUI console");
  L("  --new-top-level             link the top-level main (to recognize top-level command-line options)");
  L("  --no-top-level              do not link the top-level (force --no-debugger)");
  L("  --no-debugger               do not link the Prolog/WAM debugger");
  L("  --min-pl-bips               link only used Prolog built-in predicates");
  L("  --min-fd-bips               link only used FD solver built-in predicates");
  L("  --min-bips                  same as: --no-top-level --min-pl-bips --min-fd-bips --no-debugger");
  L("  --min-size                  same as: --min-bips --strip");
  L("  --no-pl-lib                 do not look for the Prolog and FD libraries (maintenance only)");
  L("  --no-fd-lib                 do not look for the FD library (maintenance only)");
  L("  --no-fd-lib-warn            do not warn about inexistent FD library (maintenance only)");
  L("  -s, --strip                 strip the executable");
  L("  -L OPTION, -Wl,OPTION       pass OPTION to the linker");
  L("");
  L("The file name specified after --output can include meta-characters:");
  L("  %f for the whole input file name, %F same as %f without directory");
  L("  %p for the whole prefix name, %P same as %p without directory");
  L("  %s for the suffix (or empty if not specified)");
  L("  %d for the directory part (or empty if not specified)");
  L("  %c for a auto-increment counter");
  L("");
  L("Visit www.gprolog.org for more information.");
}

#undef L
