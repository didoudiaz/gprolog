/* gprolog.h generated from headers.h using cpp_headers */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : headers.h                                                       *
 * Descr.: GNU Prolog - general header file (for users)                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine_pl.h                                                     *
 * Descr.: general header file                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : gp_config.h.in                                                  *
 * Descr.: general configuration file (handled by autoconf) - header file  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999 Daniel Diaz                                          *
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
#ifndef _GP_CONFIG_H
#define _GP_CONFIG_H
#define HAVE_TERMIOS_H 1
#define NO_USE_GUI_CONSOLE 1
#define M_ix86 1
#define M_linux 1
#define M_ix86_linux 1
#define PROLOG_NAME1               "gprolog"
#define PROLOG_NAME                "GNU Prolog"
#define PROLOG_VERSION             "1.2.3"
#define PROLOG_DATE                "Sep 12 2000"
#define PROLOG_COPYRIGHT           "Copyright (C) 1999,2000 Daniel Diaz"
#define TOP_LEVEL                  "gprolog"
#define GPLC                       "gplc"
#define HEXGPLC                    "hexgplc"
#define ENV_VARIABLE               "PL_PATH"
#define M_VENDOR                   "pc"
#define M_CPU                      "i686"
#define M_OS                       "linux-gnu"
#define CC                         "gcc"
#define CFLAGS_PREFIX_REG          "-ffixed-%s"
#define CFLAGS                     "-g -Wall"
#define CFLAGS_MACHINE             "-mpentiumpro -malign-loops=2 -malign-jumps=2 -malign-functions=2"
#define LDFLAGS                    ""
#define LDLIBS                     "-lm"
#define AS                         "as"
#define STRIP                      "strip"
#define ASM_SUFFIX                 ".s"
#define OBJ_SUFFIX                 ".o"
#define EXE_SUFFIX                 ""
#define CC_OBJ_NAME_OPT            "-o "
#define CC_EXE_NAME_OPT            "-o "
#define LIB_LINEDIT                "liblinedit.a"
#define LIB_W32GUICONS             "libw32guicons.a"
#define LIB_ENGINE_PL              "libengine_pl.a"
#define LIB_BIPS_PL                "libbips_pl.a"
#define LIB_ENGINE_FD              "libengine_fd.a"
#define LIB_BIPS_FD                "libbips_fd.a"
#define SIZEOF_LONG                4
#define WORD_SIZE                  (8*SIZEOF_LONG)
/*---------------------------------*
 * Miscellaneous                   *
 *---------------------------------*/
#ifndef NO_USE_GUI_CONSOLE
#define W32_GUI_CONSOLE
#endif
#ifdef M_sparc_sunos
#define __USE_FIXED_PROTOTYPES__
#endif
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : arch_dep.h                                                      *
 * Descr.: architecture dependent features - Header file                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#ifndef _ARCH_DEP_H
#define _ARCH_DEP_H
#ifdef M_ix86_win32
#define MAXPATHLEN                 1024
#define SIGQUIT                    SIGTERM
#define fdopen                     _fdopen
#define dup                        _dup
#define dup2                       _dup2
#define getcwd                     _getcwd
#define chdir                      _chdir
#define close                      _close
#define pclose                     _pclose
#define popen                      _popen
#define pclose                     _pclose
#define getpid                     _getpid
#define strcasecmp                 _stricmp
#define strncasecmp                _strnicmp
#define unlink                     _unlink
#define tzset                      _tzset
#define access                     _access
#define F_OK                       00
#define W_OK                       02
#define R_OK                       04
#define X_OK                       F_OK
#define stat                       _stat
#ifndef S_ISDIR
#define	S_ISDIR(m)	           (((m)&_S_IFMT) == _S_IFDIR)
#define	S_ISCHR(m)                 (((m)&_S_IFMT) == _S_IFCHR)
#define	S_ISFIFO(m)	           (((m)&_S_IFMT) == _S_IFIFO)
#define	S_ISREG(m)	           (((m)&_S_IFMT) == _S_IFREG)
#endif
#define S_IRUSR                    _S_IREAD
#define S_IWUSR                    _S_IWRITE
#define S_IXUSR                    _S_IEXEC
#define DIR_SEP_S                  "\\"
#define DIR_SEP_C                  '\\'
#else
#define DIR_SEP_S                  "/"
#define DIR_SEP_C                  '/'
#endif
#if defined(M_ix86_cygwin) || defined(M_ix86_sco)
#define Set_Line_Buf(s)            setvbuf(s,NULL,_IOLBF,0)
#elif defined(M_ix86_win32)
#define Set_Line_Buf(s)            setbuf(s,NULL)
#else
#define Set_Line_Buf(s)            setlinebuf(s)
#endif
#if defined(M_ix86_sco)
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#endif
#if !defined(_WIN32) && !defined(__unix__)
#define __unix__
#endif
#endif /* !_ARCH_DEP_H */
#endif /* !_GP_CONFIG_H */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pl_params.h                                                     *
 * Descr.: parameter header file                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#define MAX_OBJECT                 1024
#define START_PRED_TBL_SIZE        4096
#define START_OPER_TBL_SIZE        1024
#define ATOM_SIZE                  16
#define MAX_ATOM                   (1<<ATOM_SIZE)	/* number of elements */
#define NB_OF_X_REGS               256
#define MAX_ARITY                  (NB_OF_X_REGS-1)
/* NB: if NB_OF_X_REGS is changed it is necessary to modify ma2asm but
   also the byte code management */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash.h                                                          *
 * Descr.: hash table management - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct
{
  char *endt;
  char *cur_t;
  char *cur_p;
}
HashScan;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
char *Hash_Alloc_Table(int tbl_size, int elem_size);
void Hash_Free_Table(char *tbl);
char *Hash_Realloc_Table(char *tbl, int new_tbl_size);
void Hash_Delete_All(char *tbl);
char *Hash_Insert(char *tbl, char *elem, int replace);
char *Hash_Find(char *tbl, long key);
char *Hash_Delete(char *tbl, long key);
char *Hash_First(char *tbl, HashScan *scan);
char *Hash_Next(HashScan *scan);
int Hash_Table_Size(char *tbl);
int Hash_Nb_Elements(char *tbl);
#ifdef DEBUG
void Hash_Check_Table(char *tbl);
#endif
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : bool.h                                                          *
 * Descr.: boolean type definition - header file                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#ifndef _BOOL_H
#define _BOOL_H
#ifdef FALSE
#    if FALSE!=0
#        error "FALSE already defined with a value != 0"
#    endif
#else
#define FALSE 0
#endif
#ifdef TRUE
#    if TRUE!=1
#        error "TRUE already defined with a value != 1"
#    endif
#else
#define TRUE 1
#endif
#ifndef Bool
typedef int Bool;
#endif
#endif /* !_BOOL_H */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : engine.h                                                        *
 * Descr.: general engine - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#ifdef NO_STACK_TEST
#   undef  M_Check_Stacks()
#   define M_Check_Stacks()
#endif
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define cpp_recurs(p,n)            p##_##n
#define Prolog_Predicate(p,n)      cpp_recurs(p,n)
#define Prolog_Prototype(p,n)      void Prolog_Predicate(p,n)();
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef long WamWord;		/* a wamword is a long (32/64 bits) */
typedef void (*CodePtr) ();	/* a code pointer is an ptr to fct  */
typedef CodePtr WamCont;	/* a continuation is a code pointer */
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef ENGINE_FILE
int os_argc;
char **os_argv;
char glob_buff[10240];
long *base_fl;			/* overwritten by foreign if present */
double *base_fd;		/* overwritten by foreign if present */
/* we need some extra space to save our registers while in a libc routine!
 * otherwise they are not correct after a signal has raised.
 */
#ifdef M_alpha_linux
unsigned long bug_reg_buffer[32];
#endif
#else
extern int os_argc;
extern char **os_argv;
extern char glob_buff[];
extern long *base_fl;
extern double *base_fd;
#ifdef M_alpha_linux
extern unsigned long bug_reg_buffer[];
#endif
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
int Start_Prolog(int argc, char *argv[]);
void Stop_Prolog(void);
void Reset_Prolog(void);
void Reset_Prolog_In_Signal(void);
void Set_Heap_Actual_Start(WamWord *heap_actual_start);
void Execute_Directive(int pl_file, int pl_line, Bool is_system,
		       CodePtr proc);
Bool Try_Execute_Top_Level(void);
void Switch_Reg_Bank(WamWord *new_reg_bank);
Bool Call_Prolog(CodePtr codep);
Bool Call_Prolog_Next_Sol(WamWord *query_b);
void Keep_Rest_For_Prolog(WamWord *query_b);
void Exit_With_Exception(void);
void Execute_A_Continuation(CodePtr codep);
#define   Goto_Predicate(p,n)   ((*Prolog_Predicate(p,n))())
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : atom.h                                                          *
 * Descr.: atom table management - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#if 1
#define OPTIM_1_CHAR_ATOM
#endif
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
	  /* Character Classification */
#define LA                         1	/* layout character      */
#define SC                         2	/* solo character        */
#define QT                         4	/* quote                 */
#define DQ                         8	/* double quote          */
#define BQ                        16	/* back quote            */
#define GR                        32	/* graphic char          */
#define PC                        64	/* punctuation character */
#define DI                       128	/* digit                 */
#define UL                       256	/* underline             */
#define CL                       512	/* capital letter        */
#define SL                      1024	/* small letter          */
#define CM                      2048	/* comment character (%) */
#define EX                      4096	/* extended character    */
#define ATOM_NIL                  1766
	  /* Atom Type */
#define IDENTIFIER_ATOM            0
#define GRAPHIC_ATOM               1
#define SOLO_ATOM                  2
#define OTHER_ATOM                 3
#define Is_Valid_Code(c)           ((unsigned) (c)-1 <256-1)	/* 1<= c <256 */
#define Is_Valid_Byte(c)           ((unsigned) (c) <256)	/* 0=< c <256 */
#define Is_Valid_Atom(a)           ((a)>=0 && (a)<MAX_ATOM && \
                                    atom_tbl[(a)].name!=NULL)
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct			/* Atom properties                */
{				/* ------------------------------ */
  unsigned length:16;		/* its length (in characters)     */
  unsigned op_mask:4;		/* operator defined for the atom  */
  unsigned type:2;		/* IDENTIFIER GRAPHIC SOLO OTHER  */
  unsigned needs_quote:1;	/* needs ' around it ?            */
  unsigned needs_scan:1;	/* contains ' or control char ?   */
}
AtomProp;
typedef struct			/* Atom information               */
{				/* ------------------------------ */
  char *name;			/* key is <name> (the string)     */
  AtomProp prop;		/* associated properties          */
}
AtomInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef ATOM_FILE
AtomInf atom_tbl[MAX_ATOM];
int nb_atom;
int atom_void;
int atom_curly_brackets;
int atom_false;
int atom_true;
int atom_end_of_file;
#ifndef OPTIM_1_CHAR_ATOM
int atom_char[256];
#endif
    /* int     char_type[256];                    see definition in atom.c */
char char_conv[256];
    /* char    escape_symbol[];                   see definition in atom.c */
    /* char    escape_char  [];                   see definition in atom.c */
#else
extern AtomInf atom_tbl[];
extern int nb_atom;
extern int atom_void;
extern int atom_curly_brackets;
extern int atom_false;
extern int atom_true;
extern int atom_end_of_file;
#ifndef OPTIM_1_CHAR_ATOM
extern int atom_char[];
#endif
extern char char_conv[];
extern int char_type[];
extern char escape_symbol[];
extern char escape_char[];
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Init_Atom(void);
int Create_Allocate_Atom(char *name);
int Create_Atom(char *name);
int Find_Atom(char *name);
int Gen_New_Atom(char *prefix, int hash);
int Find_Next_Atom(int last_atom);
#ifdef OPTIM_1_CHAR_ATOM
#define ATOM_CHAR(c)            ((int) (unsigned char) (c))
#else
#define ATOM_CHAR(c)            (atom_char[(int) (unsigned char) (c)])
#endif
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.h                                                          *
 * Descr.: predicate table management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *-----------a----------------------*/
typedef struct			/* Predicate information          */
{				/* ------------------------------ */
  long f_n;			/* key is <functor_atom,arity>    */
  int pl_file;			/* atom pl file of its definiton  */
  int pl_line;			/* pl file line of its definition */
  int prop;			/* predicate props (cf BipsPl)    */
  long *codep;			/* compiled code                  */
  long *dyn;			/* dynamic info (cf BipsPl)       */
}
PredInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef PRED_FILE
char *pred_tbl;
#else
extern char *pred_tbl;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Init_Pred(void);
PredInf *Create_Pred(int func, int arity, int pl_file, int pl_line,
		     int prop, long *codep);
PredInf *Lookup_Pred(int func, int arity);
void Delete_Pred(int func, int arity);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : misc.h                                                          *
 * Descr.: miscellaneous operations - header file                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#include <stdlib.h>
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
char *Malloc_Check(unsigned size, char *src_file, int src_line);
char *Calloc_Check(unsigned nb, unsigned size, char *src_file,
		   int src_line);
char *Realloc_Check(char *ptr, unsigned size, char *src_file, int src_line);
char *Strdup_Check(char *str, char *src_file, int src_line);
#define Malloc(size)      Malloc_Check(size,__FILE__,__LINE__)
#define Calloc(nb,size)   Calloc_Check(nb,size,__FILE__,__LINE__)
#define Realloc(ptr,size) Realloc_Check(ptr,size,__FILE__,__LINE__)
#define Free(ptr)         free(ptr)
#define Strdup(str)       Strdup_Check(str,__FILE__,__LINE__)
void Extend_Table_If_Needed(char **hash_tbl);
void Fatal_Error(char *format, ...);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : oper.h                                                          *
 * Descr.: operator table management - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define MAX_PREC                   1200
#define MAX_ARG_OF_FUNCTOR_PREC    999
#define Make_Oper_Key(a,t)         (((unsigned long) (a) << 2) | (t))
#define Atom_Of_Oper(k)            ((unsigned long) (k) >> 2)
#define Type_Of_Oper(k)            ((unsigned long) (k) & 3)
	  /* operator type */
#define PREFIX                     0
#define POSTFIX                    1
#define INFIX                      2
#define Make_Op_Mask(type)         (1<<(type))
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct			/* Operator information           */
{				/* ------------------------------ */
  long a_t;			/* key is <atom,operator type>    */
  int prec;			/* precedence of the operator     */
  int left;			/* precedence of the operator lhs */
  int right;			/* precedence of the operator rhs */
}
OperInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef OPER_FILE
char *oper_tbl;
#else
extern char *oper_tbl;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Init_Oper(void);
OperInf *Create_Oper(int atom_op, int type, int prec, int left, int right);
OperInf *Lookup_Oper(int atom_op, int type);
OperInf *Lookup_Oper_Any_Type(int atom_op);
OperInf *Delete_Oper(int atom_op, int type);
#define Check_Oper(atom_op,type)                                            \
     (atom_tbl[(atom_op)].prop.op_mask & Make_Op_Mask(type))
#define Check_Oper_Any_Type(atom_op)                                        \
     (atom_tbl[(atom_op)].prop.op_mask)
#define MAP_REG_BANK		"ebx"
#define MAP_OFFSET_H     	((NB_OF_X_REGS+0)*4)
#define MAP_OFFSET_B     	((NB_OF_X_REGS+1)*4)
#define MAP_OFFSET_TR    	((NB_OF_X_REGS+2)*4)
#define MAP_OFFSET_CP    	((NB_OF_X_REGS+3)*4)
#define MAP_OFFSET_E     	((NB_OF_X_REGS+4)*4)
#define MAP_OFFSET_CS    	((NB_OF_X_REGS+5)*4)
#define MAP_OFFSET_S     	((NB_OF_X_REGS+6)*4)
#define MAP_OFFSET_STAMP 	((NB_OF_X_REGS+7)*4)
#define MAP_OFFSET_BCI   	((NB_OF_X_REGS+8)*4)
#define CFLAGS_REGS		"-ffixed-ebx "
#define X(x)                       (reg_bank[x])
#define A(a)                       (reg_bank[a])
typedef WamWord *WamWordP;
   /*--- Begin Register Generation ---*/
register WamWord 		*reg_bank asm ("ebx");
#define H			((WamWordP)	 (reg_bank[NB_OF_X_REGS+0]))
#define B			((WamWordP)	 (reg_bank[NB_OF_X_REGS+1]))
#define TR			((WamWordP)	 (reg_bank[NB_OF_X_REGS+2]))
#define CP			((WamCont)	 (reg_bank[NB_OF_X_REGS+3]))
#define E			((WamWordP)	 (reg_bank[NB_OF_X_REGS+4]))
#define CS			((WamWordP)	 (reg_bank[NB_OF_X_REGS+5]))
#define S			((WamWordP)	 (reg_bank[NB_OF_X_REGS+6]))
#define STAMP			((WamWord)	 (reg_bank[NB_OF_X_REGS+7]))
#define BCI			((WamWord)	 (reg_bank[NB_OF_X_REGS+8]))
#define NB_OF_REGS          	9
#define NB_OF_ALLOC_REGS    	0
#define NB_OF_NOT_ALLOC_REGS	9
#define REG_BANK_SIZE       	(NB_OF_X_REGS+NB_OF_NOT_ALLOC_REGS)
#define Reg(i)			(((i)==0) ? (WamWord) H  	: \
				 ((i)==1) ? (WamWord) B  	: \
				 ((i)==2) ? (WamWord) TR 	: \
				 ((i)==3) ? (WamWord) CP 	: \
				 ((i)==4) ? (WamWord) E  	: \
				 ((i)==5) ? (WamWord) CS 	: \
				 ((i)==6) ? (WamWord) S  	: \
				 ((i)==7) ? (WamWord) STAMP	: \
				            (WamWord) BCI)
#ifdef ENGINE_FILE
       char    *reg_tbl[]=	{"H","B","TR","CP","E","CS","S","STAMP","BCI"};
#else
extern char    *reg_tbl[];
#endif
#define Save_All_Regs(buff_save)		\
    do {            				\
     buff_save[0]=(WamWord) H     ;		\
     buff_save[1]=(WamWord) B     ;		\
     buff_save[2]=(WamWord) TR    ;		\
     buff_save[3]=(WamWord) CP    ;		\
     buff_save[4]=(WamWord) E     ;		\
     buff_save[5]=(WamWord) CS    ;		\
     buff_save[6]=(WamWord) S     ;		\
     buff_save[7]=(WamWord) STAMP ;		\
     buff_save[8]=(WamWord) BCI   ;		\
    } while(0)
#define Restore_All_Regs(buff_save)		\
    do {            				\
     H     =(WamWordP)	buff_save[0];		\
     B     =(WamWordP)	buff_save[1];		\
     TR    =(WamWordP)	buff_save[2];		\
     CP    =(WamCont)	buff_save[3];		\
     E     =(WamWordP)	buff_save[4];		\
     CS    =(WamWordP)	buff_save[5];		\
     S     =(WamWordP)	buff_save[6];		\
     STAMP =(WamWord)	buff_save[7];		\
     BCI   =(WamWord)	buff_save[8];		\
    } while(0)
#define NB_OF_USED_MACHINE_REGS 1
#define Save_Machine_Regs(buff_save)		\
    do {            				\
     register long reg0 asm ("ebx");		\
     buff_save[0]=reg0;				\
    } while(0)
#define Restore_Machine_Regs(buff_save)		\
    do {            				\
     register long reg0 asm ("ebx");		\
     reg0=buff_save[0];				\
    } while(0)
   /*--- End Register Generation ---*/
   /*--- Begin Tag Generation ---*/
#define TAG_SIZE   		3
#define VALUE_SIZE 		29
#define INT        		0 
#define REF        		1 
#define FDV        		2 
#define ATM        		3 
#define FLT        		4 
#define LST        		5 
#define STC        		6 
#define MALLOC_MASK 		0
#define STACK_MASK  		0
#define Tag_Value(t,v)		(((unsigned long) (v) << 3) | (t))
#define Tag_Of(w)     		((unsigned long) (w) & 0x7)
#define UnTag_Integer(w) 	((long) (w) >> 3)
#define UnTag_Unsigned(w)	((unsigned long) (w) >> 3)
#define UnTag_Stack(w)   	((WamWord *) (((unsigned long) (w) >> 3) | STACK_MASK))
#define UnTag_Malloc(w)  	((unsigned long) (((unsigned long) (w) >> 3) | MALLOC_MASK))
#define NB_OF_TAGS       	7
#define Tag_INT(v)  		Tag_Value(INT,v)
#define UnTag_INT(w) 		UnTag_Integer(w)
#define Tag_REF(v)  		Tag_Value(REF,v)
#define UnTag_REF(w) 		UnTag_Stack(w)
#define Tag_FDV(v)  		Tag_Value(FDV,v)
#define UnTag_FDV(w) 		UnTag_Stack(w)
#define Tag_ATM(v)  		Tag_Value(ATM,v)
#define UnTag_ATM(w) 		UnTag_Integer(w)
#define Tag_FLT(v)  		Tag_Value(FLT,v)
#define UnTag_FLT(w) 		UnTag_Stack(w)
#define Tag_LST(v)  		Tag_Value(LST,v)
#define UnTag_LST(w) 		UnTag_Stack(w)
#define Tag_STC(v)  		Tag_Value(STC,v)
#define UnTag_STC(w) 		UnTag_Stack(w)
typedef enum
{
  INTEGER,
  UNSIGNED,
  STACK,
  MALLOC
}TypTag;
typedef struct
{
  char    *name;
  TypTag   type;
}InfTag;
#ifdef ENGINE_FILE
InfTag   tag_tbl[]=	{{"INT",INTEGER},
				 {"REF",STACK},
				 {"FDV",STACK},
				 {"ATM",INTEGER},
				 {"FLT",STACK},
				 {"LST",STACK},
				 {"STC",STACK}};
#else
extern InfTag   tag_tbl[];
#endif
   /*--- End Tag Generation ---*/
#define KBytes_To_Wam_Words(kb)    ((1024*kb+sizeof(WamWord)-1)/sizeof(WamWord))
#define Wam_Words_To_KBytes(ww)    (ww*sizeof(WamWord)/1024)
#define Local_Top                  ((B>=E) ? B : E)
   /*--- Begin Stack Generation ---*/
#define NB_OF_STACKS 		4
#define Trail_Stack       	(stk_tbl[0].stack)
#define Trail_Size        	(stk_tbl[0].size)
#define Trail_Offset(adr) 	((WamWord *)(adr)-Trail_Stack)
#define Trail_Used_Size   	Trail_Offset(TR)
#define Cstr_Stack       	(stk_tbl[1].stack)
#define Cstr_Size        	(stk_tbl[1].size)
#define Cstr_Offset(adr) 	((WamWord *)(adr)-Cstr_Stack)
#define Cstr_Used_Size   	Cstr_Offset(CS)
#define Global_Stack       	(stk_tbl[2].stack)
#define Global_Size        	(stk_tbl[2].size)
#define Global_Offset(adr) 	((WamWord *)(adr)-Global_Stack)
#define Global_Used_Size   	Global_Offset(H)
#define Local_Stack       	(stk_tbl[3].stack)
#define Local_Size        	(stk_tbl[3].size)
#define Local_Offset(adr) 	((WamWord *)(adr)-Local_Stack)
#define Local_Used_Size   	Local_Offset(Local_Top)
#define Stack_Top(s)       	(((s)==0) ? TR : ((s)==1) ? CS : ((s)==2) ? H : Local_Top)
typedef struct
{
  char    *name;
  char    *env_var_name;
  long    *p_def_size;
  int      default_size; 	/* in WamWords */
  int      size;         	/* in WamWords */
  WamWord *stack;
}InfStack;
#ifdef ENGINE_FILE
    /* these variables can be overwritten by top_comp.c (see stack size file) */
long def_trail_size;
long def_cstr_size;
long def_global_size;
long def_local_size;
long fixed_sizes;
InfStack  stk_tbl[]=	{{"trail","TRAILSZ",&def_trail_size,524288,0,NULL},
			 {"cstr","CSTRSZ",&def_cstr_size,524288,0,NULL},
			 {"global","GLOBALSZ",&def_global_size,1048576,0,NULL},
			 {"local","LOCALSZ",&def_local_size,817152,0,NULL}};
#else
extern long def_trail_size;
extern long def_cstr_size;
extern long def_global_size;
extern long def_local_size;
extern long fixed_sizes;
extern InfStack stk_tbl[];
#endif
   /*--- End Stack Generation ---*/
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine + Compiler                                        *
 * File  : machine1.h                                                      *
 * Descr.: machine dependent features - Header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#include <stdio.h>
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define M_OS_UNIX                  0
#define M_OS_WINDOWS               1
#define M_OS_WINDOWS_NT            2
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef MACHINE1_FILE
int m_os_type;
char m_architecture[32];
char m_os_version[256];
#else
extern int m_os_type;
extern char m_architecture[];
extern char m_os_version[];
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Init_Machine1(void);
char **M_Create_Shell_Command(char *cmd);
char **M_Cmd_Line_To_Argv(char *cmd, int *argc);
int M_Shell(char *cmd);
int M_Spawn(char *arg[]);
int M_Spawn_Redirect(char *arg[], int detach,
		     FILE **f_in, FILE **f_out, FILE **f_err);
int M_Get_Status(int pid);
#define   DBGPRINTF             printf
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : machine.h                                                       *
 * Descr.: machine dependent features - Header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
void Init_Machine(void);
void M_Allocate_Stacks(void);
char *M_Sys_Err_String(int err_no);
long M_User_Time(void);
long M_System_Time(void);
long M_Real_Time(void);
void M_Randomize(void);
void M_Set_Seed(int n);
int M_Get_Seed(void);
int M_Random_Integer(int n);
double M_Random_Float(double n);
char *M_Host_Name_From_Name(char *host_name);
char *M_Host_Name_From_Adr(char *host_address);
char *M_Get_Working_Dir(void);
Bool M_Set_Working_Dir(char *path);
char *M_Absolute_Path_Name(char *src);
#ifdef M_ix86_win32
int getpagesize(void);
int Is_Win32_SEGV(void *exp);
void SIGSEGV_Handler(void);
#endif
/*---------------------------------*
 * Register Definitions            *
 *---------------------------------*/
#if defined(M_sparc_sunos)
#    define M_USED_REGS            {"g6","g7",0}
#elif defined(M_sparc_solaris)
#    define M_USED_REGS            {"g6","g7",0}
#elif defined(M_mips_irix)
#define M_USED_REGS                {"$16","$17","$18","$19","$20",\
                                    "$21","$22","$23",0}
#elif defined(M_alpha_linux)
#    define M_USED_REGS            {"$9","$10","$11","$12","$13","$14",0}
#elif defined(M_alpha_osf)
#    define M_USED_REGS            {"$9","$10","$11","$12","$13","$14",0}
#elif defined(M_ix86_linux)   || defined(M_ix86_sco) || \
      defined(M_ix86_solaris) || defined(M_ix86_cygwin)
#    define M_USED_REGS            {"ebx",0}
#elif defined(M_powerpc_linux)
#    define M_USED_REGS            {"15","20",0}
#else
#    define M_USED_REGS            {0}
#endif
/*---------------------------------*
 * Stacks Management               *
 *---------------------------------*/
#if defined(M_sparc_sunos) || defined(M_sparc_solaris) || \
    defined(M_ix86_linux)  || defined(M_powerpc_linux) || \
    defined(M_ix86_sco)    || defined(M_ix86_solaris)  || \
    defined(M_mips_irix)   || \
    defined(M_ix86_win32)
#   define M_USE_MMAP
#   define M_MMAP_HIGH_ADR         0x0ffffff0
#   define M_MMAP_HIGH_ADR_ALT     0x1ffffff0
#   define M_Check_Stacks()
#elif defined(M_alpha_osf) || defined(M_alpha_linux)
#   define M_USE_MMAP
#   define M_MMAP_HIGH_ADR         0x3f800000000ULL
#   define M_Check_Stacks()
#else
#   define M_USE_MALLOC
#   define M_Check_Stacks()        M_Check_Magic_Words()
#endif
#ifdef M_USE_MALLOC
#define M_USE_MAGIC_NB_TO_DETECT_STACK_NAME
void M_Check_Magic_Words(void);
#endif
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.h                                                     *
 * Descr.: object chaining management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define OBJ_CHAIN_MAGIC_1          0xdeadbeef
#define OBJ_CHAIN_MAGIC_2          0x12345678
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
#ifndef _MSC_VER
typedef struct objchain *PObjChain;
typedef struct objchain
{
  long magic1;
  long magic2;
  PObjChain *next;
  void (*fct_init) ();
}
ObjChain;
#endif
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Find_Linked_Objects(void);
void New_Object(void (*fct_exec_system) (), void (*fct_exec_user) ());
#ifdef OBJ_INIT
static void (OBJ_INIT) ();
#ifndef _MSC_VER
extern ObjChain *obj_chain_stop;
static ObjChain obj_chain_start =
  { OBJ_CHAIN_MAGIC_1, OBJ_CHAIN_MAGIC_2, &obj_chain_stop, OBJ_INIT };
static ObjChain *obj_chain_stop = &obj_chain_start;
#else
#pragma data_seg(".INIT$b")
static long obj_chain_start = (long) OBJ_INIT;
#pragma data_seg()
#endif /* _MSC_VER */
#endif /* OBJ_INIT */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_inst.h                                                      *
 * Descr.: WAM instruction implementation - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define NOT_A_WAM_WORD             Tag_Value(-1,-1)
#define NIL_WORD                   Tag_Value(ATM,ATOM_NIL)
	  /* Read/Write Modes */
	  /* if S==NULL iff we are in the write mode */
#define WRITE_MODE                 NULL
	  /* Environment Frame */
#define ENVIR_STATIC_SIZE          3
#define CPE(e)                     ((WamCont)   (e[-1]))
#define BCIE(e)                    ((WamWord)   (e[-2]))
#define EE(e)                      ((WamWord *) (e[-3]))
#define Y(e,y)                     ((WamWord)   (e[-4-(y)]))
#define ENVIR_NAMES                {"CPE","BCIE","EE"}
	  /* Choice Point Frame */
#define CHOICE_STATIC_SIZE         8
#define ALTB(b)                    ((CodePtr)   (b[-1]))
#define CPB(b)                     ((WamCont)   (b[-2]))
#define BCIB(b)                    ((WamWord)   (b[-3]))
#define EB(b)                      ((WamWord *) (b[-4]))
#define BB(b)                      ((WamWord *) (b[-5]))
#define HB(b)                      ((WamWord *) (b[-6]))
#define TRB(b)                     ((WamWord *) (b[-7]))
#define CSB(b)                     ((WamWord *) (b[-8]))
#define AB(b,a)                    ((WamWord)   (b[-9-(a)]))
#define CHOICE_NAMES               {"ALTB","CPB","BCIB","EB","BB","HB",     \
                                    "TRB","CSB"}
	  /* Wam Objects Manipulation */
	  /* Trail Tags */
#define NB_OF_TRAIL_TAGS           4
#define TUV                        0	/* Trail Unbound Variable   */
#define TOV                        1	/* Trail One Value          */
#define TMV                        2	/* Trail Multiple Values    */
#define TFC                        3	/* Trail for Function Call  */
#define TRAIL_TAG_NAMES            {"TUV","TOV","TMV","TFC"}
#define Trail_Tag_Value(t,v)       ((unsigned long) (v) | (t))
#define Trail_Tag_Of(w)            ((unsigned long) (w) & 0x3)
#define Trail_Value_Of(w)          ((unsigned long) (w) & (~0x3))
	  /* Functor/arity */
#define Functor_Arity(f,n)         (((n) << ATOM_SIZE) + (f))
#define Functor_Of(word)           ((word) & (MAX_ATOM-1))
#define Arity_Of(word)             ((word) >> ATOM_SIZE)
#define Dont_Separate_Tag(t)       ((t)==FDV)
	  /* Unbound Variables */
#define Make_Self_Ref(adr)         (Tag_Value(REF,adr))
	  /* Atom */
	  /* Integer */
#define INT_GREATEST_VALUE         ((long) ((1L<<(WORD_SIZE-TAG_SIZE-1))-1))
#define INT_LOWEST_VALUE           ((long) ((-INT_GREATEST_VALUE)-1))
	  /* List */
#define OFFSET_CAR                 0
#define Car(adr)                   (((WamWord *) adr)[OFFSET_CAR])
#define Cdr(adr)                   (((WamWord *) adr)[OFFSET_CAR+1])
	  /* Structure */
#define OFFSET_ARG                 1
#define Functor(adr)               (Functor_Of(Functor_And_Arity(adr)))
#define Arity(adr)                 (Arity_Of(Functor_And_Arity(adr)))
#define Functor_And_Arity(adr)     (((WamWord *) (adr))[0])
#define Arg(adr,i)                 (((WamWord *) (adr))[OFFSET_ARG+i])
							/* i in 0..arity-1 */
	  /* Stacks */
#define Global_Push(word)          (*H++=(WamWord) (word))
#define Global_Pop                 (*--H)
#define Trail_Push(word)           (*TR++=(WamWord) (word))
#define Trail_Pop                  (*--TR)
#define Is_A_Local_Adr(adr)        ((adr)>=Local_Stack)
	  /* CP management */
#ifdef M_sparc
#define Adjust_CP(cp)              ((WamCont) ((unsigned long) (cp)-8))
#define UnAdjust_CP(cp)            ((WamCont) ((unsigned long) (cp)+8))
#else
#define Adjust_CP(p)               ((WamCont) (p))
#define UnAdjust_CP(cp)            (cp)
#endif
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct			/* Switch item information         */
{				/* ------------------------------- */
  long key;			/* key: atm, int (if no_opt), f/n  */
  CodePtr codep;		/* compiled code pointer if static */
}
SwtInf;
typedef SwtInf *SwtTbl;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
SwtTbl Create_Swt_Table(int size);
void Create_Swt_Atm_Element(SwtTbl t, int size, int atom, CodePtr codep);
void Create_Swt_Stc_Element(SwtTbl t, int size, int func, int arity,
			    CodePtr codep);
Bool Get_Atom(int atom, WamWord start_word);
Bool Get_Integer(long n, WamWord start_word);
Bool Get_Float(double n, WamWord start_word);
Bool Get_Nil(WamWord start_word);
Bool Get_List(WamWord start_word);
Bool Get_Structure(int func, int arity, WamWord start_word);
WamWord Put_X_Variable(void);
WamWord Put_Y_Variable(WamWord *y_adr);
WamWord Put_Unsafe_Value(WamWord start_word);
WamWord Put_Atom(int atom);
WamWord Put_Integer(long n);
WamWord Put_Float(double n);
WamWord Put_Nil(void);
WamWord Put_List(void);
WamWord Put_Structure(int func, int arity);
WamWord Unify_Variable(void);
void Unify_Void(int n);
Bool Unify_Value(WamWord start_word);
Bool Unify_Local_Value(WamWord start_word);
Bool Unify_Atom(int atom);
Bool Unify_Integer(long n);
Bool Unify_Nil(void);
Bool Unify_List(void);
Bool Unify_Structure(int func, int arity);
void Allocate(int n);
void Deallocate(void);
CodePtr Switch_On_Term(CodePtr c_var, CodePtr c_atm, CodePtr c_int,
		       CodePtr c_lst, CodePtr c_stc);
CodePtr Switch_On_Atom(SwtTbl t, int size);
long Switch_On_Integer(void);
CodePtr Switch_On_Structure(SwtTbl t, int size);
void Load_Cut_Level(WamWord *word_adr);
void Cut(WamWord b_word);
void Global_Push_Float(double n);
double Obtain_Float(WamWord *adr);
void Create_Choice_Point(CodePtr codep_alt, int arity);
void Update_Choice_Point(CodePtr codep_alt, int arity);
void Delete_Choice_Point(int arity);
void Untrail(WamWord *low_adr);
WamWord Make_Copy_Of_Word(int tag, WamWord word);
Bool Unify(WamWord start_u_word, WamWord start_v_word);
Bool Unify_Occurs_Check(WamWord start_u_word, WamWord start_v_word);
/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/
	  /*---------------------------------------------------------------*
           * Deref dereferences the word start_word and sets :             *
           *   word : dereferenced word                                    *
           *   tag  : dereferenced word's tag                              *
           *   adr  : only if tag==REF then adr==value==self adress        *
           *---------------------------------------------------------------*/
#define Deref(start_word,word,tag,adr)                                      \
    do {                                                                    \
     WamWord *working_adr;                                                  \
     word=start_word;                                                       \
     adr=NULL;                                                              \
     for(;;)                                                                \
        {                                                                   \
         tag=Tag_Of(word);                                                  \
         if (tag!=REF || (working_adr=UnTag_REF(word))==adr)                \
             break;                                                         \
                                                                            \
         adr=working_adr;                                                   \
         word=*adr;                                                         \
        }                                                                   \
    } while(0)
	  /* Trail Stack Management */
#define Word_Needs_Trailing(adr)           ((adr)<(WamWord *) HB(B) ||      \
                                            (Is_A_Local_Adr(adr) && (adr)<B))
#define Bind_UV(adr,word)                                                   \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_UV(adr);                                                     \
     *(adr)=(word);                                                         \
    } while(0)
#define Bind_OV(adr,word)                                                   \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_OV(adr);                                                     \
     *(adr)=(word);                                                         \
    } while(0)
#define Bind_MV(adr,nb,real_adr)                                            \
    do {                                                                    \
     if (Word_Needs_Trailing(adr))                                          \
         Trail_MV(adr,nb);                                                  \
     Mem_Word_Cpy(adr,real_adr,nb)                                          \
    } while(0)
#define Trail_UV(adr)                                                       \
     Trail_Push(Trail_Tag_Value(TUV,adr))
#define Trail_OV(adr)                                                       \
    do {                                                                    \
     Trail_Push(*(adr));                                                    \
     Trail_Push(Trail_Tag_Value(TOV,adr));                                  \
    } while(0)
#define Trail_MV(adr,nb)                                                    \
    do {                                                                    \
     Mem_Word_Cpy(TR,adr,nb);                                               \
     TR+=nb;                                                                \
     Trail_Push(nb);                                                        \
     Trail_Push(Trail_Tag_Value(TMV,adr));                                  \
    } while(0)
#define Trail_FC(fct)                                                       \
     Trail_Push(Trail_Tag_Value(TFC,fct))
#define Delete_Last_Choice_Point()         B=BB(B)
	    /* Globalization */
#define Globalize_Local_Unbound_Var(adr)                                    \
    do {                                                                    \
     WamWord word;                                                          \
                                                                            \
     Bind_UV(adr,Tag_Value(REF,H));                                         \
     word=Make_Self_Ref(H);                                                 \
     Global_Push(word);                                                     \
    } while(0)
#define Mem_Word_Cpy(dst,src,nb)                                            \
    do {                                                                    \
     register long *s=(long *) (src);                                       \
     register long *d=(long *) (dst);                                       \
     register int   counter=(nb);                                           \
                                                                            \
     while(counter--)                                                       \
         *d++ = *s++;                                                       \
    } while(0)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.h                                                      *
 * Descr.: FD interface for Prolog engine - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef IF_NO_FD_FILE
void (*fd_init_solver) ();	/* overwritten by FD if present */
void (*fd_reset_solver) ();
Bool (*fd_unify_with_integer) ();
Bool (*fd_unify_with_fd_var) ();
int (*fd_variable_size) ();
int (*fd_copy_variable) ();
char *(*fd_variable_to_string) ();
#else
extern void (*fd_init_solver) ();
extern void (*fd_reset_solver) ();
extern Bool (*fd_unify_with_integer) ();
extern Bool (*fd_unify_with_fd_var) ();
extern int (*fd_variable_size) ();
extern int (*fd_copy_variable) ();
extern char *(*fd_variable_to_string) ();
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Fd_Init_Solver(void);
void Fd_Reset_Solver(void);
#define Fd_Unify_With_Integer(f,n) ((*fd_unify_with_integer)(f,n))
#define Fd_Unify_With_Fd_Var(f1,f2)((*fd_unify_with_fd_var)(f1,f2))
#define Fd_Variable_Size(f)        ((*fd_variable_size)(f))
#define Fd_Copy_Variable(dst_adr,f)((*fd_copy_variable)(dst_adr,f))
#define Fd_Variable_To_String(f)   ((*fd_variable_to_string)(f))
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bips_pl.h                                                       *
 * Descr.: general header file                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : b_params.h                                                      *
 * Descr.: parameter header file                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#define MAX_STREAM                 256
#define MAX_VAR_NAME_LENGTH        256
#define MAX_VAR_IN_TERM            2048
#define MAX_SYS_VARS               256
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : inl_protos.h                                                    *
 * Descr.: inline predicate prototypes - header file                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
	  /* from type_inl_c.c */
Bool Blt_Var(WamWord x);
Bool Blt_Non_Var(WamWord x);
Bool Blt_Atom(WamWord x);
Bool Blt_Integer(WamWord x);
Bool Blt_Float(WamWord x);
Bool Blt_Number(WamWord x);
Bool Blt_Atomic(WamWord x);
Bool Blt_Compound(WamWord x);
Bool Blt_Callable(WamWord x);
Bool Blt_Fd_Var(WamWord x);
Bool Blt_Non_Fd_Var(WamWord x);
Bool Blt_Generic_Var(WamWord x);
Bool Blt_Non_Generic_Var(WamWord x);
Bool Blt_List(WamWord x);
Bool Blt_Partial_List(WamWord x);
Bool Blt_List_Or_Partial_List(WamWord x);
	  /* from term_inl_c.c */
Bool Blt_Term_Eq(WamWord x, WamWord y);
Bool Blt_Term_Neq(WamWord x, WamWord y);
Bool Blt_Term_Lt(WamWord x, WamWord y);
Bool Blt_Term_Lte(WamWord x, WamWord y);
Bool Blt_Term_Gt(WamWord x, WamWord y);
Bool Blt_Term_Gte(WamWord x, WamWord y);
Bool Blt_Compare(WamWord cmp_word, WamWord x, WamWord y);
Bool Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word);
Bool Blt_Functor(WamWord term_word, WamWord functor_word,
		 WamWord arity_word);
Bool Blt_Univ(WamWord term_word, WamWord list_word);
	  /* from g_var_inl_c.c */
Bool Blt_G_Assign(WamWord x, WamWord y);
Bool Blt_G_Assignb(WamWord x, WamWord y);
Bool Blt_G_Link(WamWord x, WamWord y);
Bool Blt_G_Read(WamWord x, WamWord y);
Bool Blt_G_Array_Size(WamWord x, WamWord y);
	  /* from arith_inl_c.c */
void Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr);
void Math_Load_Value(WamWord start_word, WamWord *word_adr);
WamWord Fct_Fast_Neg(int x);
WamWord Fct_Fast_Inc(int x);
WamWord Fct_Fast_Dec(int x);
WamWord Fct_Fast_Add(int x, int y);
WamWord Fct_Fast_Sub(int x, int y);
WamWord Fct_Fast_Mul(int x, int y);
WamWord Fct_Fast_Div(int x, int y);
WamWord Fct_Fast_Rem(int x, int y);
WamWord Fct_Fast_Mod(int x, int y);
WamWord Fct_Fast_And(int x, int y);
WamWord Fct_Fast_Or(int x, int y);
WamWord Fct_Fast_Xor(int x, int y);
WamWord Fct_Fast_Not(int x);
WamWord Fct_Fast_Shl(int x, int y);
WamWord Fct_Fast_Shr(int x, int y);
WamWord Fct_Fast_Abs(int x);
WamWord Fct_Fast_Sign(int x);
WamWord Fct_Neg(WamWord x);
WamWord Fct_Inc(WamWord x);
WamWord Fct_Dec(WamWord x);
WamWord Fct_Add(WamWord x, WamWord y);
WamWord Fct_Sub(WamWord x, WamWord y);
WamWord Fct_Mul(WamWord x, WamWord y);
WamWord Fct_Div(WamWord x, WamWord y);
WamWord Fct_Float_Div(WamWord x, WamWord y);
WamWord Fct_Rem(WamWord x, WamWord y);
WamWord Fct_Mod(WamWord x, WamWord y);
WamWord Fct_And(WamWord x, WamWord y);
WamWord Fct_Or(WamWord x, WamWord y);
WamWord Fct_Xor(WamWord x, WamWord y);
WamWord Fct_Not(WamWord x);
WamWord Fct_Shl(WamWord x, WamWord y);
WamWord Fct_Shr(WamWord x, WamWord y);
WamWord Fct_Abs(WamWord x);
WamWord Fct_Sign(WamWord x);
WamWord Fct_Pow(WamWord x, WamWord y);
WamWord Fct_Sqrt(WamWord x);
WamWord Fct_Atan(WamWord x);
WamWord Fct_Cos(WamWord x);
WamWord Fct_Sin(WamWord x);
WamWord Fct_Exp(WamWord x);
WamWord Fct_Log(WamWord x);
WamWord Fct_Float(WamWord x);
WamWord Fct_Ceiling(WamWord x);
WamWord Fct_Floor(WamWord x);
WamWord Fct_Round(WamWord x);
WamWord Fct_Truncate(WamWord x);
WamWord Fct_Float_Fract_Part(WamWord x);
WamWord Fct_Float_Integ_Part(WamWord x);
Bool Blt_Fast_Eq(WamWord x, WamWord y);
Bool Blt_Fast_Neq(WamWord x, WamWord y);
Bool Blt_Fast_Lt(WamWord x, WamWord y);
Bool Blt_Fast_Lte(WamWord x, WamWord y);
Bool Blt_Fast_Gt(WamWord x, WamWord y);
Bool Blt_Fast_Gte(WamWord x, WamWord y);
Bool Blt_Eq(WamWord x, WamWord y);
Bool Blt_Neq(WamWord x, WamWord y);
Bool Blt_Lt(WamWord x, WamWord y);
Bool Blt_Lte(WamWord x, WamWord y);
Bool Blt_Gt(WamWord x, WamWord y);
Bool Blt_Gte(WamWord x, WamWord y);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : c_supp.h                                                        *
 * Descr.: C interface support - header file                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
long Rd_Integer_Check(WamWord start_word);
long Rd_Integer(WamWord start_word);
long Rd_Positive_Check(WamWord start_word);
long Rd_Positive(WamWord start_word);
double Rd_Float_Check(WamWord start_word);
double Rd_Float(WamWord start_word);
double Rd_Number_Check(WamWord start_word);
double Rd_Number(WamWord start_word);
int Rd_Atom_Check(WamWord start_word);
int Rd_Atom(WamWord start_word);
int Rd_Boolean_Check(WamWord start_word);
int Rd_Boolean(WamWord start_word);
int Rd_Char_Check(WamWord start_word);
int Rd_Char(WamWord start_word);
int Rd_In_Char_Check(WamWord start_word);
int Rd_In_Char(WamWord start_word);
int Rd_Code_Check(WamWord start_word);
int Rd_Code(WamWord start_word);
int Rd_In_Code_Check(WamWord start_word);
int Rd_In_Code(WamWord start_word);
int Rd_Byte_Check(WamWord start_word);
int Rd_Byte(WamWord start_word);
int Rd_In_Byte_Check(WamWord start_word);
int Rd_In_Byte(WamWord start_word);
char *Rd_String_Check(WamWord start_word);
char *Rd_String(WamWord start_word);
char *Rd_Chars_Check(WamWord start_word);
char *Rd_Chars(WamWord start_word);
char *Rd_Codes_Check(WamWord start_word);
char *Rd_Codes(WamWord start_word);
int Rd_Chars_Str_Check(WamWord start_word, char *str);
int Rd_Chars_Str(WamWord start_word, char *str);
int Rd_Codes_Str_Check(WamWord start_word, char *str);
int Rd_Codes_Str(WamWord start_word, char *str);
WamWord *Rd_List_Check(WamWord start_word);
WamWord *Rd_List(WamWord start_word);
int Rd_Proper_List_Check(WamWord start_word, WamWord *arg);
int Rd_Proper_List(WamWord start_word, WamWord *arg);
WamWord *Rd_Compound_Check(WamWord start_word, int *func, int *arity);
WamWord *Rd_Compound(WamWord start_word, int *func, int *arity);
WamWord *Rd_Callable_Check(WamWord start_word, int *func, int *arity);
WamWord *Rd_Callable(WamWord start_word, int *func, int *arity);
void Check_For_Un_Integer(WamWord start_word);
void Check_For_Un_Positive(WamWord start_word);
void Check_For_Un_Float(WamWord start_word);
void Check_For_Un_Number(WamWord start_word);
void Check_For_Un_Atom(WamWord start_word);
void Check_For_Un_Boolean(WamWord start_word);
void Check_For_Un_Char(WamWord start_word);
void Check_For_Un_In_Char(WamWord start_word);
void Check_For_Un_Code(WamWord start_word);
void Check_For_Un_In_Code(WamWord start_word);
void Check_For_Un_Byte(WamWord start_word);
void Check_For_Un_In_Byte(WamWord start_word);
void Check_For_Un_String(WamWord start_word);
void Check_For_Un_Chars(WamWord start_word);
void Check_For_Un_Codes(WamWord start_word);
void Check_For_Un_List(WamWord start_word);
void Check_For_Un_Compound(WamWord start_word);
void Check_For_Un_Callable(WamWord start_word);
void Check_For_Un_Variable(WamWord start_word);
Bool Un_Integer_Check(long value, WamWord start_word);
Bool Un_Integer(long value, WamWord start_word);
Bool Un_Positive_Check(long value, WamWord start_word);
Bool Un_Positive(long value, WamWord start_word);
Bool Un_Float_Check(double value, WamWord start_word);
Bool Un_Float(double value, WamWord start_word);
Bool Un_Number_Check(double value, WamWord start_word);
Bool Un_Number(double value, WamWord start_word);
Bool Un_Atom_Check(int value, WamWord start_word);
Bool Un_Atom(int value, WamWord start_word);
Bool Un_Boolean_Check(int value, WamWord start_word);
Bool Un_Boolean(int value, WamWord start_word);
Bool Un_Char_Check(int value, WamWord start_word);
Bool Un_Char(int value, WamWord start_word);
Bool Un_In_Char_Check(int value, WamWord start_word);
Bool Un_In_Char(int value, WamWord start_word);
Bool Un_Code_Check(int value, WamWord start_word);
Bool Un_Code(int value, WamWord start_word);
Bool Un_In_Code_Check(int value, WamWord start_word);
Bool Un_In_Code(int value, WamWord start_word);
Bool Un_Byte_Check(int value, WamWord start_word);
Bool Un_Byte(int value, WamWord start_word);
Bool Un_In_Byte_Check(int value, WamWord start_word);
Bool Un_In_Byte(int value, WamWord start_word);
Bool Un_String_Check(char *value, WamWord start_word);
Bool Un_String(char *value, WamWord start_word);
Bool Un_Chars_Check(char *value, WamWord start_word);
Bool Un_Chars(char *value, WamWord start_word);
Bool Un_Codes_Check(char *value, WamWord start_word);
Bool Un_Codes(char *value, WamWord start_word);
Bool Un_List_Check(WamWord *arg, WamWord start_word);
Bool Un_List(WamWord *arg, WamWord start_word);
Bool Un_Proper_List_Check(int n, WamWord *arg, WamWord start_word);
Bool Un_Proper_List(int n, WamWord *arg, WamWord start_word);
Bool Un_Compound_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);
Bool Un_Compound(int func, int arity, WamWord *arg, WamWord start_word);
Bool Un_Callable_Check(int func, int arity, WamWord *arg,
		       WamWord start_word);
Bool Un_Callable(int func, int arity, WamWord *arg, WamWord start_word);
WamWord Mk_Integer(long value);
WamWord Mk_Positive(long value);
WamWord Mk_Float(double value);
WamWord Mk_Number(double value);
WamWord Mk_Atom(int value);
WamWord Mk_Boolean(int value);
WamWord Mk_Char(int value);
WamWord Mk_In_Char(int value);
WamWord Mk_Code(int value);
WamWord Mk_In_Code(int value);
WamWord Mk_Byte(int value);
WamWord Mk_In_Byte(int value);
WamWord Mk_String(char *value);
WamWord Mk_Chars(char *value);
WamWord Mk_Codes(char *value);
WamWord Mk_List(WamWord *arg);
WamWord Mk_Proper_List(int n, WamWord *arg);
WamWord Mk_Compound(int func, int arity, WamWord *arg);
WamWord Mk_Callable(int func, int arity, WamWord *arg);
WamWord Mk_Variable(void);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : foreign_supp.h                                                  *
 * Descr.: foreign interface support - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define PL_RECOVER                 0
#define PL_CUT                     1
#define PL_KEEP_FOR_PROLOG         2
#define PL_FAILURE                 FALSE
#define PL_SUCCESS                 TRUE
#define PL_EXCEPTION               2
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct
{
  Bool is_var;
  Bool unify;
  union
  {
    long l;
    char *s;
    double d;
  }
  value;
}
FIOArg;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef FOREIGN_SUPP_FILE
int foreign_bkt_counter;
char *foreign_bkt_buffer;
#else
extern int foreign_bkt_counter;
extern char *foreign_bkt_buffer;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Foreign_Create_Choice(CodePtr codep_alt, int arity, int choice_size);
void Foreign_Update_Choice(CodePtr codep_alt, int arity, int choice_size);
CodePtr Foreign_Jump_Ret(CodePtr codep);
FIOArg *Foreign_Rd_IO_Arg(int arg_long, WamWord start_word,
			  long (*rd_fct) (), int fio_arg_index);
Bool Foreign_Un_IO_Arg(int arg_long, Bool (*un_fct) (), FIOArg *fa,
		       WamWord start_word);
void Emit_Syntax_Error(char *file_name, int err_line, int err_col,
		       char *err_msg);
int Type_Of_Term(WamWord start_word);
void Pl_Exec_Continuation(int func, int arity, WamWord *arg_adr);
int Pl_Query_Start(int func, int arity, WamWord *arg_adr, Bool recoverable);
int Pl_Query_Next_Solution(void);
void Pl_Query_End(int op);
WamWord Pl_Get_Exception(void);
#define Get_Choice_Counter()   foreign_bkt_counter
#define Get_Choice_Buffer(t)   ((t) foreign_bkt_buffer)
#define No_More_Choice()       Delete_Last_Choice_Point()
#define PlTerm                 WamWord
#define PLV                    REF
#define Atom_Name(a)           (atom_tbl[(a)].name)
#define Atom_Length(a)         (atom_tbl[(a)].prop.length)
#define Atom_Needs_Quote(a)    (atom_tbl[(a)].prop.needs_quote)
#define Atom_Needs_Scan(a)     (atom_tbl[(a)].prop.needs_scan)
#define atom_nil               ATOM_NIL
#define Stream_Pointer(s)      (stm_tbl+(s))
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_supp.h                                                     *
 * Descr.: predicate management support - header file                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
					  /* if modified -> modif wam2ma.c */
#define MASK_PRED_NATIVE_CODE      1
#define MASK_PRED_DYNAMIC          2
#define MASK_PRED_PUBLIC           4
#define MASK_PRED_BUILTIN          8
#define MASK_PRED_BUILTIN_FD       16
#define MASK_PRED_ANY_BUILTIN      (MASK_PRED_BUILTIN | MASK_PRED_BUILTIN_FD)
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
char *Detect_If_Aux_Name(int func);
int Father_Pred_Of_Aux(int func, int *father_arity);
int Pred_Without_Aux(int func, int arity, int *arity1);
int Make_Aux_Name(int func, int arity, int aux_nb);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_supp.h                                                     *
 * Descr.: term support - header file                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef TERM_SUPP_FILE
WamWord pi_name_word;
WamWord pi_arity_word;
long glob_dico_var[MAX_VAR_IN_TERM];	/* a general purpose dico */
#else
extern WamWord pi_name_word;
extern WamWord pi_arity_word;
extern long glob_dico_var[];
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
int Term_Compare(WamWord start_u_word, WamWord start_v_word);
Bool Is_List(WamWord start_word);
Bool Is_Partial_List(WamWord start_word);
Bool Is_List_Or_Partial(WamWord start_word);
void Treat_Vars_Of_Term(WamWord start_word, Bool generic_var,
			void (*fct) ());
int List_Length(WamWord start_word);
int Term_Size(WamWord start_word);
void Copy_Term(WamWord *dst_adr, WamWord *src_adr);
void Copy_Contiguous_Term(WamWord *dst_adr, WamWord *src_adr);
int Get_Pred_Indicator(WamWord pred_indic_word, Bool must_be_ground,
		       int *arity);
char *Float_To_String(double d);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream_supp.h                                                   *
 * Descr.: stream support - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#include <stdio.h>
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define STREAM_PB_SIZE             8	/* push back buffer size */
#define STREAM_MODE_READ           0
#define STREAM_MODE_WRITE          1
#define STREAM_MODE_APPEND         2
#define STREAM_EOF_ACTION_ERROR    0
#define STREAM_EOF_ACTION_EOF_CODE 1
#define STREAM_EOF_ACTION_RESET    2
#define STREAM_BUFFERING_NONE      0
#define STREAM_BUFFERING_LINE      1
#define STREAM_BUFFERING_BLOCK     2
#define STREAM_EOF_NOT             0
#define STREAM_EOF_AT              1
#define STREAM_EOF_PAST            2
					 /* values for Get_Stream_Or_Alias */
#define STREAM_CHECK_VALID         0	/* simply a valid stream */
#define STREAM_CHECK_EXIST         1	/* valid and exist */
#define STREAM_CHECK_INPUT         2	/* valid, exist and mode=input  */
#define STREAM_CHECK_OUTPUT        3	/* valid, exist and mode=output */
#define STREAM_FCT_UNDEFINED       ((StmFct) (-1))	/* for optional fct */
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct			/* Stream properties              */
{				/* ------------------------------ */
  unsigned mode:2;		/* see STREAM_MODE_xxx defs       */
  unsigned input:1;		/* is it an input  stream ?       */
  unsigned output:1;		/* is it an output stream ?       */
  unsigned text:1;		/* is it a text stream . (or bin) */
  unsigned reposition:1;	/* can it be repositioned ?       */
  unsigned eof_action:2;	/* see STREAM_EOF_ACTION_xxx defs */
  unsigned buffering:2;		/* see STREAM_BUFFERING_xxx defs  */
  unsigned tty:1;		/* is it a tty ?                  */
  unsigned special_close:1;	/* does it need a special close ? */
  unsigned other:8;		/* other prop (1,2,3=term_streams */
}				/*             4=socket_stream)   */
StmProp;
typedef struct			/* Push Back stack                */
{				/* ------------------------------ */
  int buff[STREAM_PB_SIZE];	/* the buffer                     */
  int *ptr;			/* pointer into the buffer        */
  int nb_elems;			/* # of elements in the buffer    */
}
PbStk;
typedef int (*StmFct) ();	/* generic type for file fctions */
typedef struct			/* Stream information             */
{				/* ------------------------------ */
  int atom_file_name;		/* atom associated to filename    */
  long file;			/* accessor (FILE *,TTYInf *)!=0  */
  StmProp prop;			/* assoctiated properties         */
  /* ----- Basic I/O functions ---- */
  StmFct fct_getc;		/* get char function (mandatory)  */
  StmFct fct_putc;		/* put char function (mandatory)  */
  StmFct fct_flush;		/* flush    function (optional)   */
  StmFct fct_close;		/* close    function (optional)   */
  StmFct fct_tell;		/* tell     function (optional)   */
  StmFct fct_seek;		/* seek     function (optional)   */
  StmFct fct_clearerr;		/* clearerr function (optional)   */
  /* ------ Read information  ----- */
  Bool eof_reached;		/* eof char has been read ?       */
  PbStk pb_char;		/* character push back stack      */
  /* ---- Position information  --- */
  int char_count;		/* character read count           */
  int line_count;		/* line read count                */
  int line_pos;			/* line position                  */
  PbStk pb_line_pos;		/* line position push back stack  */
}
StmInf;
typedef struct			/* Alias information              */
{				/* ------------------------------ */
  long atom;			/* atom of the alias (the key)    */
  int stm;			/* associated stream              */
}
AliasInf;
typedef struct			/* String Stream information      */
{				/* ------------------------------ */
  char *buff;			/* the I/O buffer                 */
  char *ptr;			/* current position into the buff */
  Bool buff_alloc_size;		/* mallocated size (iff output)   */
}
StrSInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef STREAM_SUPP_FILE
StmInf stm_tbl[MAX_STREAM + 1];	/* +1 for global term stream */
int stm_last_used = -1;
char *alias_tbl;
WamWord last_input_sora;
WamWord last_output_sora;
int stm_stdin;
int stm_stdout;
int stm_input;
int stm_output;
int stm_top_level_input;
int stm_top_level_output;
int stm_debugger_input;
int stm_debugger_output;
char *le_prompt;
int atom_glob_stream_alias;
int atom_user_input;
int atom_user_output;
int atom_top_level_input;
int atom_top_level_output;
int atom_debugger_input;
int atom_debugger_output;
int atom_read;
int atom_write;
int atom_append;
int atom_reposition;
int atom_stream_position;
int atom_text;
int atom_binary;
int atom_error;
int atom_eof_code;
int atom_reset;
int atom_none;
int atom_line;
int atom_block;
int atom_not;
int atom_at;
int atom_past;
int atom_bof;
int atom_current;
int atom_eof;
#else
extern StmInf stm_tbl[];
extern int stm_last_used;
extern char *alias_tbl;
extern WamWord last_input_sora;
extern WamWord last_output_sora;
extern int stm_stdin;
extern int stm_stdout;
extern int stm_input;
extern int stm_output;
extern int stm_top_level_input;
extern int stm_top_level_output;
extern int stm_debugger_input;
extern int stm_debugger_output;
extern char *le_prompt;
extern int atom_glob_stream_alias;
extern int atom_user_input;
extern int atom_user_output;
extern int atom_top_level_input;
extern int atom_top_level_output;
extern int atom_debugger_input;
extern int atom_debugger_output;
extern int atom_read;
extern int atom_write;
extern int atom_append;
extern int atom_reposition;
extern int atom_stream_position;
extern int atom_text;
extern int atom_binary;
extern int atom_error;
extern int atom_eof_code;
extern int atom_reset;
extern int atom_none;
extern int atom_line;
extern int atom_block;
extern int atom_not;
extern int atom_at;
extern int atom_past;
extern int atom_bof;
extern int atom_current;
extern int atom_eof;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
StmProp Get_Stream_Mode(WamWord mode_word, Bool only_rw, char *open_str);
int Add_Stream(int atom_file_name, long file, StmProp prop,
	       StmFct fct_getc, StmFct fct_putc,
	       StmFct fct_flush, StmFct fct_close,
	       StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr);
void Delete_Stream(int stm);
int Find_Stream_By_Alias(int atom_alias);
Bool Add_Alias_To_Stream(int atom_alias, int stm);
void Reassign_Alias(int atom_alias, int stm);
void Del_Aliases_Of_Stream(int stm);
void Flush_All_Streams(void);
void Set_Stream_Buffering(int stm);
int Get_Stream_Or_Alias(WamWord sora_word, int test_mask);
void Check_Stream_Type(int stm, Bool check_text, Bool for_input);
FILE *File_Star_Of_Stream(int stm);
int File_Number_Of_Stream(int stm);
int Stream_Getc(StmInf *pstm);
int Stream_Getc_No_Echo(StmInf *pstm);
void Stream_Ungetc(int c, StmInf *pstm);
int Stream_Peekc(StmInf *pstm);
char *Stream_Gets(char *str, int size, StmInf *pstm);
void Stream_Putc(int c, StmInf *pstm);
int Stream_Puts(char *str, StmInf *pstm);
int Stream_Printf(StmInf *pstm, char *format, ...);
void Stream_Flush(StmInf *pstm);
int Stream_Close(StmInf *pstm);
int Stream_End_Of_Stream(StmInf *pstm);
void Stream_Get_Position(StmInf *pstm, int *offset,
			 int *char_count, int *line_count, int *line_pos);
int Stream_Set_Position(StmInf *pstm, int whence, int offset,
			int char_count, int line_count, int line_pos);
int Stream_Set_Position_LC(StmInf *pstm, int line_count, int line_pos);
int Add_Str_Stream(Bool use_global, char *buff);
void Delete_Str_Stream(int stm);
char *Term_Write_Str_Stream(int stm);
#define PB_Init(pb)          pb.ptr=pb.buff, pb.nb_elems=0;
#define PB_Is_Empty(pb)      (pb.nb_elems==0)
#define PB_Push(pb,elem)                                                    \
    do {                                                                    \
     *(pb.ptr)=(elem);                                                      \
     if (pb.ptr!=pb.buff+STREAM_PB_SIZE-1)                                  \
         pb.ptr++;                                                          \
      else                                                                  \
         pb.ptr=pb.buff;                                                    \
     if (pb.nb_elems<STREAM_PB_SIZE)                                        \
         pb.nb_elems++;                                                     \
    } while(0)
#define PB_Pop(pb,elem)                                                     \
    do {                                                                    \
     if (pb.ptr!=pb.buff)                                                   \
         pb.ptr--;                                                          \
      else                                                                  \
         pb.ptr=pb.buff+STREAM_PB_SIZE-1;                                   \
     (elem)=*pb.ptr;                                                        \
     pb.nb_elems--;                                                         \
    } while(0)
#define PB_Top(pb,elem)                                                     \
    do {                                                                    \
     if (pb.ptr!=pb.buff)                                                   \
         (elem)=pb.ptr[-1];                                                 \
      else                                                                  \
         (elem)=pb.buff[STREAM_PB_SIZE-1];                                  \
    } while(0)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : error_supp.h                                                    *
 * Descr.: Prolog errors support - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef ERROR_SUPP_FILE
int type_atom;
int type_atomic;
int type_byte;
int type_callable;
int type_character;
int type_compound;
int type_evaluable;
int type_float;			/* for arithmetic */
int type_boolean;		/* for setarg/4 */
int type_in_byte;
int type_in_character;
int type_integer;
int type_list;
int type_number;
int type_predicate_indicator;
int type_variable;
int type_fd_variable;		/* for FD */
int type_fd_evaluable;		/* for FD */
int type_fd_bool_evaluable;	/* for FD */
int domain_character_code_list;
int domain_close_option;
int domain_flag_value;
int domain_io_mode;
int domain_non_empty_list;
int domain_not_less_than_zero;
int domain_operator_priority;
int domain_operator_specifier;
int domain_prolog_flag;
int domain_read_option;
int domain_source_sink;
int domain_stream;
int domain_stream_option;
int domain_stream_or_alias;
int domain_stream_position;
int domain_stream_property;
int domain_write_option;
int domain_term_stream_or_alias;	/* for term_streams */
int domain_g_array_index;	/* for g_vars */
int domain_stream_seek_method;	/* for seek/4 */
int domain_format_control_sequence;	/* for format/2-3 */
int domain_os_path;		/* for absolute_file_name/2 */
int domain_os_file_permission;	/* for file_permission/2 */
int domain_selectable_item;	/* for select_read/3 */
int domain_date_time;		/* for os_interf */
#ifndef NO_USE_SOCKETS
int domain_socket_domain;	/* for sockets */
int domain_socket_address;	/* for sockets */
#endif
int existence_procedure;
int existence_source_sink;
int existence_stream;
int permission_operation_access;
int permission_operation_close;
int permission_operation_create;
int permission_operation_input;
int permission_operation_modify;
int permission_operation_open;
int permission_operation_output;
int permission_operation_reposition;
int permission_type_binary_stream;
int permission_type_flag;
int permission_type_operator;
int permission_type_past_end_of_stream;
int permission_type_private_procedure;
int permission_type_static_procedure;
int permission_type_source_sink;
int permission_type_stream;
int permission_type_text_stream;
int representation_character;
int representation_character_code;
int representation_in_character_code;
int representation_max_arity;
int representation_max_integer;
int representation_min_integer;
int representation_too_many_variables;
						    /* for Copy_Term(),... */
int evluation_float_overflow;
int evluation_int_overflow;
int evluation_undefined;
int evluation_underflow;
int evluation_zero_divisor;
int resource_too_many_open_streams;	/* for streams */
int resource_print_object_not_linked;	/* for print and format */
int resource_too_big_fd_constraint;	/* for FD */
#else
extern int type_atom;
extern int type_atomic;
extern int type_byte;
extern int type_callable;
extern int type_character;
extern int type_compound;
extern int type_evaluable;
extern int type_float;		/* for arithmetic */
extern int type_boolean;	/* for setarg/4 */
extern int type_in_byte;
extern int type_in_character;
extern int type_integer;
extern int type_list;
extern int type_number;
extern int type_predicate_indicator;
extern int type_variable;
extern int type_fd_variable;	/* for FD */
extern int type_fd_evaluable;	/* for FD */
extern int type_fd_bool_evaluable;	/* for FD */
extern int domain_character_code_list;
extern int domain_close_option;
extern int domain_flag_value;
extern int domain_io_mode;
extern int domain_non_empty_list;
extern int domain_not_less_than_zero;
extern int domain_operator_priority;
extern int domain_operator_specifier;
extern int domain_prolog_flag;
extern int domain_read_option;
extern int domain_source_sink;
extern int domain_stream;
extern int domain_stream_option;
extern int domain_stream_or_alias;
extern int domain_stream_position;
extern int domain_stream_property;
extern int domain_write_option;
extern int domain_term_stream_or_alias;	/* for term_streams */
extern int domain_g_array_index;	/* for g_vars */
extern int domain_stream_seek_method;	/* for seek/4 */
extern int domain_format_control_sequence;	/* for format/2-3 */
extern int domain_os_path;	/* for absolute_file_name/2 */
extern int domain_os_file_permission;	/* for file_permission/2 */
extern int domain_selectable_item;	/* for select_read/3 */
extern int domain_date_time;	/* for os_interf */
#ifndef NO_USE_SOCKETS
extern int domain_socket_domain;	/* for sockets */
extern int domain_socket_address;	/* for sockets */
#endif
extern int existence_procedure;
extern int existence_source_sink;
extern int existence_stream;
extern int permission_operation_access;
extern int permission_operation_close;
extern int permission_operation_create;
extern int permission_operation_input;
extern int permission_operation_modify;
extern int permission_operation_open;
extern int permission_operation_output;
extern int permission_operation_reposition;
extern int permission_type_binary_stream;
extern int permission_type_flag;
extern int permission_type_operator;
extern int permission_type_past_end_of_stream;
extern int permission_type_private_procedure;
extern int permission_type_static_procedure;
extern int permission_type_source_sink;
extern int permission_type_stream;
extern int permission_type_text_stream;
extern int representation_character;
extern int representation_character_code;
extern int representation_in_character_code;
extern int representation_max_arity;
extern int representation_max_integer;
extern int representation_min_integer;
extern int representation_too_many_variables;
						    /* for Copy_Term(),... */
extern int evluation_float_overflow;
extern int evluation_int_overflow;
extern int evluation_undefined;
extern int evluation_underflow;
extern int evluation_zero_divisor;
extern int resource_too_many_open_streams;	/* for streams */
extern int resource_print_object_not_linked;	/* for print and format */
extern int resource_too_big_fd_constraint;	/* for FD */
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Set_Bip_Name_2(WamWord func_word, WamWord arity_word);
void Set_C_Bip_Name(char *func_str, int arity);
void Unset_C_Bip_Name(void);
int Get_Current_Bip(int *arity);
void Set_Last_Syntax_Error(char *file_name, int err_line, int err_col,
			   char *err_msg);
void Syntax_Error(int flag_value);
void Unknown_Pred_Error(int func, int arity);
void Os_Error(void);
void Pl_Err_Instantiation(void);
void Pl_Err_Type(int atom_type, WamWord term);
void Pl_Err_Domain(int atom_domain, WamWord term);
void Pl_Err_Existence(int atom_object, WamWord term);
void Pl_Err_Permission(int atom_oper, int atom_perm, WamWord term);
void Pl_Err_Representation(int atom_flag);
void Pl_Err_Evaluation(int atom_error);
void Pl_Err_Resource(int atom_resource);
void Pl_Err_Syntax(int atom_error);
void Pl_Err_System(int atom_error);
#define Os_Test_Error(tst)  \
      do { if (tst) { Os_Error(); return FALSE; } } while(0)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : scan_supp.h                                                     *
 * Descr.: scanner support - header file                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define SCAN_BIG_BUFFER            10240
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef enum
{
  TOKEN_VARIABLE,
  TOKEN_INTEGER,
  TOKEN_FLOAT,
  TOKEN_NAME,
  TOKEN_STRING,
  TOKEN_BACK_QUOTED,
  TOKEN_PUNCTUATION,
  TOKEN_IMMEDIAT_OPEN,
  TOKEN_FULL_STOP,
  TOKEN_END_OF_FILE,
  TOKEN_EXTENDED
}
TypTok;
typedef struct
{
  TypTok type;
  char name[SCAN_BIG_BUFFER];	/* for VARIABLE NAME STRING BACK_QUOTED */
  int punct;			/* for PUNCTUATION                      */
  long int_num;			/* for INTEGER                          */
  double float_num;		/* for FLOAT                            */
  int line;			/* source line of the token             */
  int col;			/* source column of the token           */
}
TokInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef SCAN_SUPP_FILE
TokInf token;
#else
extern TokInf token;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
int Scan_Peek_Char(StmInf *pstm, Bool convert);
char *Scan_Token(StmInf *pstm, Bool comma_is_punct);
void Recover_After_Error(StmInf *pstm);
char *Scan_Next_Atom(StmInf *pstm);
char *Scan_Next_Number(StmInf *pstm, Bool integer_only);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : parse_supp.h                                                    *
 * Descr.: parser support - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define PARSE_END_OF_TERM_DOT      0
#define PARSE_END_OF_TERM_EOF      1
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef struct			/* Parsed variable information    */
{				/* ------------------------------ */
  char name[MAX_VAR_NAME_LENGTH];	/* variable name                */
  WamWord word;			/* associated WAM word            */
  Bool named;			/* has it a name ?                */
  int nb_of_uses;		/* occurrence counter             */
}
InfVar;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef PARSE_SUPP_FILE
int parse_end_of_term;
InfVar parse_dico_var[MAX_VAR_IN_TERM];
int parse_nb_var;
int last_read_line;
int last_read_col;
#else
extern int parse_end_of_term;
extern InfVar parse_dico_var[];
extern int parse_nb_var;
extern int last_read_line;
extern int last_read_col;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
WamWord Read_Term(StmInf *pstm);
WamWord Read_Atom(StmInf *pstm);
WamWord Read_Integer(StmInf *pstm);
WamWord Read_Number(StmInf *pstm);
WamWord Read_Token(StmInf *pstm);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : write_supp.h                                                    *
 * Descr.: term writing support - header file                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define WRITE_QUOTED                1
#define WRITE_IGNORE_OP             2
#define WRITE_NUMBER_VARS           4
#define WRITE_NAME_VARS             8
#define WRITE_SPACE_ARGS           16
#define WRITE_PORTRAYED            32
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef WRITE_SUPP_FILE
int last_writing;
#else
extern int last_writing;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
void Write_Term(StmInf *pstm, int depth, int prec, int mask,
		WamWord term_word);
void Write_Simple(WamWord term_word);
void Write_A_Char(StmInf *pstm, int c);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_supp.h                                                     *
 * Descr.: Prolog flag and system variable support - header file           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define FLAG_BOUNDED               0	/* flags defining integer type */
#define FLAG_MAX_INTEGER           1
#define FLAG_MIN_INTEGER           2
#define FLAG_ROUNDING_FCT          3
#define FLAG_CHAR_CONVERSION       4	/* other flags */
#define FLAG_DEBUG                 5
#define FLAG_MAX_ARITY             6
#define FLAG_UNKNOWN               7
#define FLAG_DOUBLE_QUOTES         8
#define FLAG_SYNTAX_ERROR          9	/* non ISO flags */
#define FLAG_OS_ERROR              10
#define FLAG_MAX_ATOM              11
#define FLAG_MAX_STREAM            12
#define FLAG_MAX_UNGET             13
#define FLAG_SINGLETON_WARNING     14
#define FLAG_STRICT_ISO            15
#define FLAG_PROLOG_NAME           16
#define FLAG_PROLOG_VERSION        17
#define FLAG_PROLOG_DATE           18
#define FLAG_PROLOG_COPYRIGHT      19
#define NB_OF_FLAGS                20
#define FLAG_VALUE_ERROR           0	/* same order as in read.pl */
#define FLAG_VALUE_WARNING         1
#define FLAG_VALUE_FAIL            2
#define FLAG_DOUBLE_QUOTES_CODES   0
#define FLAG_DOUBLE_QUOTES_CHARS   1
#define FLAG_DOUBLE_QUOTES_ATOM    2
#define Char_Conversion(c)         ((Flag_Value(FLAG_CHAR_CONVERSION) &&    \
                                    Is_Valid_Code(c)) ? char_conv[c] : (c))
#define SYS_VAR_OPTION_MASK         (sys_var[0])
#define SYS_VAR_WRITE_DEPTH         (sys_var[1])
#define SYS_VAR_SYNTAX_ERROR_ACTON  (sys_var[1])
#define SYS_VAR_FD_BCKTS            (sys_var[3])
#define SYS_VAR_PRINT_STM           (sys_var[6])
#define SYS_VAR_TOP_LEVEL           (sys_var[10])
#define SYS_VAR_LINEDIT             (sys_var[12])
#define SYS_VAR_DEBUGGER            (sys_var[13])
#define Flag_Value(flag)            (sys_var[200+(flag)])
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef FLAG_C_FILE
long sys_var[MAX_SYS_VARS];
#else
extern long sys_var[];
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
/*-------------------------------------------------------------------------*
 * System variables (C and Prolog) - bank description                      *
 *                                                                         *
 *   0: temporary (e.g. masks for option lists (open/read/write)).         *
 *   1: temporary (e.g. depth in write).                                   *
 *   2: temporary (e.g. reorder in FD labeling).                           *
 *   3: temporary (e.g. backtracks counter in FD labeling).                *
 *                                                                         *
 *   6: pseudo-permanent current print stream (for get_print_stream/1)     *
 *                                                                         *
 *   7: permanent catch handler.                                           *
 *   8: permanent catch ball.                                              *
 *                                                                         *
 *  10: permanent top level depth (for top-level and stop/abort).          *
 *  11: permanent top level handler (B level) for abort and stop.          *
 *  12: permanent: is linedit present ?                                    *
 *  13: permanent: is the debugger present ?                               *
 *                                                                         *
 * 200..: some prolog flag values.                                         *
 *-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dynam_supp.h                                                    *
 * Descr.: dynamic predicate support - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define DYN_ALT_FCT_FOR_TEST       0
#define DYN_ALT_FCT_FOR_JUMP       1
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef long (*ScanFct) ();
typedef unsigned long DynStamp;
typedef struct dynpinf *DynPInfP;
typedef struct dyncinf *DynCInfP;
typedef struct dyncinf		/* Dynamic clause information     */
{				/* ------------------------------ */
  long *seq_chain_f;		/* sequential chain fwd(->nxt ch) */
  long *seq_chain_b;		/* sequential chain bwd(->nxt ch) */
  long *ind_chain_f;		/* indexical  chain fwd(->nxt ch) */
  long *ind_chain_b;		/* indexical  chain bwd(->nxt ch) */
  DynPInfP dyn;			/* associated dyn inf (redundant) */
  int cl_no;			/* clause number                  */
  DynStamp erase_stamp;		/* FFF...F if not erased or stamp */
  DynCInfP next_erased_cl;	/* pointer to next erased clause  */
  unsigned *byte_code;		/* bc pointer (NULL=interpreted)  */
  int term_size;		/* size of the term of the clause */
  WamWord term_word;		/* clause [Head|Body]=<LST,adr+1> */
  WamWord head_word;		/* adr+1 = Car = clause term Head */
  WamWord body_word;		/* adr+2 = Cdr = clause term Body */
}
DynCInf;
typedef struct			/* Dynamic switch item info       */
{				/* ------------------------------ */
  long key;			/* key: atm, int, f/n             */
  long *ind_chain;		/* indexical chain (->1st ch)     */
}
DSwtInf;
typedef struct dynpinf		/* Dynamic predicate information  */
{				/* ------------------------------ */
  long *seq_chain;		/* sequential chain    (->1st ch) */
  long *var_ind_chain;		/* index if 1st arg=VAR(->1st ch) */
  char *atm_htbl;		/* index if 1st arg=ATM(->htable) */
  char *int_htbl;		/* index if 1st arg=INT(->htable) */
  long *lst_ind_chain;		/* index if 1st arg=LST(->1st ch) */
  char *stc_htbl;		/* index if 1st arg=STC(->htable) */
  int arity;			/* arity (redundant but faster)   */
  int count_a;			/* next clause nb for asserta     */
  int count_z;			/* next clause nb for assertz     */
  DynCInfP first_erased_cl;	/* 1st erased clause NULL if none */
  DynPInfP next_dyn_with_erase;	/* next dyn with some erased cl.  */
}
DynPInf;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
DynCInf *Add_Dynamic_Clause(WamWord head_word, WamWord body_word,
			    Bool asserta, Bool check_perm);
void Delete_Dynamic_Clause(DynCInf *clause);
PredInf *Update_Dynamic_Pred(int func, int arity, int what_to_do);
DynCInf *Scan_Dynamic_Pred(int owner_func, int owner_arity,
			   DynPInf *dyn, WamWord first_arg_word,
			   ScanFct alt_fct, int alt_fct_type,
			   int alt_info_size, WamWord *alt_info);
int Scan_Choice_Point_Pred(WamWord *b, int *arity);
void Copy_Clause_To_Heap(DynCInf *clause, WamWord *head_word,
			 WamWord *body_word);
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : callinf_supp.h                                                  *
 * Descr.: meta call info support - header file                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
void Call_Info_Bip_Name_1(WamWord call_info_word);
#define Call_Info(f,a,dc)          ((Functor_Arity(f,a)<<1)|dc)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : bc_supp.h                                                       *
 * Descr.: byte-Code support - header file                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef BC_SUPP_FILE
unsigned *byte_code;
int byte_len;
#else
extern unsigned *byte_code;
extern int byte_len;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
WamCont BC_Emulate_Pred(int func, DynPInf *dyn);
#ifndef NO_USE_FD_SOLVER
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : engine_fd.h                                                     *
 * Descr.: general header file                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.h                                                      *
 * Descr.: FD Range Implementation - header file                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
typedef unsigned long VecWord;
typedef VecWord *Vector;
typedef struct			/* Ranges are always handled through pointers */
{
  Bool extra_cstr;
  int min;
  int max;
  Vector vec;
}
Range;
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_hook_range.h                                                 *
 * Descr.: FD Range Implementation - customizable header file              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#ifdef FD_RANGE_FILE
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_inst.h                                                       *
 * Descr.: FD instruction implementation - header file                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
	  /* FD Variable Frame */
#define FD_VARIABLE_FRAME_SIZE     (OFFSET_RANGE+RANGE_SIZE+CHAINS_SIZE)
#define FD_INT_VARIABLE_FRAME_SIZE (OFFSET_RANGE+RANGE_SIZE)
#define OFFSET_RANGE               5
#define RANGE_SIZE                 (2+(sizeof(Range)/sizeof(WamWord)))
#define OFFSET_CHAINS              (OFFSET_RANGE+RANGE_SIZE)
#define CHAINS_SIZE                8
#define FD_Tag_Value(fdv_adr)      (((WamWord *) fdv_adr)[0])
#define FD_INT_Date(fdv_adr)       (((WamWord *) fdv_adr)[1])
#define Queue_Date_At_Push(fdv_adr)(((WamWord *) fdv_adr)[2])
#define Queue_Propag_Mask(fdv_adr) (((WamWord *) fdv_adr)[3])
#define Queue_Next_Fdv_Adr(fdv_adr)(((WamWord *) fdv_adr)[4])
#define Range_Stamp(fdv_adr)       (((WamWord *) fdv_adr)[OFFSET_RANGE])
#define Nb_Elem(fdv_adr)           (((WamWord *) fdv_adr)[OFFSET_RANGE+1])
#define Range(fdv_adr)             ((Range *) ((WamWord *) fdv_adr+OFFSET_RANGE+2))
#define Chains_Stamp(fdv_adr)      (((WamWord *) fdv_adr)[OFFSET_CHAINS])
#define Nb_Cstr(fdv_adr)           (((WamWord *) fdv_adr)[OFFSET_CHAINS+1])
#define Chains_Mask(fdv_adr)       (((WamWord *) fdv_adr)[OFFSET_CHAINS+2])
#define Chain_Min(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+3])
#define Chain_Max(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+4])
#define Chain_Min_Max(fdv_adr)     (((WamWord *) fdv_adr)[OFFSET_CHAINS+5])
#define Chain_Dom(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+6])
#define Chain_Val(fdv_adr)         (((WamWord *) fdv_adr)[OFFSET_CHAINS+7])
	  /* Shorthands for Range(fdv_adr)'s fields */
#define Extra_Cstr(fdv_adr)        (Range(fdv_adr)->extra_cstr)
#define Min(fdv_adr)               (Range(fdv_adr)->min)
#define Max(fdv_adr)               (Range(fdv_adr)->max)
#define Vec(fdv_adr)               (Range(fdv_adr)->vec)
	  /* Chain / Propagation Mask */
#define CHAIN_NB_MIN               0
#define CHAIN_NB_MAX               1
#define CHAIN_NB_MIN_MAX           2
#define CHAIN_NB_DOM               3
#define CHAIN_NB_VAL               4
#define MASK_EMPTY                 0
#define MASK_MIN                   1
#define MASK_MAX                   2
#define MASK_MIN_MAX               4
#define MASK_DOM                   8
#define MASK_VAL                   16
#define Has_Min_Mask(mask)         ((mask) & MASK_MIN)
#define Has_Max_Mask(mask)         ((mask) & MASK_MAX)
#define Has_Min_Max_Mask(mask)     ((mask) & MASK_MIN_MAX)
#define Has_Dom_Mask(mask)         ((mask) & MASK_DOM)
#define Has_Val_Mask(mask)         ((mask) & MASK_VAL)
#define Set_Min_Mask(mask)         ((mask) |= MASK_MIN)
#define Set_Max_Mask(mask)         ((mask) |= MASK_MAX)
#define Set_Min_Max_Mask(mask)     ((mask) |= MASK_MIN_MAX)
#define Set_Dom_Mask(mask)         ((mask) |= MASK_DOM)
#define Set_Val_Mask(mask)         ((mask) |= MASK_VAL)
	  /* Chain Record Frame */
#define CHAIN_RECORD_FRAME_SIZE    2
#define CF_Pointer(rec_adr)        ((WamWord *) (rec_adr[0]))
#define Next_Chain(rec_adr)        ((WamWord *) (rec_adr[1]))
	  /* Constraint Frame */
#define CONSTRAINT_FRAME_SIZE      3
#define OFFSET_OF_OPTIM_POINTER    1	/* this offset must corresponds to */
#define AF_Pointer(cf)             ((WamWord *)  (cf[0]))
#define Optim_Pointer(cf)          ((WamWord *)  (cf[1]))	/* this cell */
#define Cstr_Address(cf)           ((long (*)()) (cf[2]))
	  /* Miscellaneous */
#define ENV_VAR_VECTOR_MAX         "VECTORMAX"
#define DEFAULT_VECTOR_MAX         127
#define Fd_Variable_Is_Ground(fdv_adr) (Tag_Of(FD_Tag_Value(fdv_adr))==INT)
#define math_min(x,y)              ((x) <= (y) ? (x) : (y))
#define math_max(x,y)              ((x) >= (y) ? (x) : (y))
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef FD_INST_FILE
WamWord DATE;
WamWord *TP;
WamWord vec_size;
WamWord vec_max_integer;
#else
extern WamWord DATE;
extern WamWord *TP;
extern WamWord vec_size;
extern WamWord vec_max_integer;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
WamWord *Fd_Prolog_To_Fd_Var(WamWord arg_word, Bool pl_var_ok);
Range *Fd_Prolog_To_Range(WamWord list_word);
int Fd_Prolog_To_Value(WamWord arg_word);
WamWord *Fd_Prolog_To_Array_Int(WamWord list_word);
WamWord *Fd_Prolog_To_Array_Any(WamWord list_word);
WamWord *Fd_Prolog_To_Array_Fdv(WamWord list_word, Bool pl_var_ok);
void Fd_List_Int_To_Range(Range *range, WamWord list_word);
WamWord *Fd_New_Variable(void);
WamWord *Fd_New_Bool_Variable(void);
WamWord *Fd_New_Int_Variable(int n);
WamWord *Fd_Create_C_Frame(long (*cstr_fct) (), WamWord *AF,
			   WamWord *fdv_adr, Bool optim2);
void Fd_Add_Dependency(WamWord *fdv_adr, int chain_nb, WamWord *CF);
void Fd_Add_List_Dependency(WamWord *array, int chain_nb, WamWord *CF);
void Fd_Before_Add_Cstr(void);
Bool Fd_After_Add_Cstr(void);
void Fd_Stop_Constraint(WamWord *CF);
Bool Fd_Tell_Value(WamWord *fdv_adr, int n);
Bool Fd_Tell_Not_Value(WamWord *fdv_adr, int n);
Bool Fd_Tell_Int_Range(WamWord *fdv_adr, Range *range);
Bool Fd_Tell_Interv_Interv(WamWord *fdv_adr, int min, int max);
Bool Fd_Tell_Range_Range(WamWord *fdv_adr, Range *range);
void Fd_Display_Extra_Cstr(WamWord *fdv_adr);
void Fd_Init_Solver0(void);
void Fd_Reset_Solver0(void);
Bool Fd_Assign_Value(WamWord *fdv_adr, int n);
Bool Fd_Unify_With_Integer0(WamWord *fdv_adr, int n);
Bool Fd_Unify_With_Fd_Var0(WamWord *fdv_adr1, WamWord *fdv_adr2);
Bool Fd_Use_Vector(WamWord *fdv_adr);
Bool Fd_Check_For_Bool_Var(WamWord x_word);
int Fd_Deref_Check_Fd_Var(WamWord *fdv_word);
int Fd_Variable_Size0(WamWord *fdv_adr);
int Fd_Copy_Variable0(WamWord *dst_adr, WamWord *fdv_adr);
char *Fd_Variable_To_String0(WamWord *fdv_adr);
#endif
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define RANGE_TOP_STACK            CS
#define INTERVAL_MAX_INTEGER       ((int)((1L<<(32-TAG_SIZE-1))-1))	/* only 32 bits */
	  /* Default definitions (if not defined in fd_hook_range.h) */
#ifndef WORD_SIZE
#   define WORD_SIZE                32
#endif
#if WORD_SIZE==32
#   define WORD_SIZE_BITS          5
#else
#   define WORD_SIZE_BITS          6
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
int Least_Significant_Bit(VecWord x);
int Most_Significant_Bit(VecWord x);
void Define_Vector_Size(int max_val);
void Vector_From_Interval(Vector vec, int min, int max);
int Vector_Nb_Elem(Vector vec);
int Vector_Ith_Elem(Vector vec, int n);
int Vector_Next_After(Vector vec, int n);
int Vector_Next_Before(Vector vec, int n);
void Vector_Empty(Vector vec);
void Vector_Full(Vector vec);
Bool Vector_Test_Null_Inter(Vector vec, Vector vec1);
void Vector_Copy(Vector vec, Vector vec1);
void Vector_Union(Vector vec, Vector vec1);
void Vector_Inter(Vector vec, Vector vec1);
void Vector_Compl(Vector vec);
void Vector_Add_Vector(Vector vec, Vector vec1);
void Vector_Sub_Vector(Vector vec, Vector vec1);
void Vector_Mul_Vector(Vector vec, Vector vec1);
void Vector_Div_Vector(Vector vec, Vector vec1);
void Vector_Mod_Vector(Vector vec, Vector vec1);
void Vector_Add_Value(Vector vec, int n);
void Vector_Mul_Value(Vector vec, int n);
void Vector_Div_Value(Vector vec, int n);
void Vector_Mod_Value(Vector vec, int n);
Bool Range_Test_Value(Range *range, int n);
Bool Range_Test_Null_Inter(Range *range, Range *range1);
void Range_Copy(Range *range, Range *range1);
int Range_Nb_Elem(Range *range);
int Range_Ith_Elem(Range *range, int n);
int Range_Next_After(Range *range, int n);
int Range_Next_Before(Range *range, int n);
void Range_Set_Value(Range *range, int n);
void Range_Reset_Value(Range *range, int n);
void Range_Becomes_Sparse(Range *range);
void Range_From_Vector(Range *range);
void Range_Union(Range *range, Range *range1);
void Range_Inter(Range *range, Range *range1);
void Range_Compl(Range *range);
void Range_Add_Range(Range *range, Range *range1);
void Range_Sub_Range(Range *range, Range *range1);
void Range_Mul_Range(Range *range, Range *range1);
void Range_Div_Range(Range *range, Range *range1);
void Range_Mod_Range(Range *range, Range *range1);
void Range_Add_Value(Range *range, int n);
void Range_Mul_Value(Range *range, int n);
void Range_Div_Value(Range *range, int n);
void Range_Mod_Value(Range *range, int n);
char *Range_To_String(Range *range);
/*---------------------------------*
 * Vector Management Macros        *
 *---------------------------------*/
#define Word_No_And_Bit_No(w,b)    (((VecWord) (w) << WORD_SIZE_BITS)|\
                                     (VecWord) (b))
#define Word_No(n)                 ((VecWord) (n) >> WORD_SIZE_BITS)
#define Bit_No(n)                  ((n) & (((VecWord) 1<<WORD_SIZE_BITS)-1))
#define Vector_Test_Value(vec,n)   ((vec[Word_No(n)] & ((VecWord) 1 << Bit_No(n))) != 0)
#define Vector_Set_Value(vec,n)    (vec[Word_No(n)] |= ((VecWord) 1 << Bit_No(n)))
#define Vector_Reset_Value(vec,n)  (vec[Word_No(n)] &= ~((VecWord) 1 << Bit_No(n)))
#define Vector_Allocate_If_Necessary(vec)                                   \
   do {                                                                     \
     if (vec==NULL)                                                         \
         Vector_Allocate(vec);                                              \
    } while(0)
#define Vector_Allocate(vec)                                                \
   do {                                                                     \
     vec=(Vector) RANGE_TOP_STACK;                                          \
     RANGE_TOP_STACK += vec_size;                                           \
    } while(0)
	  /* To enumerate a vector use VECTOR_BEGIN_ENUM / VECTOR_END_ENUM */
	  /* macros as follows:                                            */
	  /* ...                                                           */
	  /* VECTOR_BEGIN_ENUM(the_vector,vec_elem)                        */
	  /*    your code (vec_elem contains the current range element)    */
	  /* VECTOR_END_ENUM                                               */
#define VECTOR_BEGIN_ENUM(vec,vec_elem)                                     \
    {                                                                       \
     Vector  enum_end=vec+vec_size,enum_i=vec;                              \
     int     enum_j;                                                        \
     VecWord enum_word;                                                     \
                                                                            \
     vec_elem=0;                                                            \
     do                                                                     \
        {                                                                   \
         enum_word= *enum_i;                                                \
         for(enum_j=0;enum_j++<WORD_SIZE;enum_word >>= 1,vec_elem++)        \
            {                                                               \
             if (enum_word & 1)                                             \
                {
#define VECTOR_END_ENUM                                                     \
                }                                                           \
            }                                                               \
        }                                                                   \
     while(++enum_i<enum_end);                                              \
    }
/*---------------------------------*
 * Range Management Macros         *
 *---------------------------------*/
#define Is_Interval(range)         ((range)->vec==NULL)
#define Is_Sparse(range)           ((range)->vec!=NULL)
#define Is_Empty(range)            ((range)->min > (range)->max)
#define Is_Not_Empty(range)        ((range)->max >=(range)->min)
#define Set_To_Empty(range)        (range)->max=(int)(1 << (sizeof(int)*8-1))
#define Range_Init_Interval(range,r_min,r_max)                              \
    do {                                                                    \
     (range)->extra_cstr=FALSE;                                             \
     (range)->min       =(r_min);                                           \
     (range)->max       =(r_max);                                           \
     (range)->vec       =NULL;                                              \
    } while(0)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : bips_fd.h                                                       *
 * Descr.: general header file                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : math_supp.h                                                     *
 * Descr.: mathematical support - header file                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
/*---------------------------------*
 * Constants                       *
 *---------------------------------*/
#define MASK_EMPTY                 0
#define MASK_LEFT                  1
#define MASK_RIGHT                 2
#define TAGGED_1                   Tag_Value(INT,1)
#if 0
#define DEBUG
#endif
/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/
/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/
#ifdef MATH_SUPP_FILE
Bool full_ac;
#ifdef DEBUG
char *cur_op;
#endif
#else
#ifdef DEBUG
char *cur_op;
#endif
extern Bool full_ac;
#endif
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/
Bool Load_Left_Right(Bool optim_eq,
		     WamWord le_word, WamWord re_word,
		     int *mask, WamWord *c_word,
		     WamWord *l_word, WamWord *r_word);
Bool Term_Math_Loading(WamWord l_word, WamWord r_word);
Bool Fd_Math_Unify_X_Y(WamWord x, WamWord y);
#ifdef DEBUG
void Debug_Display(char *fct, int n, ...);
#endif
	  /* defined in fd_math_fd.fd */
Bool x_eq_c(WamWord x, WamWord c);	/* in math_supp.c */
Bool x_eq_y(WamWord x, WamWord y);
Bool x_plus_c_eq_y(WamWord x, WamWord c, WamWord y);
Bool x_eq_y_F(WamWord x, WamWord y);
Bool x_plus_c_eq_y_F(WamWord x, WamWord c, WamWord y);
Bool x_neq_c(WamWord x, WamWord c);
Bool x_neq_y(WamWord x, WamWord y);
Bool x_plus_c_neq_y(WamWord x, WamWord c, WamWord y);
Bool x_lt_y(WamWord x, WamWord y);
Bool x_lte_c(WamWord x, WamWord c);
Bool x_lte_y(WamWord x, WamWord y);
Bool x_plus_c_lte_y(WamWord x, WamWord c, WamWord y);
Bool x_gte_c(WamWord x, WamWord c);
Bool x_plus_c_gte_y(WamWord x, WamWord c, WamWord y);
Bool ax_eq_y(WamWord a, WamWord x, WamWord y);
Bool x_plus_y_eq_z(WamWord x, WamWord y, WamWord z);
Bool ax_plus_y_eq_z(WamWord a, WamWord x, WamWord y, WamWord z);
Bool ax_plus_by_eq_z(WamWord a, WamWord x, WamWord b, WamWord y, WamWord z);
Bool x_plus_y_plus_z_eq_t(WamWord x, WamWord y, WamWord z, WamWord t);
Bool ax_plus_y_plus_z_eq_t(WamWord a, WamWord x, WamWord y, WamWord z,
			   WamWord t);
Bool ax_plus_by_plus_z_eq_t(WamWord a, WamWord x, WamWord b, WamWord y,
			    WamWord z, WamWord t);
Bool ax_eq_y_F(WamWord a, WamWord x, WamWord y);
Bool x_plus_y_eq_z_F(WamWord x, WamWord y, WamWord z);
Bool ax_plus_y_eq_z_F(WamWord a, WamWord x, WamWord y, WamWord z);
Bool ax_plus_by_eq_z_F(WamWord a, WamWord x, WamWord b, WamWord y,
		       WamWord z);
Bool x_plus_y_plus_z_eq_t_F(WamWord x, WamWord y, WamWord z, WamWord t);
Bool ax_plus_y_plus_z_eq_t_F(WamWord a, WamWord x, WamWord y, WamWord z,
			     WamWord t);
Bool ax_plus_by_plus_z_eq_t_F(WamWord a, WamWord x, WamWord b, WamWord y,
			      WamWord z, WamWord t);
Bool zero_power_n_eq_y(WamWord n, WamWord y);
Bool a_power_n_eq_y(WamWord a, WamWord n, WamWord y);
Bool x_power_a_eq_y(WamWord x, WamWord a, WamWord y);
Bool x2_eq_y(WamWord x, WamWord y);
Bool xy_eq_z(WamWord x, WamWord y, WamWord z);
Bool a_power_n_eq_y_F(WamWord a, WamWord n, WamWord y);
Bool x_power_a_eq_y_F(WamWord x, WamWord a, WamWord y);
Bool x2_eq_y_F(WamWord x, WamWord y);
Bool xy_eq_z_F(WamWord x, WamWord y, WamWord z);
Bool min_x_a_eq_z(WamWord x, WamWord a, WamWord z);
Bool min_x_y_eq_z(WamWord x, WamWord y, WamWord z);
Bool min_x_a_eq_z_F(WamWord x, WamWord a, WamWord z);
Bool min_x_y_eq_z_F(WamWord x, WamWord y, WamWord z);
Bool max_x_a_eq_z(WamWord x, WamWord a, WamWord z);
Bool max_x_y_eq_z(WamWord x, WamWord y, WamWord z);
Bool max_x_a_eq_z_F(WamWord x, WamWord a, WamWord z);
Bool max_x_y_eq_z_F(WamWord x, WamWord y, WamWord z);
Bool abs_x_minus_a_eq_z(WamWord x, WamWord a, WamWord z);
Bool abs_x_minus_y_eq_z(WamWord x, WamWord y, WamWord z);
Bool abs_x_minus_a_eq_z_F(WamWord x, WamWord a, WamWord z);
Bool abs_x_minus_y_eq_z_F(WamWord x, WamWord y, WamWord z);
Bool quot_rem_a_y_r_eq_z(WamWord a, WamWord y, WamWord r, WamWord z);
Bool quot_rem_x_a_r_eq_z(WamWord x, WamWord a, WamWord r, WamWord z);
Bool quot_rem_x_y_r_eq_z(WamWord x, WamWord y, WamWord r, WamWord z);
Bool quot_rem_a_y_r_eq_z_F(WamWord a, WamWord y, WamWord r, WamWord z);
Bool quot_rem_x_a_r_eq_z_F(WamWord x, WamWord a, WamWord r, WamWord z);
Bool quot_rem_x_y_r_eq_z_F(WamWord x, WamWord y, WamWord r, WamWord z);
	  /* defined in fd_bool_fd.fd */
Bool not_x_eq_b(WamWord x, WamWord b);
Bool x_equiv_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_nequiv_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_imply_y_eq_1(WamWord x, WamWord y);
Bool x_imply_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_nimply_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_and_y_eq_0(WamWord x, WamWord y);
Bool x_and_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_nand_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_or_y_eq_1(WamWord x, WamWord y);
Bool x_or_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool x_nor_y_eq_b(WamWord x, WamWord y, WamWord b);
Bool truth_x_eq_c(WamWord x, WamWord c, WamWord b);
Bool truth_x_eq_y(WamWord x, WamWord y, WamWord b);
Bool truth_x_plus_c_eq_y(WamWord x, WamWord c, WamWord y, WamWord b);
Bool truth_x_eq_c_F(WamWord x, WamWord c, WamWord b);
Bool truth_x_eq_y_F(WamWord x, WamWord y, WamWord b);
Bool truth_x_plus_c_eq_y_F(WamWord x, WamWord c, WamWord y, WamWord b);
Bool truth_x_neq_c(WamWord x, WamWord c, WamWord b);
Bool truth_x_neq_y(WamWord x, WamWord y, WamWord b);
Bool truth_x_plus_c_neq_y(WamWord x, WamWord c, WamWord y, WamWord b);
Bool truth_x_neq_c_F(WamWord x, WamWord c, WamWord b);
Bool truth_x_neq_y_F(WamWord x, WamWord y, WamWord b);
Bool truth_x_plus_c_neq_y_F(WamWord x, WamWord c, WamWord y, WamWord b);
Bool truth_x_lt_y(WamWord x, WamWord y, WamWord b);
Bool truth_x_lte_c(WamWord x, WamWord c, WamWord b);
Bool truth_x_lte_y(WamWord x, WamWord y, WamWord b);
Bool truth_x_plus_c_lte_y(WamWord x, WamWord c, WamWord y, WamWord b);
Bool truth_x_gte_c(WamWord x, WamWord c, WamWord b);
Bool truth_x_plus_c_gte_y(WamWord x, WamWord c, WamWord y, WamWord b);
#ifdef DEBUG
#define DEBUG_2(f,a1,a2)               Debug_Display(#f,2,a1,a2);
#define DEBUG_3(f,a1,a2,a3)            Debug_Display(#f,3,a1,a2,a3);
#define DEBUG_4(f,a1,a2,a3,a4)         Debug_Display(#f,4,a1,a2,a3,a4);
#define DEBUG_5(f,a1,a2,a3,a4,a5)      Debug_Display(#f,5,a1,a2,a3,a4,a5);
#define DEBUG_6(f,a1,a2,a3,a4,a5,a6)   Debug_Display(#f,6,a1,a2,a3,a4,a5,a6);
#else
#define DEBUG_2(f,a1,a2)
#define DEBUG_3(f,a1,a2,a3)
#define DEBUG_4(f,a1,a2,a3,a4)
#define DEBUG_5(f,a1,a2,a3,a4,a5)
#define DEBUG_6(f,a1,a2,a3,a4,a5,a6)
#endif
#define PRIM_CSTR_2(f,a1,a2)                                                \
    do {                                                                    \
     DEBUG_2(f,a1,a2)                                                       \
     if (!f(a1,a2))                                                         \
         return FALSE;                                                      \
    } while(0)
#define PRIM_CSTR_3(f,a1,a2,a3)                                             \
    do {                                                                    \
     DEBUG_3(f,a1,a2,a3)                                                    \
     if (!f(a1,a2,a3))                                                      \
         return FALSE;                                                      \
    } while(0)
#define PRIM_CSTR_4(f,a1,a2,a3,a4)                                          \
    do {                                                                    \
     DEBUG_4(f,a1,a2,a3,a4)                                                 \
     if (!f(a1,a2,a3,a4))                                                   \
         return FALSE;                                                      \
    } while(0)
#define PRIM_CSTR_5(f,a1,a2,a3,a4,a5)                                       \
    do {                                                                    \
     DEBUG_5(f,a1,a2,a3,a4,a5)                                              \
     if (!f(a1,a2,a3,a4,a5))                                                \
         return FALSE;                                                      \
    } while(0)
#define PRIM_CSTR_6(f,a1,a2,a3,a4,a5,a6)                                    \
    do {                                                                    \
     DEBUG_6(f,a1,a2,a3,a4,a5,a6)                                           \
     if (!f(a1,a2,a3,a4,a5,a6))                                             \
         return FALSE;                                                      \
    } while(0)
#define MATH_CSTR_2(f,a1,a2)                                                \
    do {                                                                    \
     if (full_ac==FALSE)                                                    \
         PRIM_CSTR_2(f,a1,a2);                                              \
      else                                                                  \
         PRIM_CSTR_2(f##_F,a1,a2);                                          \
    } while(0)
#define MATH_CSTR_3(f,a1,a2,a3)                                             \
    do {                                                                    \
     if (full_ac==FALSE)                                                    \
         PRIM_CSTR_3(f,a1,a2,a3);                                           \
      else                                                                  \
         PRIM_CSTR_3(f##_F,a1,a2,a3);                                       \
    } while(0)
#define MATH_CSTR_4(f,a1,a2,a3,a4)                                          \
    do {                                                                    \
     if (full_ac==FALSE)                                                    \
         PRIM_CSTR_4(f,a1,a2,a3,a4);                                        \
      else                                                                  \
         PRIM_CSTR_4(f##_F,a1,a2,a3,a4);                                    \
    } while(0)
#define MATH_CSTR_5(f,a1,a2,a3,a4,a5)                                       \
    do {                                                                    \
     if (full_ac==FALSE)                                                    \
         PRIM_CSTR_5(f,a1,a2,a3,a4,a5);                                     \
      else                                                                  \
         PRIM_CSTR_5(f##_F,a1,a2,a3,a4,a5);                                 \
    } while(0)
#define MATH_CSTR_6(f,a1,a2,a3,a4,a5,a6)                                    \
    do {                                                                    \
     if (full_ac==FALSE)                                                    \
         PRIM_CSTR_6(f,a1,a2,a3,a4,a5,a6);                                  \
      else                                                                  \
         PRIM_CSTR_6(f##_F,a1,a2,a3,a4,a5,a6);                              \
    } while(0)
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : oper_supp.h                                                     *
 * Descr.: FD Operation support - header file                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
unsigned Power(unsigned x, unsigned n);
unsigned Nth_Root_Dn(unsigned y, unsigned n);
unsigned Nth_Root_Up(unsigned y, unsigned n);
unsigned Nth_Root_Exact(unsigned y, unsigned n);
unsigned Sqrt_Dn(unsigned y);
unsigned Sqrt_Up(unsigned y);
unsigned Sqrt_Exact(unsigned y);
unsigned Find_Expon_Dn(unsigned x, unsigned y);
unsigned Find_Expon_Up(unsigned x, unsigned y);
unsigned Find_Expon_Exact(unsigned x, unsigned y);
void Full_Coeff_Power_Var(Range *y, int a, Range *n);
void Full_Find_Expon(Range *n, int a, Range *y);
void Full_Var_Power_Coeff(Range *y, Range *x, int a);
void Full_Nth_Root(Range *x, Range *y, int a);
void Full_Max_Cst_Var(Range *z, int a, Range *x);
void Full_Min_Cst_Var(Range *z, int a, Range *x);
#endif /* NO_USE_FD_SOLVER */
