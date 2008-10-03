/* gprolog.h generated from headers.h using cpp_headers */
/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : headers.h                                                       *
 * Descr.: GNU Prolog - general header file (for users)                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/
/* $Id$ */
#ifndef _GPROLOG_H
#define _GPROLOG_H
#ifdef __cplusplus
extern "C" {
#endif
#ifndef _GP_CONFIG_H
#define _GP_CONFIG_H
#define HAVE_TERMIOS_H 1
#define HAVE_MALLOC_H 1
#define HAVE_MMAP 1
#define HAVE_MPROTECT 1
#define HAVE_MALLOPT 1
#define NO_USE_EBP 1
#define NO_USE_GUI_CONSOLE 1
#define M_ix86 1
#define M_linux 1
#define M_ix86_linux 1
#define PROLOG_NAME1 "gprolog"
#define PROLOG_NAME "GNU Prolog"
#define PROLOG_VERSION "1.3.1"
#define PROLOG_DATE "Oct 1 2008"
#define PROLOG_COPYRIGHT "Copyright (C) 1999-2008 Daniel Diaz"
#define TOP_LEVEL "gprolog"
#define GPLC "gplc"
#define HEXGPLC "hexgplc"
#define ENV_VARIABLE "PL_PATH"
#define M_VENDOR "pc"
#define M_CPU "i686"
#define M_OS "linux-gnu"
#define CC "gcc"
#define CFLAGS_PREFIX_REG "-ffixed-%s"
#define CFLAGS "-g -Wall"
#define CFLAGS_MACHINE "-march=pentiumpro"
#define LDFLAGS ""
#define LDLIBS "-lm"
#define AS "as"
#define STRIP "strip"
#define ASM_SUFFIX ".s"
#define OBJ_SUFFIX ".o"
#define EXE_SUFFIX ""
#define CC_OBJ_NAME_OPT "-o "
#define CC_EXE_NAME_OPT "-o "
#define DLL_W32GUICONS "w32guicons.dll"
#define LIB_LINEDIT "liblinedit.a"
#define LIB_ENGINE_PL "libengine_pl.a"
#define LIB_BIPS_PL "libbips_pl.a"
#define LIB_ENGINE_FD "libengine_fd.a"
#define LIB_BIPS_FD "libbips_fd.a"
#define SIZEOF_LONG 4
#define WORD_SIZE                  (8 * SIZEOF_LONG)
#ifndef _ARCH_DEP_H
#define _ARCH_DEP_H
#if defined(_WIN32) && !defined(__CYGWIN__)
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
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
#define strcasecmp                 stricmp
#define strncasecmp                strnicmp
#define unlink                     _unlink
#define tzset                      _tzset
#define access                     _access
#ifndef F_OK
#define F_OK                       00
#define W_OK                       02
#define R_OK                       04
#define X_OK                       F_OK
#endif
#ifndef S_ISDIR
#define	S_ISDIR(m)	           (((m)&_S_IFMT) == _S_IFDIR)
#define	S_ISCHR(m)                 (((m)&_S_IFMT) == _S_IFCHR)
#define	S_ISFIFO(m)	           (((m)&_S_IFMT) == _S_IFIFO)
#define	S_ISREG(m)	           (((m)&_S_IFMT) == _S_IFREG)
#endif
#ifndef S_IRUSR
#define S_IRUSR                    _S_IREAD
#define S_IWUSR                    _S_IWRITE
#define S_IXUSR                    _S_IEXEC
#endif
#define DIR_SEP_S                  "\\"
#define DIR_SEP_C                  '\\'
#else
#define DIR_SEP_S                  "/"
#define DIR_SEP_C                  '/'
#endif /* defined(_WIN32) && !defined(__CYGWIN__) */
#if defined(M_ix86_cygwin) || defined(M_ix86_sco)
#define Set_Line_Buf(s)            setvbuf(s, NULL, _IOLBF, 0)
#elif defined(_WIN32)
#define Set_Line_Buf(s)            setbuf(s, NULL)
#else
#define Set_Line_Buf(s)            setlinebuf(s)
#endif
#ifndef NO_USE_GUI_CONSOLE
#define W32_GUI_CONSOLE
#endif
#ifdef M_sparc_sunos
#define __USE_FIXED_PROTOTYPES__
#endif
#if defined(M_ix86_sco)
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#endif
#if !defined(_WIN32) && !defined(__unix__)
#define __unix__
#endif
#ifndef HAVE_FGETC
#define fgetc getc
#endif
#if defined(M_ix86)
#define COULD_COMPILE_FOR_FC
#ifdef __GNUC__
#define FC_MAX_ARGS_IN_REGS 3	
#define FC_SET_OF_REGISTERS { "%eax", "%edx", "%ecx" };
#define FC_ATTRIB __attribute__((regparm(FC_MAX_ARGS_IN_REGS)))
#elif 0  /* under MSVC++ we can use __fastcall convention (#elif 1 if wanted) */
#define FC_MAX_ARGS_IN_REGS 2
#define FC_SET_OF_REGISTERS { "%ecx", "%edx" };
#define FC_ATTRIB __fastcall
#else
#define FC_MAX_ARGS_IN_REGS 0
#define FC_SET_OF_REGISTERS { NULL };
#define FC_ATTRIB
#endif
#endif
#if !defined(NO_USE_FAST_CALL) && defined(FC_ATTRIB)
#define FC_USED_TO_COMPILE_CORE
#ifndef FC /* to compile Ma2Asm/check.c without FC */
#define FC FC_ATTRIB
#endif
#else
#define FC
#endif
#if defined(_WIN32) || defined(__CYGWIN__)
typedef enum {
    ExceptContinueExecution,
    ExceptContinueSearch,
    ExceptNestedException,
    ExceptCollidedUnwind
} EXCEPT_DISPOSITION;
typedef struct _excp_lst
{
  struct _excp_lst *chain;
  EXCEPT_DISPOSITION (*handler)();
} excp_lst;
#ifdef __GNUC__
#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  asm("movl %%fs:0,%0" : "=r" (e.chain));	\
  asm("movl %0,%%fs:0" : : "r" (&e));
#  define SEH_POP				\
  asm("movl %0,%%fs:0" : : "r" (e.chain));	\
}
#elif defined(_MSC_VER)
#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  __asm push eax				\
  __asm mov eax,dword ptr fs:[0]		\
  __asm mov dword ptr [e.chain],eax		\
  __asm lea eax,[e]				\
  __asm mov dword ptr fs:[0],eax		\
  __asm pop eax
#  define SEH_POP				\
  __asm push eax				\
  __asm mov eax,dword ptr [e.chain]		\
  __asm mov dword ptr fs:[0],eax		\
  __asm pop eax					\
}
#elif defined(__LCC__)
#  define SEH_PUSH(new_handler)			\
{						\
  excp_lst e;					\
  EXCEPT_DISPOSITION new_handler();		\
  e.handler = new_handler;			\
  _asm("pushl %eax");				\
  _asm("movl %fs:0,%eax");			\
  _asm("movl %eax,%e");				\
  _asm("leal %e,%eax");				\
  _asm("movl %eax,%fs:0");			\
  _asm("popl %eax");
#  define SEH_POP				\
  _asm("pushl %eax");				\
  _asm("movl %e,%eax");				\
  _asm("movl %eax,%fs:0");			\
  _asm("popl %eax");				\
}
#else
#  error macros SEH_PUSH/POP undefined for this compiler
#endif
#endif /* defined(_WIN32) || defined(__CYGWIN__) */
#endif /* !_ARCH_DEP_H */
#endif /* !_GP_CONFIG_H */
#define MAX_OBJECT                 1024
#define START_PRED_TBL_SIZE        4096
#define START_OPER_TBL_SIZE        1024
#define ATOM_SIZE                  20
#define MAX_ATOM                   (1 << ATOM_SIZE) /* number of elements */
#define NB_OF_X_REGS               256
#define MAX_ARITY                  (NB_OF_X_REGS - 1)
typedef struct
{
  char *endt;
  char *cur_t;
  char *cur_p;
}
HashScan;
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
#ifndef _BOOL_H
#define _BOOL_H
#ifdef FALSE
#    if FALSE != 0
#        error "FALSE already defined with a value != 0"
#    endif
#else
#define FALSE 0
#endif
#ifdef TRUE
#    if TRUE != 1
#        error "TRUE already defined with a value != 1"
#    endif
#else
#define TRUE 1
#endif
#ifndef Bool
typedef int Bool;
#endif
#endif /* !_BOOL_H */
#define MAP_REG_TR        	"ebx"
#define MAP_OFFSET_B     	((NB_OF_X_REGS+0)*4)
#define MAP_OFFSET_H     	((NB_OF_X_REGS+1)*4)
#define MAP_OFFSET_HB1   	((NB_OF_X_REGS+2)*4)
#define MAP_OFFSET_CP    	((NB_OF_X_REGS+3)*4)
#define MAP_OFFSET_E     	((NB_OF_X_REGS+4)*4)
#define MAP_OFFSET_CS    	((NB_OF_X_REGS+5)*4)
#define MAP_OFFSET_S     	((NB_OF_X_REGS+6)*4)
#define MAP_OFFSET_STAMP 	((NB_OF_X_REGS+7)*4)
#define MAP_OFFSET_BCI   	((NB_OF_X_REGS+8)*4)
#define MAP_OFFSET_LSSA  	((NB_OF_X_REGS+9)*4)
#define CFLAGS_REGS		"-ffixed-ebx "
typedef long WamWord;		/* a wamword is a long (32/64 bits) */
typedef void (*CodePtr) ();	/* a code pointer is a ptr to fct */
typedef CodePtr WamCont;	/* a continuation is a code pointer */
#ifndef ONLY_TAG_PART
#define X(x)                       (reg_bank[x])
#define A(a)                       (reg_bank[a])
typedef WamWord *WamWordP;
register WamWordP		TR  asm ("ebx");
#define B			(((WamWordP *) reg_bank)[NB_OF_X_REGS+0])
#define H			(((WamWordP *) reg_bank)[NB_OF_X_REGS+1])
#define HB1			(((WamWordP *) reg_bank)[NB_OF_X_REGS+2])
#define CP			(((WamCont  *) reg_bank)[NB_OF_X_REGS+3])
#define E			(((WamWordP *) reg_bank)[NB_OF_X_REGS+4])
#define CS			(((WamWordP *) reg_bank)[NB_OF_X_REGS+5])
#define S			(((WamWordP *) reg_bank)[NB_OF_X_REGS+6])
#define STAMP			(((WamWord  *) reg_bank)[NB_OF_X_REGS+7])
#define BCI			(((WamWord  *) reg_bank)[NB_OF_X_REGS+8])
#define LSSA			(((WamWordP *) reg_bank)[NB_OF_X_REGS+9])
#define NB_OF_REGS          	11
#define NB_OF_ALLOC_REGS    	1
#define NB_OF_NOT_ALLOC_REGS	10
#define REG_BANK_SIZE       	(NB_OF_X_REGS+NB_OF_NOT_ALLOC_REGS)
#define NB_OF_USED_MACHINE_REGS 1
#ifdef ENGINE_FILE
WamWord reg_bank[REG_BANK_SIZE];
WamWord buff_signal_reg[NB_OF_USED_MACHINE_REGS + 1];
char *reg_tbl[] = { "TR", "B", "H", "HB1", "CP", "E", "CS", "S", "STAMP", "BCI", "LSSA"};
#else
extern WamWord reg_bank[];
extern WamWord buff_signal_reg[];
extern char *reg_tbl[];
#endif
#define Init_Reg_Bank(x)
#define Reg(i)			(((i)==0) ? (WamWord) TR 	: \
				 ((i)==1) ? (WamWord) B  	: \
				 ((i)==2) ? (WamWord) H  	: \
				 ((i)==3) ? (WamWord) HB1	: \
				 ((i)==4) ? (WamWord) CP 	: \
				 ((i)==5) ? (WamWord) E  	: \
				 ((i)==6) ? (WamWord) CS 	: \
				 ((i)==7) ? (WamWord) S  	: \
				 ((i)==8) ? (WamWord) STAMP	: \
				 ((i)==9) ? (WamWord) BCI	: \
				            (WamWord) LSSA)
#define Save_All_Regs(buff_save) \
  do { \
    buff_save[0] = (WamWord) TR; \
    buff_save[1] = (WamWord) B; \
    buff_save[2] = (WamWord) H; \
    buff_save[3] = (WamWord) HB1; \
    buff_save[4] = (WamWord) CP; \
    buff_save[5] = (WamWord) E; \
    buff_save[6] = (WamWord) CS; \
    buff_save[7] = (WamWord) S; \
    buff_save[8] = (WamWord) STAMP; \
    buff_save[9] = (WamWord) BCI; \
    buff_save[10] = (WamWord) LSSA; \
  } while(0)
#define Restore_All_Regs(buff_save) \
  do { \
    TR     = (WamWordP) buff_save[0]; \
    B      = (WamWordP) buff_save[1]; \
    H      = (WamWordP) buff_save[2]; \
    HB1    = (WamWordP) buff_save[3]; \
    CP     = (WamCont ) buff_save[4]; \
    E      = (WamWordP) buff_save[5]; \
    CS     = (WamWordP) buff_save[6]; \
    S      = (WamWordP) buff_save[7]; \
    STAMP  = (WamWord ) buff_save[8]; \
    BCI    = (WamWord ) buff_save[9]; \
    LSSA   = (WamWordP) buff_save[10]; \
  } while(0)
#define Save_Machine_Regs(buff_save) \
  do { \
    buff_save[0] = (WamWord) TR; \
  } while(0)
#define Restore_Machine_Regs(buff_save) \
  do { \
    TR = (WamWordP) buff_save[0]; \
  } while(0)
#define Start_Protect_Regs_For_Signal \
  do { \
    Save_Machine_Regs(buff_signal_reg); \
    buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 1; \
  } while(0)
#define Stop_Protect_Regs_For_Signal \
  buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 0; \

#define Restore_Protect_Regs_For_Signal \
  do { \
    if (buff_signal_reg[NB_OF_USED_MACHINE_REGS]) { \
      Restore_Machine_Regs(buff_signal_reg); \
      Stop_Protect_Regs_For_Signal; \
    } \
  } while(0)
#endif
#define TAG_SIZE     		3
#define TAG_SIZE_LOW 		2
#define TAG_SIZE_HIGH		1
#define VALUE_SIZE   		29
#define TAG_MASK     		0x80000003UL
#define VALUE_MASK   		0x7ffffffcUL
#define Tag_Mask_Of(w)		((unsigned long) (w) & (TAG_MASK))
#define Tag_From_Tag_Mask(w) 	(((unsigned long) (w) >> 29) | ((w) & 3))
#define Tag_Of(w)     		((((unsigned long) (w) >> 31) << 2) | ((w) & 3))
#define TAG_REF_MASK		0UL
#define TAG_LST_MASK		0x1UL
#define TAG_STC_MASK		0x2UL
#define TAG_ATM_MASK		0x3UL
#define TAG_FLT_MASK		0x80000000UL
#define TAG_FDV_MASK		0x80000001UL
#define TAG_INT_MASK		0x80000003UL
#define NB_OF_TAGS       	7
#define REF        		0 
#define LST        		1 
#define STC        		2 
#define ATM        		3 
#define FLT        		4 
#define FDV        		5 
#define INT        		7 
#define Tag_Long_Int(tm, v)  	((((unsigned long) ((v) << 3)) >> 1) | (tm))
#define Tag_Short_Uns(tm, v)	(((unsigned long) (v) << 2) + (tm))
#define Tag_Address(tm, v)  	((unsigned long) (v) + (tm))
#define UnTag_Long_Int(w)    	((long) ((w) << 1) >> 3)
#define UnTag_Short_Uns(w)	UnTag_Long_Int(w)
#define UnTag_Address(w)  	((WamWord *) ((w) & VALUE_MASK))
#define Tag_REF(v)  		Tag_Address(TAG_REF_MASK, v)
#define Tag_LST(v)  		Tag_Address(TAG_LST_MASK, v)
#define Tag_STC(v)  		Tag_Address(TAG_STC_MASK, v)
#define Tag_ATM(v)  		Tag_Short_Uns(TAG_ATM_MASK, v)
#define Tag_FLT(v)  		Tag_Address(TAG_FLT_MASK, v)
#define Tag_FDV(v)  		Tag_Address(TAG_FDV_MASK, v)
#define Tag_INT(v)  		(((unsigned long) (v) << 2) | TAG_MASK)
#define UnTag_REF(w)  		((WamWord *) (w))
#define UnTag_LST(w)  		UnTag_Address(w)
#define UnTag_STC(w)  		UnTag_Address(w)
#define UnTag_ATM(w)  		((unsigned long) (w) >> 2)
#define UnTag_FLT(w)  		UnTag_Address(w)
#define UnTag_FDV(w)  		UnTag_Address(w)
#define UnTag_INT(w)  		UnTag_Long_Int(w)
#define Tag_Is_REF(w)  		(Tag_Mask_Of(w) == TAG_REF_MASK)
#define Tag_Is_LST(w)  		(Tag_Mask_Of(w) == TAG_LST_MASK)
#define Tag_Is_STC(w)  		(Tag_Mask_Of(w) == TAG_STC_MASK)
#define Tag_Is_ATM(w)  		(Tag_Mask_Of(w) == TAG_ATM_MASK)
#define Tag_Is_FLT(w)  		(Tag_Mask_Of(w) == TAG_FLT_MASK)
#define Tag_Is_FDV(w)  		(Tag_Mask_Of(w) == TAG_FDV_MASK)
#define Tag_Is_INT(w)  		(Tag_Mask_Of(w) == TAG_INT_MASK)
typedef enum
{
  LONG_INT,
  SHORT_UNS,
  ADDRESS
}TypTag;
typedef struct
{
  char *name;
  TypTag type;
  int value;
  long tag_mask;
}InfTag;
#ifdef ENGINE_FILE
InfTag tag_tbl[] =
{
  { "REF", ADDRESS, 0, 0UL },
  { "LST", ADDRESS, 1, 0x1UL },
  { "STC", ADDRESS, 2, 0x2UL },
  { "ATM", SHORT_UNS, 3, 0x3UL },
  { "FLT", ADDRESS, 4, 0x80000000UL },
  { "FDV", ADDRESS, 5, 0x80000001UL },
  { "INT", LONG_INT, 7, 0x80000003UL }
};
#else
extern InfTag tag_tbl[];
#endif
#ifndef ONLY_TAG_PART
#define KBytes_To_Wam_Words(kb)    ((1024 * kb + sizeof(WamWord) - 1) / sizeof(WamWord))
#define Wam_Words_To_KBytes(ww)    (ww * sizeof(WamWord) / 1024)
#define Local_Top                  ((B >= E) ? B : E)
#define NB_OF_STACKS 		4
#define Trail_Stack       	(stk_tbl[0].stack)
#define Trail_Size        	(stk_tbl[0].size)
#define Trail_Offset(adr) 	((WamWord *)(adr) - Trail_Stack)
#define Trail_Used_Size   	Trail_Offset(TR)
#define Cstr_Stack       	(stk_tbl[1].stack)
#define Cstr_Size        	(stk_tbl[1].size)
#define Cstr_Offset(adr) 	((WamWord *)(adr) - Cstr_Stack)
#define Cstr_Used_Size   	Cstr_Offset(CS)
#define Global_Stack       	(stk_tbl[2].stack)
#define Global_Size        	(stk_tbl[2].size)
#define Global_Offset(adr) 	((WamWord *)(adr) - Global_Stack)
#define Global_Used_Size   	Global_Offset(H)
#define Local_Stack       	(stk_tbl[3].stack)
#define Local_Size        	(stk_tbl[3].size)
#define Local_Offset(adr) 	((WamWord *)(adr) - Local_Stack)
#define Local_Used_Size   	Local_Offset(Local_Top)
#define Stack_Top(s)       	(((s) == 0) ? TR : ((s) == 1) ? CS : ((s) == 2) ? H : Local_Top)
typedef struct
{
  char *name;
  char *env_var_name;
  long *p_def_size;
  int default_size; 	/* in WamWords */
  int size;         	/* in WamWords */
  WamWord *stack;
}InfStack;
#ifdef ENGINE_FILE
long def_trail_size;
long def_cstr_size;
long def_global_size;
long def_local_size;
long fixed_sizes;
InfStack stk_tbl[] =
{
 { "trail", "TRAILSZ", &def_trail_size, 2097152, 0, NULL },
 { "cstr", "CSTRSZ", &def_cstr_size, 2097152, 0, NULL },
 { "global", "GLOBALSZ", &def_global_size, 4194304, 0, NULL },
 { "local", "LOCALSZ", &def_local_size, 2097152, 0, NULL }
};
#else
extern long def_trail_size;
extern long def_cstr_size;
extern long def_global_size;
extern long def_local_size;
extern long fixed_sizes;
extern InfStack stk_tbl[];
#endif
#endif
#ifdef NO_STACK_TEST
#   undef  M_Check_Stacks()
#   define M_Check_Stacks()
#endif
#define cpp_recurs(p, n)           p##_##n
#define Prolog_Predicate(p, n)     cpp_recurs(p, n)
#define Prolog_Prototype(p, n)     void Prolog_Predicate(p, n)()
#ifdef ENGINE_FILE
int os_argc;
char **os_argv;
char glob_buff[10240];
long *base_fl;			/* overwritten by foreign if present */
double *base_fd;		/* overwritten by foreign if present */
int use_gui;
#else
extern int os_argc;
extern char **os_argv;
extern char glob_buff[];
extern long *base_fl;
extern double *base_fd;
extern int use_gui;
#endif
int Start_Prolog(int argc, char *argv[]);
void Stop_Prolog(void);
void Reset_Prolog(void);
void Reset_Prolog_In_Signal(void);
void Set_Heap_Actual_Start(WamWord *heap_actual_start);
void Execute_Directive(int pl_file, int pl_line, Bool is_system,
		       CodePtr proc);
Bool Try_Execute_Top_Level(void);
Bool Call_Prolog(CodePtr codep);
Bool Call_Prolog_Next_Sol(WamWord *query_b);
void Keep_Rest_For_Prolog(WamWord *query_b);
void Exit_With_Exception(void);
void Execute_A_Continuation(CodePtr codep);
#define   Goto_Predicate(p, n)  ((*Prolog_Predicate(p, n))())
#if 1
#define OPTIM_1_CHAR_ATOM
#endif
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
#define IDENTIFIER_ATOM            0
#define GRAPHIC_ATOM               1
#define SOLO_ATOM                  2
#define OTHER_ATOM                 3
#define Is_Valid_Code(c)           ((unsigned) (c)-1 <256-1)	/* 1<= c <256 */
#define Is_Valid_Byte(c)           ((unsigned) (c) <256)	/* 0=< c <256 */
#define Is_Valid_Atom(a)           ((a)>=0 && (a)<MAX_ATOM && \
                                    atom_tbl[(a)].name!=NULL)
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
char char_conv[256];
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
void Init_Atom(void);
int FC Create_Allocate_Atom(char *name);
int FC Create_Atom(char *name);
WamWord FC Create_Atom_Tagged(char *name);
int FC Find_Atom(char *name);
int FC Gen_New_Atom(char *prefix, int hash);
int FC Find_Next_Atom(int last_atom);
#ifdef OPTIM_1_CHAR_ATOM
#define ATOM_CHAR(c)            ((int) (unsigned char) (c))
#else
#define ATOM_CHAR(c)            (atom_char[(int) (unsigned char) (c)])
#endif
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
#ifdef PRED_FILE
char *pred_tbl;
#else
extern char *pred_tbl;
#endif
void Init_Pred(void);
PredInf * FC Create_Pred(int func, int arity, int pl_file, int pl_line,
		     int prop, long *codep);
PredInf * FC Lookup_Pred(int func, int arity);
void FC Delete_Pred(int func, int arity);
#include <stdlib.h>
char *Malloc_Check(unsigned size, char *src_file, int src_line);
char *Calloc_Check(unsigned nb, unsigned size, char *src_file,
		   int src_line);
char *Realloc_Check(char *ptr, unsigned size, char *src_file, int src_line);
char *Strdup_Check(char *str, char *src_file, int src_line);
#define Malloc(size)       Malloc_Check(size, __FILE__, __LINE__)
#define Calloc(nb, size)   Calloc_Check(nb, size, __FILE__, __LINE__)
#define Realloc(ptr, size) Realloc_Check(ptr, size, __FILE__, __LINE__)
#define Free(ptr)          free(ptr)
#define Strdup(str)        Strdup_Check(str, __FILE__, __LINE__)
void Extend_Table_If_Needed(char **hash_tbl);
void Extend_Array(char **ptbl, int *nb_elem, int elem_size, Bool bzero);
void Exit_With_Value(int ret_val);
void Fatal_Error(char *format, ...);
#define MAX_PREC                   1200
#define MAX_ARG_OF_FUNCTOR_PREC    999
#define Make_Oper_Key(a, t)        (((unsigned long) (a) << 2) | (t))
#define Atom_Of_Oper(k)            ((unsigned long) (k) >> 2)
#define Type_Of_Oper(k)            ((unsigned long) (k) & 3)
#define PREFIX                     0
#define POSTFIX                    1
#define INFIX                      2
#define Make_Op_Mask(type)         (1<<(type))
typedef struct			/* Operator information           */
{				/* ------------------------------ */
  long a_t;			/* key is <atom,operator type>    */
  int prec;			/* precedence of the operator     */
  int left;			/* precedence of the operator lhs */
  int right;			/* precedence of the operator rhs */
}
OperInf;
#ifdef OPER_FILE
char *oper_tbl;
#else
extern char *oper_tbl;
#endif
void Init_Oper(void);
OperInf *Create_Oper(int atom_op, int type, int prec, int left, int right);
OperInf *Lookup_Oper(int atom_op, int type);
OperInf *Lookup_Oper_Any_Type(int atom_op);
OperInf *Delete_Oper(int atom_op, int type);
#define Check_Oper(atom_op, type) \
     (atom_tbl[(atom_op)].prop.op_mask & Make_Op_Mask(type))
#define Check_Oper_Any_Type(atom_op) \
     (atom_tbl[(atom_op)].prop.op_mask)
#include <stdio.h>
#define M_OS_UNIX                  0
#define M_OS_WINDOWS               1
#define M_OS_WINDOWS_NT            2
#ifdef MACHINE1_FILE
int m_os_type;
char m_architecture[32];
char m_os_version[256];
#else
extern int m_os_type;
extern char m_architecture[];
extern char m_os_version[];
#endif
void Init_Machine1(void);
char **M_Create_Shell_Command(char *cmd);
char **M_Cmd_Line_To_Argv(char *cmd, int *argc);
int M_Shell(char *cmd);
int M_Spawn(char *arg[]);
int M_Spawn_Redirect(char *arg[], int detach,
		     FILE **f_in, FILE **f_out, FILE **f_err);
int M_Get_Status(int pid);
char *M_Mktemp(char *tmp_template);
char *M_Tempnam(char *dir, char *pfx);
#define   DBGPRINTF             printf
#ifndef _MACHINE_H
#define _MACHINE_H
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
#if defined(_WIN32) && !defined(__CYGWIN__)
int getpagesize(void);
#endif
void M_Check_Magic_Words(void); /* not compiled if not needed */
#if defined(M_sparc)
#    define M_USED_REGS            {"g6", "g7", 0}
#elif defined(M_mips)
#define M_USED_REGS                {"$16", "$17", "$18", "$19", "$20", \
                                    "$21", "$22", "$23", 0}
#elif defined(M_alpha)
#    define M_USED_REGS            {"$9", "$10", "$11", "$12", "$13", "$14", 0}
#elif defined(M_ix86) && !defined(_MSC_VER) && !defined(M_ix86_darwin)
#ifdef NO_USE_EBP
#    define M_USED_REGS            {"ebx", 0}
#else
#    define M_USED_REGS            {"ebx", "ebp", 0}
#endif
#elif defined(M_powerpc)
#    define M_USED_REGS            {"15", "20", 0}
#elif defined(M_x86_64)
#    define M_USED_REGS            {"r12", "r13", "r14", "r15", 0}
#else
#    define M_USED_REGS            {0}
#endif
#if defined(M_ix86) && !defined(_WIN32) && !defined(NO_USE_REGS)
#define NO_MACHINE_REG_FOR_REG_BANK
#endif
#if WORD_SIZE == 32
#   define M_MMAP_HIGH_ADR1        0x0ffffff0
#   define M_MMAP_HIGH_ADR2        0x3ffffff0
#   define M_MMAP_HIGH_ADR3        0x7ffffff0
#elif defined(M_alpha_osf) || defined(M_alpha_linux)
#   define M_MMAP_HIGH_ADR1        0x3f800000000ULL
#elif defined(M_x86_64_linux) || defined(M_x86_64_solaris)
#   define M_MMAP_HIGH_ADR1        0x4000000000ULL
#endif
#if defined(M_sunos) || defined(M_solaris)
#   define MMAP_NEEDS_FIXED
#endif
#ifdef __OpenBSD__
#define USE_DL_MALLOC
#endif
#endif
void Find_Linked_Objects(void);
void New_Object(void (*fct_obj_init)(), void (*fct_exec_system) (), void (*fct_exec_user) ());
#ifdef OBJ_INIT
static void OBJ_INIT(void);
#define CPP_CAT1(x, y)   x ## y
#define CPP_CAT(x, y)    CPP_CAT1(x, y)
#define OBJ_CTOR  CPP_CAT(OBJ_INIT,_ctor)
#ifdef __GNUC__
static void __attribute__ ((constructor))
OBJ_CTOR(void)
{
  New_Object(OBJ_INIT, NULL, NULL);
}
#else /* _MSC_VER */
static void 
OBJ_CTOR(void)
{
  New_Object(OBJ_INIT, NULL, NULL);
}
#pragma data_seg(".GPLC$m")
static long obj_chain_start = (long) OBJ_CTOR;
#pragma data_seg()
#endif /* _MSC_VER */
#endif /* OBJ_INIT */
#if defined(_MSC_VER) || defined(M_ix86_darwin)
#define OBJ_CHAIN_REVERSE_ORDER
#endif
#if 0
#define GARBAGE_COLLECTOR
#endif
#define NOT_A_WAM_WORD             Tag_REF(0)
#define NIL_WORD                   Tag_ATM(ATOM_NIL)
#define WRITE_MODE                 NULL
#ifdef GARBAGE_COLLECTOR
#define ENVIR_STATIC_SIZE          4
#define CPE(e)                     (*(WamCont *)  &(e[-1]))
#define BCIE(e)                    (*(WamWord *)  &(e[-2]))
#define EE(e)                      (*(WamWord **) &(e[-3]))
#define NBYE(e)                    (*(WamWord *)  &(e[-4]))
#define Y(e, y)                    (*(WamWord *)  &(e[-5 - (y)]))
#define ENVIR_NAMES                {"CPE", "BCIE", "EE", "NBYE"}
#else
#define ENVIR_STATIC_SIZE          3
#define CPE(e)                     (*(WamCont *)  &(e[-1]))
#define BCIE(e)                    (*(WamWord *)  &(e[-2]))
#define EE(e)                      (*(WamWord **) &(e[-3]))
#define Y(e, y)                    (*(WamWord *)  &(e[-4 - (y)]))
#define ENVIR_NAMES                {"CPE", "BCIE", "EE"}
#endif
#define CHOICE_STATIC_SIZE         8
#define ALTB(b)                    (*(CodePtr *)  &(b[-1]))
#define CPB(b)                     (*(WamCont *)  &(b[-2]))
#define BCIB(b)                    (*(WamWord *)  &(b[-3]))
#define EB(b)                      (*(WamWord **) &(b[-4]))
#define BB(b)                      (*(WamWord **) &(b[-5]))
#define HB(b)                      (*(WamWord **) &(b[-6]))
#define TRB(b)                     (*(WamWord **) &(b[-7]))
#define CSB(b)                     (*(WamWord **) &(b[-8]))
#define AB(b, a)                   (*(WamWord *)  &(b[-9 - (a)]))
#define CHOICE_NAMES               {"ALTB", "CPB", "BCIB", "EB", "BB", \
                                    "HB", "TRB", "CSB"}
#define NB_OF_TRAIL_TAGS           4
#define TUV                        0	/* Trail Unbound Variable   */
#define TOV                        1	/* Trail One Value          */
#define TMV                        2	/* Trail Multiple Values    */
#define TFC                        3	/* Trail for Function Call  */
#define TRAIL_TAG_NAMES            {"TUV", "TOV", "TMV", "TFC"}
#define Trail_Tag_Value(t, v)      ((unsigned long) (v) | (t))
#define Trail_Tag_Of(w)            ((unsigned long) (w) & 0x3)
#define Trail_Value_Of(w)          ((unsigned long) (w) & (~0x3))
#define Functor_Arity(f, n)        (((n) << ATOM_SIZE) + (f))
#define Functor_Of(word)           ((word) & (MAX_ATOM - 1))
#define Arity_Of(word)             ((word) >> ATOM_SIZE)
#ifndef NO_USE_FD_SOLVER
#define Dont_Separate_Tag(tag_mask) ((tag_mask) == TAG_FDV_MASK)
#else
#define Dont_Separate_Tag(tag_mask) (0)
#endif
#define Do_Copy_Of_Word(tag_mask, word) \
  if (Dont_Separate_Tag(tag_mask)) \
    word = Tag_REF(UnTag_Address(word))
#define Make_Self_Ref(adr)         (Tag_REF(adr))
#define INT_GREATEST_VALUE         ((long) ((1L<<(WORD_SIZE-TAG_SIZE-1))-1))
#define INT_LOWEST_VALUE           ((long) ((-INT_GREATEST_VALUE)-1))
#define OFFSET_CAR                 0
#define Car(adr)                   (((WamWord *) adr)[OFFSET_CAR])
#define Cdr(adr)                   (((WamWord *) adr)[OFFSET_CAR+1])
#define OFFSET_ARG                 1
#define Functor(adr)               (Functor_Of(Functor_And_Arity(adr)))
#define Arity(adr)                 (Arity_Of(Functor_And_Arity(adr)))
#define Functor_And_Arity(adr)     (((WamWord *) (adr))[0])
#define Arg(adr, i)                (((WamWord *) (adr))[OFFSET_ARG+i])
#define Global_Push(word)          (*H++ = (WamWord) (word))
#define Global_Pop                 (*--H)
#define Trail_Push(word)           (*TR++ = (WamWord) (word))
#define Trail_Pop                  (*--TR)
#define Is_A_Local_Adr(adr)        ((adr) >= LSSA)
#define From_B_To_WamWord(b)       (Tag_INT((b) - LSSA))
#define From_WamWord_To_B(word)    (LSSA + UnTag_INT(word))
#ifdef M_sparc
#define Adjust_CP(cp)              ((WamCont) ((unsigned long) (cp) - 8))
#define UnAdjust_CP(cp)            ((WamCont) ((unsigned long) (cp) + 8))
#else
#define Adjust_CP(p)               ((WamCont) (p))
#define UnAdjust_CP(cp)            (cp)
#endif
#ifndef FRAMES_ONLY
typedef struct			/* Switch item information         */
{				/* ------------------------------- */
  long key;			/* key: atm, int (if no_opt), f/n  */
  CodePtr codep;		/* compiled code pointer if static */
}
SwtInf;
typedef SwtInf *SwtTbl;
WamWord FC Create_Functor_Arity_Tagged(char *func_str, int arity);
SwtTbl FC Create_Swt_Table(int size);
void FC Create_Swt_Atm_Element(SwtTbl t, int size, int atom, CodePtr codep);
void FC Create_Swt_Stc_Element(SwtTbl t, int size, int func, int arity,
			    CodePtr codep);
Bool FC Get_Atom_Tagged(WamWord w, WamWord start_word);
Bool FC Get_Atom(int atom, WamWord start_word);
Bool FC Get_Integer_Tagged(WamWord w, WamWord start_word);
Bool FC Get_Integer(long n, WamWord start_word);
Bool FC Get_Float(double n, WamWord start_word);
Bool FC Get_Nil(WamWord start_word);
Bool FC Get_List(WamWord start_word);
Bool FC Get_Structure_Tagged(WamWord w, WamWord start_word);
Bool FC Get_Structure(int func, int arity, WamWord start_word);
WamWord FC Put_X_Variable(void);
WamWord FC Put_Y_Variable(WamWord *y_adr);
WamWord FC Put_Unsafe_Value(WamWord start_word);
WamWord FC Put_Atom_Tagged(WamWord w);
WamWord FC Put_Atom(int atom);
WamWord FC Put_Integer_Tagged(WamWord w);
WamWord FC Put_Integer(long n);
WamWord FC Put_Float(double n);
WamWord FC Put_Nil(void);
WamWord FC Put_List(void);
WamWord FC Put_Structure_Tagged(WamWord w);
WamWord FC Put_Structure(int func, int arity);
WamWord FC Unify_Variable(void);
void FC Unify_Void(int n);
Bool FC Unify_Value(WamWord start_word);
Bool FC Unify_Local_Value(WamWord start_word);
Bool FC Unify_Atom_Tagged(WamWord w);
Bool FC Unify_Atom(int atom);
Bool FC Unify_Integer_Tagged(WamWord w);
Bool FC Unify_Integer(long n);
Bool FC Unify_Nil(void);
Bool FC Unify_List(void);
Bool FC Unify_Structure_Tagged(WamWord w);
Bool FC Unify_Structure(int func, int arity);
WamWord FC Globalize_If_In_Local(WamWord start_word);
void FC Allocate(int n);
void FC Deallocate(void);
CodePtr FC Switch_On_Term(CodePtr c_var, CodePtr c_atm, CodePtr c_int,
		          CodePtr c_lst, CodePtr c_stc);
CodePtr FC Switch_On_Term_Var_Atm(CodePtr c_var, CodePtr c_atm);
CodePtr FC Switch_On_Term_Var_Stc(CodePtr c_var, CodePtr c_stc);
CodePtr FC Switch_On_Term_Var_Atm_Lst(CodePtr c_var, CodePtr c_atm,
				      CodePtr c_lst);
CodePtr FC Switch_On_Term_Var_Atm_Stc(CodePtr c_var, CodePtr c_atm,
				      CodePtr c_stc);
CodePtr FC Switch_On_Atom(SwtTbl t, int size);
long FC Switch_On_Integer(void);
CodePtr FC Switch_On_Structure(SwtTbl t, int size);
void FC Load_Cut_Level(WamWord *word_adr);
void FC Cut(WamWord b_word);
void FC Global_Push_Float(double n);
double FC Obtain_Float(WamWord *adr);
void FC Create_Choice_Point(CodePtr codep_alt, int arity);
void FC Create_Choice_Point1(CodePtr codep_alt);
void FC Create_Choice_Point2(CodePtr codep_alt);
void FC Create_Choice_Point3(CodePtr codep_alt);
void FC Create_Choice_Point4(CodePtr codep_alt);
void FC Update_Choice_Point(CodePtr codep_alt, int arity);
void FC Update_Choice_Point1(CodePtr codep_alt);
void FC Update_Choice_Point2(CodePtr codep_alt);
void FC Update_Choice_Point3(CodePtr codep_alt);
void FC Update_Choice_Point4(CodePtr codep_alt);
void FC Delete_Choice_Point(int arity);
void FC Delete_Choice_Point1(void);
void FC Delete_Choice_Point2(void);
void FC Delete_Choice_Point3(void);
void FC Delete_Choice_Point4(void);
void FC Untrail(WamWord *low_adr);
Bool FC Unify(WamWord start_u_word, WamWord start_v_word);
Bool FC Unify_Occurs_Check(WamWord start_u_word, WamWord start_v_word);
#endif /* FRAME_ONLY */
#if 0
#define DEREF_STATS
#endif
#ifdef DEREF_STATS
long nb_deref;
long chain_len;
#define DEREF_COUNT(x)  x++
#else
#define DEREF_COUNT(x)
#endif
#define DEREF(start_word, word, tag_mask)	\
  do						\
    {						\
      WamWord deref_last_word;			\
						\
      word = start_word;			\
						\
      DEREF_COUNT(nb_deref);			\
      do					\
	{					\
	  DEREF_COUNT(chain_len);		\
	  deref_last_word = word;		\
	  tag_mask = Tag_Mask_Of(word);		\
	  if (tag_mask != TAG_REF_MASK)		\
	    break;				\
	  word = *(UnTag_REF(word));		\
	}					\
      while (word != deref_last_word);		\
    }						\
  while (0)
#define Word_Needs_Trailing(adr)			\
  ((adr) < HB1 || (Is_A_Local_Adr(adr) && (adr) < B))
#define Bind_UV(adr, word)			\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_UV(adr);				\
      *(adr) = (word);				\
    }						\
  while (0)
#define Bind_OV(adr, word)			\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_OV(adr);				\
      *(adr) = (word);				\
    }						\
  while (0)
#define Bind_MV(adr, nb, real_adr)		\
  do						\
    {						\
      if (Word_Needs_Trailing(adr))		\
	Trail_MV(adr, nb);			\
      Mem_Word_Cpy(adr, real_adr, nb);		\
    }						\
  while (0)
#define Trail_UV(adr)				\
  Trail_Push(Trail_Tag_Value(TUV, adr))
#define Trail_OV(adr)				\
  do						\
    {						\
      Trail_Push(*(adr));			\
      Trail_Push(Trail_Tag_Value(TOV, adr));	\
    }						\
  while (0)
#define Trail_MV(adr, nb)			\
  do						\
    {						\
      Mem_Word_Cpy(TR, adr, nb);		\
      TR += nb;					\
      Trail_Push(nb);				\
      Trail_Push(Trail_Tag_Value(TMV, adr));	\
    }						\
  while (0)
#define Trail_FC(fct, nb, arg)			\
  do						\
    {						\
      Mem_Word_Cpy(TR, arg, nb);		\
      TR += nb;					\
      Trail_Push(nb);				\
      Trail_Push(fct);	/*fct adr not aligned*/	\
      Trail_Push(Trail_Tag_Value(TFC, 0));	\
    }						\
  while (0)
#define Assign_B(newB)              (B = (newB), HB1 = HB(B))
#define Delete_Last_Choice_Point()  Assign_B(BB(B))
#define Globalize_Local_Unbound_Var(adr, res_word)	\
  do							\
    {							\
      WamWord *cur_H = H;				\
							\
      res_word = Make_Self_Ref(cur_H);			\
      *cur_H = res_word;				\
      H++;						\
      Bind_UV(adr, res_word);				\
    }							\
  while (0)
#define Mem_Word_Cpy(dst, src, nb)		\
  do						\
    {						\
      register long *s = (long *) (src);	\
      register long *d = (long *) (dst);	\
      register int counter = (nb);		\
						\
      do					\
	*d++ = *s++;				\
      while (--counter);			\
    }						\
  while (0)
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
void Fd_Init_Solver(void);
void Fd_Reset_Solver(void);
#define Fd_Unify_With_Integer(f, n) ((*fd_unify_with_integer)(f, n))
#define Fd_Unify_With_Fd_Var(f1, f2)((*fd_unify_with_fd_var)(f1, f2))
#define Fd_Variable_Size(f)         ((*fd_variable_size)(f))
#define Fd_Copy_Variable(dst_adr, f)((*fd_copy_variable)(dst_adr, f))
#define Fd_Variable_To_String(f)    ((*fd_variable_to_string)(f))
#define MAX_VAR_NAME_LENGTH        256
#define MAX_VAR_IN_TERM            2048
#define MAX_SYS_VARS               256
Bool FC Blt_Var(WamWord x);
Bool FC Blt_Non_Var(WamWord x);
Bool FC Blt_Atom(WamWord x);
Bool FC Blt_Integer(WamWord x);
Bool FC Blt_Float(WamWord x);
Bool FC Blt_Number(WamWord x);
Bool FC Blt_Atomic(WamWord x);
Bool FC Blt_Compound(WamWord x);
Bool FC Blt_Callable(WamWord x);
Bool FC Blt_Fd_Var(WamWord x);
Bool FC Blt_Non_Fd_Var(WamWord x);
Bool FC Blt_Generic_Var(WamWord x);
Bool FC Blt_Non_Generic_Var(WamWord x);
Bool FC Blt_List(WamWord x);
Bool FC Blt_Partial_List(WamWord x);
Bool FC Blt_List_Or_Partial_List(WamWord x);
Bool FC Blt_Term_Eq(WamWord x, WamWord y);
Bool FC Blt_Term_Neq(WamWord x, WamWord y);
Bool FC Blt_Term_Lt(WamWord x, WamWord y);
Bool FC Blt_Term_Lte(WamWord x, WamWord y);
Bool FC Blt_Term_Gt(WamWord x, WamWord y);
Bool FC Blt_Term_Gte(WamWord x, WamWord y);
Bool FC Blt_Compare(WamWord cmp_word, WamWord x, WamWord y);
Bool FC Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word);
Bool FC Blt_Functor(WamWord term_word, WamWord functor_word,
		    WamWord arity_word);
Bool FC Blt_Univ(WamWord term_word, WamWord list_word);
void FC Blt_G_Assign(WamWord x, WamWord y);
void FC Blt_G_Assignb(WamWord x, WamWord y);
void FC Blt_G_Link(WamWord x, WamWord y);
Bool FC Blt_G_Read(WamWord x, WamWord y);
Bool FC Blt_G_Array_Size(WamWord x, WamWord y);
void FC Blt_G_Inc(WamWord x);
Bool FC Blt_G_Inco(WamWord x, WamWord y);
Bool FC Blt_G_Inc_2(WamWord x, WamWord y);
Bool FC Blt_G_Inc_3(WamWord x, WamWord y, WamWord z);
void FC Blt_G_Dec(WamWord x);
Bool FC Blt_G_Deco(WamWord x, WamWord y);
Bool FC Blt_G_Dec_2(WamWord x, WamWord y);
Bool FC Blt_G_Dec_3(WamWord x, WamWord y, WamWord z);
void FC Blt_G_Set_Bit(WamWord x, WamWord y);
void FC Blt_G_Reset_Bit(WamWord x, WamWord y);
Bool FC Blt_G_Test_Set_Bit(WamWord x, WamWord y);
Bool FC Blt_G_Test_Reset_Bit(WamWord x, WamWord y);
void FC Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr);
void FC Math_Load_Value(WamWord start_word, WamWord *word_adr);
WamWord FC Fct_Fast_Neg(WamWord x);
WamWord FC Fct_Fast_Inc(WamWord x);
WamWord FC Fct_Fast_Dec(WamWord x);
WamWord FC Fct_Fast_Add(WamWord x, WamWord y);
WamWord FC Fct_Fast_Sub(WamWord x, WamWord y);
WamWord FC Fct_Fast_Mul(WamWord x, WamWord y);
WamWord FC Fct_Fast_Div(WamWord x, WamWord y);
WamWord FC Fct_Fast_Rem(WamWord x, WamWord y);
WamWord FC Fct_Fast_Mod(WamWord x, WamWord y);
WamWord FC Fct_Fast_And(WamWord x, WamWord y);
WamWord FC Fct_Fast_Or(WamWord x, WamWord y);
WamWord FC Fct_Fast_Xor(WamWord x, WamWord y);
WamWord FC Fct_Fast_Not(WamWord x);
WamWord FC Fct_Fast_Shl(WamWord x, WamWord y);
WamWord FC Fct_Fast_Shr(WamWord x, WamWord y);
WamWord FC Fct_Fast_Abs(WamWord x);
WamWord FC Fct_Fast_Sign(WamWord x);
WamWord FC Fct_Neg(WamWord x);
WamWord FC Fct_Inc(WamWord x);
WamWord FC Fct_Dec(WamWord x);
WamWord FC Fct_Add(WamWord x, WamWord y);
WamWord FC Fct_Sub(WamWord x, WamWord y);
WamWord FC Fct_Mul(WamWord x, WamWord y);
WamWord FC Fct_Div(WamWord x, WamWord y);
WamWord FC Fct_Float_Div(WamWord x, WamWord y);
WamWord FC Fct_Rem(WamWord x, WamWord y);
WamWord FC Fct_Mod(WamWord x, WamWord y);
WamWord FC Fct_And(WamWord x, WamWord y);
WamWord FC Fct_Or(WamWord x, WamWord y);
WamWord FC Fct_Xor(WamWord x, WamWord y);
WamWord FC Fct_Not(WamWord x);
WamWord FC Fct_Shl(WamWord x, WamWord y);
WamWord FC Fct_Shr(WamWord x, WamWord y);
WamWord FC Fct_Abs(WamWord x);
WamWord FC Fct_Sign(WamWord x);
WamWord FC Fct_Min(WamWord x, WamWord y);
WamWord FC Fct_Max(WamWord x, WamWord y);
WamWord FC Fct_Pow(WamWord x, WamWord y);
WamWord FC Fct_Sqrt(WamWord x);
WamWord FC Fct_Atan(WamWord x);
WamWord FC Fct_Cos(WamWord x);
WamWord FC Fct_Acos(WamWord x);
WamWord FC Fct_Sin(WamWord x);
WamWord FC Fct_Asin(WamWord x);
WamWord FC Fct_Exp(WamWord x);
WamWord FC Fct_Log(WamWord x);
WamWord FC Fct_Float(WamWord x);
WamWord FC Fct_Ceiling(WamWord x);
WamWord FC Fct_Floor(WamWord x);
WamWord FC Fct_Round(WamWord x);
WamWord FC Fct_Truncate(WamWord x);
WamWord FC Fct_Float_Fract_Part(WamWord x);
WamWord FC Fct_Float_Integ_Part(WamWord x);
WamWord FC Fct_Identity(WamWord x);
Bool FC Blt_Fast_Eq(WamWord x, WamWord y);
Bool FC Blt_Fast_Neq(WamWord x, WamWord y);
Bool FC Blt_Fast_Lt(WamWord x, WamWord y);
Bool FC Blt_Fast_Lte(WamWord x, WamWord y);
Bool FC Blt_Fast_Gt(WamWord x, WamWord y);
Bool FC Blt_Fast_Gte(WamWord x, WamWord y);
Bool FC Blt_Eq(WamWord x, WamWord y);
Bool FC Blt_Neq(WamWord x, WamWord y);
Bool FC Blt_Lt(WamWord x, WamWord y);
Bool FC Blt_Lte(WamWord x, WamWord y);
Bool FC Blt_Gt(WamWord x, WamWord y);
Bool FC Blt_Gte(WamWord x, WamWord y);
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
Bool Un_Term(WamWord term_word, WamWord start_word);
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
#define PL_RECOVER                 0
#define PL_CUT                     1
#define PL_KEEP_FOR_PROLOG         2
#define PL_FAILURE                 FALSE
#define PL_SUCCESS                 TRUE
#define PL_EXCEPTION               2
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
#ifdef FOREIGN_SUPP_FILE
int foreign_bkt_counter;
char *foreign_bkt_buffer;
#else
extern int foreign_bkt_counter;
extern char *foreign_bkt_buffer;
#endif
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
void Pl_Query_Begin(Bool recoverable);
int Pl_Query_Call(int func, int arity, WamWord *arg_adr);
#define Pl_Query_Start(func, arity, arg_adr, recoverable) \
 (Pl_Query_Begin(recoverable), Pl_Query_Call(func, arity, arg_adr))
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
#define Stream_Pointer(s)      (stm_tbl + (s))
#define MASK_PRED_NATIVE_CODE      1
#define MASK_PRED_DYNAMIC          2
#define MASK_PRED_PUBLIC           4
#define MASK_PRED_BUILTIN          8
#define MASK_PRED_BUILTIN_FD       16
#define MASK_PRED_ANY_BUILTIN      (MASK_PRED_BUILTIN | MASK_PRED_BUILTIN_FD)
char *Detect_If_Aux_Name(int func);
int Father_Pred_Of_Aux(int func, int *father_arity);
int Pred_Without_Aux(int func, int arity, int *arity1);
int Make_Aux_Name(int func, int arity, int aux_nb);
#ifdef TERM_SUPP_FILE
WamWord pi_name_word;
WamWord pi_arity_word;
long glob_dico_var[MAX_VAR_IN_TERM];	/* a general purpose dico */
#else
extern WamWord pi_name_word;
extern WamWord pi_arity_word;
extern long glob_dico_var[];
#endif
long Term_Compare(WamWord start_u_word, WamWord start_v_word);
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
#include <stdio.h>
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
#define STREAM_CHECK_VALID         0	/* simply a valid stream */
#define STREAM_CHECK_EXIST         1	/* valid and exist */
#define STREAM_CHECK_INPUT         2	/* valid, exist and mode=input  */
#define STREAM_CHECK_OUTPUT        3	/* valid, exist and mode=output */
#define STREAM_FCT_UNDEFINED       ((StmFct) (-1)) /* for optional fct */
#define TERM_STREAM_ATOM           1 /* values also used in stream.pl */
#define TERM_STREAM_CHARS          2
#define TERM_STREAM_CODES          3
typedef struct			/* Stream properties              */
{				/* ------------------------------ */
  unsigned mode:2;		/* see STREAM_MODE_xxx defs       */
  unsigned input:1;		/* is it an input  stream ?       */
  unsigned output:1;		/* is it an output stream ?       */
  unsigned text:1;		/* is it a text stream . (or bin) */
  unsigned reposition:1;	/* can it be repositioned ?       */
  unsigned eof_action:2;	/* see STREAM_EOF_ACTION_xxx defs */
  unsigned buffering:2;		/* see STREAM_BUFFERING_xxx defs  */
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
typedef struct stm_lst *PStmLst;
typedef struct stm_lst		/* Chained stream list            */
{				/* ------------------------------ */
  int stm;			/* the stream                     */
  PStmLst next;			/* next entry                     */
} StmLst;
typedef struct stm_inf		/* Stream information             */
{				/* ------------------------------ */
  int atom_file_name;		/* atom associated to filename    */
  long file;			/* accessor (FILE *,TTYInf *) != 0*/
  StmProp prop;			/* assoctiated properties         */
  StmLst *mirror;		/* mirror streams                 */
  StmLst *mirror_of;            /* streams this stream as mirror  */
  StmFct fct_getc;		/* get char function (mandatory)  */
  StmFct fct_putc;		/* put char function (mandatory)  */
  StmFct fct_flush;		/* flush    function (optional)   */
  StmFct fct_close;		/* close    function (optional)   */
  StmFct fct_tell;		/* tell     function (optional)   */
  StmFct fct_seek;		/* seek     function (optional)   */
  StmFct fct_clearerr;		/* clearerr function (optional)   */
  Bool eof_reached;		/* has eof char been read ?       */
  PbStk pb_char;		/* character push back stack      */
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
#ifdef STREAM_SUPP_FILE
StmInf **stm_tbl;
int stm_tbl_size;
int stm_last_used;
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
int use_le_prompt;
int atom_stream;
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
extern StmInf **stm_tbl;
extern int stm_tbl_size;
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
extern int use_le_prompt;
extern int atom_stream;
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
int Add_Stream(int atom_file_name, long file, StmProp prop,
	       StmFct fct_getc, StmFct fct_putc,
	       StmFct fct_flush, StmFct fct_close,
	       StmFct fct_tell, StmFct fct_seek, StmFct fct_clearerr);
int Add_Stream_For_Stdio_Desc(FILE *f, int atom_path, int mode, int text);
int Add_Stream_For_Stdio_File(char *path, int mode, Bool text);
void Delete_Stream(int stm);
int Find_Stream_By_Alias(int atom_alias);
Bool Add_Alias_To_Stream(int atom_alias, int stm);
void Reassign_Alias(int atom_alias, int stm);
void Add_Mirror_To_Stream(int stm, int m_stm);
Bool Del_Mirror_From_Stream(int stm, int m_stm);
int Find_Stream_From_PStm(StmInf *pstm);
void Flush_All_Streams(void);
void Set_Stream_Buffering(int stm);
int Get_Stream_Or_Alias(WamWord sora_word, int test_mask);
void Check_Stream_Type(int stm, Bool check_text, Bool for_input);
WamWord Make_Stream_Tagged_Word(int stm);
Bool Stdio_Is_Repositionable(FILE *f);
void Stdio_Set_Buffering(FILE *f, int buffering);
FILE *Stdio_Desc_Of_Stream(int stm);
int Io_Fileno_Of_Stream(int stm);
int Stream_Getc(StmInf *pstm);
int Stream_Get_Key(StmInf *pstm, Bool echo, Bool catch_ctrl_c);
void Stream_Ungetc(int c, StmInf *pstm);
int Stream_Peekc(StmInf *pstm);
char *Stream_Gets(char *str, int size, StmInf *pstm);
char *Stream_Gets_Prompt(char *prompt, StmInf *pstm_o,
			 char *str, int size, StmInf *pstm_i);
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
int Add_Str_Stream(char *buff, int prop_other);
void Delete_Str_Stream(int stm);
char *Term_Write_Str_Stream(int stm);
void Close_Stm(int stm, Bool force); /* from close_c.c */
#define PB_Init(pb)          pb.ptr = pb.buff, pb.nb_elems = 0;
#define PB_Is_Empty(pb)      (pb.nb_elems == 0)
#define PB_Push(pb, elem)                  		\
  do							\
    {							\
      *(pb.ptr) = (elem);				\
      if (pb.ptr != pb.buff + STREAM_PB_SIZE - 1)	\
	pb.ptr++;					\
      else						\
	pb.ptr = pb.buff;				\
      if (pb.nb_elems < STREAM_PB_SIZE)			\
	pb.nb_elems++;					\
    }							\
  while (0)
#define PB_Pop(pb, elem)                    	\
  do						\
    {						\
      if (pb.ptr != pb.buff)			\
	pb.ptr--;				\
      else					\
	pb.ptr = pb.buff + STREAM_PB_SIZE - 1;	\
      (elem) = *pb.ptr;				\
      pb.nb_elems--;				\
    }						\
  while (0)
#define PB_Top(pb, elem)                    	\
  do						\
    {						\
      if (pb.ptr != pb.buff)			\
	(elem) = pb.ptr[-1];			\
      else					\
	(elem) = pb.buff[STREAM_PB_SIZE - 1];	\
    }						\
  while (0)
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
int domain_g_argument_selector;	/* for g_vars */
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
int existence_sr_descriptor;	/* for source reader */
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
int evluation_float_overflow;
int evluation_int_overflow;
int evluation_undefined;
int evluation_underflow;
int evluation_zero_divisor;
int resource_print_object_not_linked; /* for print and format */
int resource_too_big_fd_constraint; /* for FD */
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
extern int domain_g_argument_selector;	/* for g_vars */
extern int domain_stream_seek_method;	/* for seek/4 */
extern int domain_format_control_sequence;	/* for format/2-3 */
extern int domain_os_path;	/* for absolute_file_name/2 */
extern int domain_os_file_permission; /* for file_permission/2 */
extern int domain_selectable_item; /* for select_read/3 */
extern int domain_date_time;	/* for os_interf */
#ifndef NO_USE_SOCKETS
extern int domain_socket_domain; /* for sockets */
extern int domain_socket_address; /* for sockets */
#endif
extern int existence_procedure;
extern int existence_source_sink;
extern int existence_stream;
extern int existence_sr_descriptor; /* for source reader */
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
extern int evluation_float_overflow;
extern int evluation_int_overflow;
extern int evluation_undefined;
extern int evluation_underflow;
extern int evluation_zero_divisor;
extern int resource_too_many_open_streams; /* for streams */
extern int resource_print_object_not_linked; /* for print and format */
extern int resource_too_big_fd_constraint; /* for FD */
#endif
void Set_Bip_Name_2(WamWord func_word, WamWord arity_word);
void FC Set_C_Bip_Name(char *func_str, int arity);
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
#define SCAN_BIG_BUFFER            10240
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
#ifdef SCAN_SUPP_FILE
TokInf token;
#else
extern TokInf token;
#endif
int Scan_Peek_Char(StmInf *pstm, Bool convert);
char *Scan_Token(StmInf *pstm, Bool comma_is_punct);
void Recover_After_Error(StmInf *pstm);
char *Scan_Next_Atom(StmInf *pstm);
char *Scan_Next_Number(StmInf *pstm, Bool integer_only);
#define PARSE_END_OF_TERM_DOT      0
#define PARSE_END_OF_TERM_EOF      1
typedef struct			/* Parsed variable information    */
{				/* ------------------------------ */
  char name[MAX_VAR_NAME_LENGTH]; /* variable name                */
  WamWord word;			/* associated WAM word            */
  Bool named;			/* has it a name ?                */
  int nb_of_uses;		/* occurrence counter             */
}
InfVar;
#ifdef PARSE_SUPP_FILE
InfVar parse_dico_var[MAX_VAR_IN_TERM];
int parse_nb_var;
#else
extern InfVar parse_dico_var[];
extern int parse_nb_var;
#endif
int last_read_line;
int last_read_col;
WamWord Read_Term(StmInf *pstm, int parse_end_of_term);
WamWord Read_Atom(StmInf *pstm);
WamWord Read_Integer(StmInf *pstm);
WamWord Read_Number(StmInf *pstm);
WamWord Read_Token(StmInf *pstm);
#define WRITE_QUOTED                1
#define WRITE_IGNORE_OP             2
#define WRITE_NUMBER_VARS           4
#define WRITE_NAME_VARS             8
#define WRITE_SPACE_ARGS           16
#define WRITE_PORTRAYED            32
#ifdef WRITE_SUPP_FILE
int last_writing;
#else
extern int last_writing;
#endif
void Write_Term(StmInf *pstm, int depth, int prec, int mask,
		WamWord term_word);
void Write_Simple(WamWord term_word);
void Write_A_Char(StmInf *pstm, int c);
char *Float_To_String(double d);
int Get_Print_Stm(void);
enum
{
  FLAG_BOUNDED,
  FLAG_MAX_INTEGER,
  FLAG_MIN_INTEGER,
  FLAG_ROUNDING_FCT,
  FLAG_CHAR_CONVERSION,
  FLAG_DEBUG,
  FLAG_MAX_ARITY,
  FLAG_UNKNOWN,
  FLAG_DOUBLE_QUOTES,
  FLAG_BACK_QUOTES,
  FLAG_SYNTAX_ERROR,
  FLAG_OS_ERROR,
  FLAG_MAX_ATOM,
  FLAG_MAX_UNGET,
  FLAG_SINGLETON_WARNING,
  FLAG_STRICT_ISO,
  FLAG_PROLOG_NAME,
  FLAG_PROLOG_VERSION,
  FLAG_PROLOG_DATE,
  FLAG_PROLOG_COPYRIGHT,
  NB_OF_FLAGS			/* this gives us the number of used flags */
};
#define FLAG_VALUE_ERROR           0	/* same order as in read.pl */
#define FLAG_VALUE_WARNING         1
#define FLAG_VALUE_FAIL            2
#define FLAG_AS_CODES              0	/* bit 2 is set if no_escape */	
#define FLAG_AS_CHARS              1
#define FLAG_AS_ATOM               2
#define FLAG_NO_ESCAPE_BIT         2
#define FLAG_AS_PART_MASK          ((1 << FLAG_NO_ESCAPE_BIT) - 1)
#define FLAG_NO_ESCAPE_MASK        (1 << FLAG_NO_ESCAPE_BIT)
#define Char_Conversion(c)         ((Flag_Value(FLAG_CHAR_CONVERSION) &&    \
                                    Is_Valid_Code(c)) ? char_conv[c] : (c))
#define SYS_VAR_OPTION_MASK         (sys_var[0])
#define SYS_VAR_WRITE_DEPTH         (sys_var[1])
#define SYS_VAR_SYNTAX_ERROR_ACTON  (sys_var[1])
#define SYS_VAR_WRITE_PREC          (sys_var[2])
#define SYS_VAR_FD_BCKTS            (sys_var[3])
#define SYS_VAR_TOP_LEVEL           (sys_var[10])
#define SYS_VAR_LINEDIT             (sys_var[12])
#define SYS_VAR_DEBUGGER            (sys_var[13])
#define SYS_VAR_SAY_GETC            (sys_var[20])
#define CHAR_TO_EMIT_WHEN_CHAR      '\1'
#define Flag_Value(flag)            (sys_var[200 + (flag)])
#ifdef FLAG_C_FILE
long sys_var[MAX_SYS_VARS];
#else
extern long sys_var[];
#endif
Bool Read_Pl_State_File(WamWord file_word);
Bool Write_Pl_State_File(WamWord file_word);
#define DYN_ALT_FCT_FOR_TEST       0
#define DYN_ALT_FCT_FOR_JUMP       1
typedef long (*ScanFct) ();
typedef unsigned long DynStamp;
typedef struct dynpinf *DynPInfP;
typedef struct dyncinf *DynCInfP;
typedef struct			/* Dobly-linked chain header     */
{				/* ----------------------------- */
  DynCInfP first;		/* first clause (or NULL)        */
  DynCInfP last;		/* last  clause (or NULL)        */
}D2ChHdr;
typedef struct			/* Dobly-linked chain cell       */
{				/* ----------------------------- */
  DynCInfP next;		/* next     clause (or NULL)     */
  DynCInfP prev;		/* previous clause (or NULL)     */
}D2ChCell;
typedef struct dyncinf		/* Dynamic clause information     */
{				/* ------------------------------ */
  D2ChCell seq_chain;		/* sequential chain               */
  D2ChCell ind_chain;		/* indexical  chain               */
  DynPInfP dyn;			/* back ptr to associated dyn inf */
  D2ChHdr *p_ind_hdr;		/* back ptr to ind_chain header   */
  char **p_ind_htbl;		/* back ptr to ind htbl (or NULL) */
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
  D2ChHdr ind_chain;		/* indexical chain                */
}
DSwtInf;
typedef struct dynpinf		/* Dynamic predicate information  */
{				/* ------------------------------ */
  D2ChHdr seq_chain;		/* sequential chain               */
  D2ChHdr var_ind_chain;	/* index if 1st arg=VAR (chain)   */
  char *atm_htbl;		/* index if 1st arg=ATM (htable)  */
  char *int_htbl;		/* index if 1st arg=INT (htable)  */
  D2ChHdr lst_ind_chain;	/* index if 1st arg=LST (chain)   */
  char *stc_htbl;		/* index if 1st arg=STC (htable)  */
  int arity;			/* arity (redundant but faster)   */
  int count_a;			/* next clause nb for asserta     */
  int count_z;			/* next clause nb for assertz     */
  DynCInfP first_erased_cl;	/* 1st erased clause NULL if none */
  DynPInfP next_dyn_with_erase;	/* next dyn with erased clauses   */
}
DynPInf;
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
void Call_Info_Bip_Name_1(WamWord call_info_word);
#define Call_Info(f, a, dc)        ((Functor_Arity(f, a) << 1) | dc)
#ifdef BC_SUPP_FILE
int byte_len;
#else
extern int byte_len;
#endif
unsigned *byte_code;
WamCont BC_Emulate_Pred(int func, DynPInf *dyn);
#ifndef NO_USE_FD_SOLVER
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
#define RANGE_TOP_STACK            CS
#define INTERVAL_MAX_INTEGER       ((int)((1L<<(32-TAG_SIZE-1))-1))	/* only 32 bits */
#ifndef WORD_SIZE
#   define WORD_SIZE               32
#endif
#if WORD_SIZE == 32
#   define WORD_SIZE_BITS          5
#else
#   define WORD_SIZE_BITS          6
#endif
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
#define Word_No_And_Bit_No(w, b)   (((VecWord) (w) << WORD_SIZE_BITS)|\
                                     (VecWord) (b))
#define Word_No(n)                 ((VecWord) (n) >> WORD_SIZE_BITS)
#define Bit_No(n)                  ((n) & (((VecWord) 1 << WORD_SIZE_BITS)-1))
#define Vector_Test_Value(vec, n)  ((vec[Word_No(n)] & ((VecWord) 1 << Bit_No(n))) != 0)
#define Vector_Set_Value(vec, n)   (vec[Word_No(n)] |= ((VecWord) 1 << Bit_No(n)))
#define Vector_Reset_Value(vec, n) (vec[Word_No(n)] &= ~((VecWord) 1 << Bit_No(n)))
#define Vector_Allocate_If_Necessary(vec)	\
  do						\
    {						\
      if (vec == NULL)				\
	Vector_Allocate(vec);			\
    }						\
  while (0)
#define Vector_Allocate(vec)       		\
  do						\
    {						\
      vec = (Vector) RANGE_TOP_STACK;		\
      RANGE_TOP_STACK += vec_size;		\
    }						\
  while (0)
#define VECTOR_BEGIN_ENUM(vec, vec_elem)                              	  \
{									  \
  Vector enum_end = vec + vec_size, enum_i = vec;			  \
  int enum_j;								  \
  VecWord enum_word;							  \
									  \
  vec_elem = 0;								  \
  do									  \
    {									  \
      enum_word = *enum_i;						  \
      for (enum_j = 0; enum_j++ < WORD_SIZE; enum_word >>= 1, vec_elem++) \
	{								  \
	  if (enum_word & 1)						  \
	    {
#define VECTOR_END_ENUM                                              	\
	    }								\
	}								\
    }									\
  while (++enum_i < enum_end);						\
}
#define Is_Interval(range)         ((range)->vec == NULL)
#define Is_Sparse(range)           ((range)->vec != NULL)
#define Is_Empty(range)            ((range)->min >  (range)->max)
#define Is_Not_Empty(range)        ((range)->max >= (range)->min)
#define Set_To_Empty(range) (range)->max = (int)(1 << (sizeof(int) * 8 - 1))
#define Range_Init_Interval(range, r_min, r_max)	\
  do							\
    {							\
      (range)->extra_cstr = FALSE;			\
      (range)->min = (r_min);				\
      (range)->max = (r_max);				\
      (range)->vec = NULL;				\
    }							\
  while (0)
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
#define Extra_Cstr(fdv_adr)        (Range(fdv_adr)->extra_cstr)
#define Min(fdv_adr)               (Range(fdv_adr)->min)
#define Max(fdv_adr)               (Range(fdv_adr)->max)
#define Vec(fdv_adr)               (Range(fdv_adr)->vec)
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
#define CHAIN_RECORD_FRAME_SIZE    2
#define CF_Pointer(rec_adr)        (*(WamWord **) &(rec_adr[0]))
#define Next_Chain(rec_adr)        (*(WamWord **) &(rec_adr[1]))
#define CONSTRAINT_FRAME_SIZE      3
#define OFFSET_OF_OPTIM_POINTER    1	/* this offset must corresponds to */
#define AF_Pointer(cf)             (*(WamWord **)  &(cf[0]))
#define Optim_Pointer(cf)          (*(WamWord **)  &(cf[1]))	/* this cell */
#define Cstr_Address(cf)           (*(long (**)()) &(cf[2]))
#define ENV_VAR_VECTOR_MAX         "VECTORMAX"
#define DEFAULT_VECTOR_MAX         127
#define Fd_Variable_Is_Ground(fdv_adr) (Tag_Of(FD_Tag_Value(fdv_adr))==INT)
#define math_min(x, y)             ((x) <= (y) ? (x) : (y))
#define math_max(x, y)             ((x) >= (y) ? (x) : (y))
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
int Fd_Variable_Size0(WamWord *fdv_adr);
int Fd_Copy_Variable0(WamWord *dst_adr, WamWord *fdv_adr);
char *Fd_Variable_To_String0(WamWord *fdv_adr);
#define Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask)         \
  DEREF(fdv_word, word, tag_mask);                              \
  if (tag_mask == TAG_REF_MASK)                                 \
    Pl_Err_Instantiation();                                     \
                                                                \
  if (tag_mask != TAG_INT_MASK && tag_mask != TAG_FDV_MASK)     \
    Pl_Err_Type(type_fd_variable, word)
#define MASK_EMPTY                 0
#define MASK_LEFT                  1
#define MASK_RIGHT                 2
#if 0
#define DEBUG
#endif
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
Bool Load_Left_Right(Bool optim_eq, WamWord le_word, WamWord re_word,
		     int *mask, long *c, WamWord *l_word, WamWord *r_word);
Bool Term_Math_Loading(WamWord l_word, WamWord r_word);
Bool Fd_Math_Unify_X_Y(WamWord x, WamWord y);
#ifdef DEBUG
void Debug_Display(char *fct, int n, ...);
#endif
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
#define DEBUG_2(f, a1, a2)                 Debug_Display(#f, 2, a1, a2)
#define DEBUG_3(f, a1, a2, a3)             Debug_Display(#f, 3, a1, a2, a3)
#define DEBUG_4(f, a1, a2, a3, a4)         Debug_Display(#f, 4, a1, a2, a3, a4)
#define DEBUG_5(f, a1, a2, a3, a4, a5)     Debug_Display(#f, 5, a1, a2, a3, a4, a5)
#define DEBUG_6(f, a1, a2, a3, a4, a5, a6) Debug_Display(#f, 6, a1, a2, a3, a4, a5, a6)
#else
#define DEBUG_2(f, a1, a2)
#define DEBUG_3(f, a1, a2, a3)
#define DEBUG_4(f, a1, a2, a3, a4)
#define DEBUG_5(f, a1, a2, a3, a4, a5)
#define DEBUG_6(f, a1, a2, a3, a4, a5, a6)
#endif
#define PRIM_CSTR_2(f, a1, a2)			\
  do						\
    {						\
      DEBUG_2(f, a1, a2);			\
      if (!f(a1, a2))				\
	return FALSE;				\
    }						\
  while (0)
#define PRIM_CSTR_3(f, a1, a2, a3)		\
  do						\
    {						\
      DEBUG_3(f, a1, a2, a3);			\
      if (!f(a1, a2, a3))			\
	return FALSE;				\
    }						\
  while (0)
#define PRIM_CSTR_4(f, a1, a2, a3, a4)		\
  do						\
    {						\
      DEBUG_4(f, a1, a2, a3, a4);		\
      if (!f(a1, a2, a3, a4))			\
	return FALSE;				\
    }						\
  while (0)
#define PRIM_CSTR_5(f, a1, a2, a3, a4, a5)	\
  do						\
    {						\
      DEBUG_5(f, a1, a2, a3, a4, a5);		\
      if (!f(a1, a2, a3, a4, a5))		\
	return FALSE;				\
    }						\
  while (0)
#define PRIM_CSTR_6(f, a1, a2, a3, a4, a5, a6)	\
  do						\
    {						\
      DEBUG_6(f, a1, a2, a3, a4, a5, a6);	\
      if (!f(a1, a2, a3, a4, a5, a6))		\
	return FALSE;				\
    }						\
  while (0)
#define MATH_CSTR_2(f, a1, a2)			\
  do						\
    {						\
      if (full_ac == FALSE)			\
	PRIM_CSTR_2(f, a1, a2);			\
      else					\
	PRIM_CSTR_2(f##_F, a1, a2);		\
    }						\
  while (0)
#define MATH_CSTR_3(f, a1, a2, a3)		\
  do						\
    {						\
      if (full_ac == FALSE)			\
	PRIM_CSTR_3(f, a1, a2, a3);		\
      else					\
	PRIM_CSTR_3(f##_F, a1, a2, a3);		\
    }						\
  while (0)
#define MATH_CSTR_4(f, a1, a2, a3, a4)		\
  do						\
    {						\
      if (full_ac == FALSE)			\
	PRIM_CSTR_4(f, a1, a2, a3, a4);		\
      else					\
	PRIM_CSTR_4(f##_F, a1, a2, a3, a4);	\
    }						\
  while (0)
#define MATH_CSTR_5(f, a1, a2, a3, a4, a5)	\
  do						\
    {						\
      if (full_ac == FALSE)			\
	PRIM_CSTR_5(f, a1, a2, a3, a4, a5);	\
      else					\
	PRIM_CSTR_5(f##_F, a1, a2, a3, a4, a5);	\
    }						\
  while (0)
#define MATH_CSTR_6(f, a1, a2, a3, a4, a5, a6)		\
  do							\
    {							\
      if (full_ac == FALSE)				\
	PRIM_CSTR_6(f, a1, a2, a3, a4, a5, a6);		\
      else						\
	PRIM_CSTR_6(f##_F, a1, a2, a3, a4, a5, a6);	\
    }							\
  while (0)
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
#ifdef __cplusplus
}
#endif
#endif
