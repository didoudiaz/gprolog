/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : line-edit library                                               */
/* File  : linedit.c                                                       */
/* Descr.: line editor                                                     */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "stty.h"
#include "char_io.h"
#include "linedit.h"

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#include <sys/time.h>
#elif defined(_MSC_VER)
#include <time.h>
#endif




/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

#define LINEDIT_VERSION            "2.1"

#define MAX_HISTORY_LINES          64
#define MAX_SEPARATORS             256

#define NB_TAB_BEFORE_LIST         1

#define DEFAULT_SEPARATORS         " ,;:-'\"!@$#^&()-+*/\\[]|<=>`~{}"

#define NB_MATCH_LINES_BEFORE_ASK  20

#ifdef NO_USE_SELECT
#define BRACKET_TIMMING            300000               /* in microseconds */
#else
#define BRACKET_TIMMING            900000               /* in microseconds */
#endif

#define IGNORE_QUOTED_PART




/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

typedef struct
    {
     int       buff_length;
     int       line_length;
     char     *line;
    }HistCell;



typedef struct comp_node CompNode;

struct comp_node
    {
     char     *word;
     int       word_length;
     CompNode *next;
    };




/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

static char      separators[MAX_SEPARATORS]=DEFAULT_SEPARATORS;
static int       ins_mode=1;

static char     *global_str;
static char     *global_pos;
static char     *global_end;

static char      clipboard[4096]="";


static HistCell  hist_tbl[MAX_HISTORY_LINES];
static int       hist_start=0;
static int       hist_end=0;


static CompNode *comp_start=NULL;
static CompNode *comp_first_match;
static CompNode *comp_last_match;
static int       comp_nb_match;
static int       comp_match_max_lg;

static CompNode *comp_cur_match;



/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

static
void      Backd                 (int fd_out,int n);
static
void      Forwd                 (int fd_out,int n,char *str);
static
void      Displ                 (int fd_out,int n,char *str);
static
void      Erase                 (int fd_out,int n);

static
int       New_Char              (int c,int ins_mode,char *str,int size,
                                 int fd_out,char **p_pos,char **p_end);
static 
char     *Skip                  (char *from,char *limit,int res_sep_cmp,
                                 int direction);
static
int       Is_A_Separator        (char c);


static
void      History_Add_Line      (char *line,int length);
static 
void      History_Update_Line   (char *line,int length,int hist_no);
static
int       History_Get_Line      (char *str,int hist_no);


static
char     *Completion_Do_Match   (char *prefix,int prefix_length,
                                 int *rest_length);
static 
void      Completion_Print_All  (int fd_in,int fd_out);

static
void      Display_String        (int fd_out,char *s);

static
void      Display_Help          (int fd_out);



#define NewLn()  {LE_Put_Char('\n',fd_out);  }


#define Hist_Inc(n)   { if (++(n)==MAX_HISTORY_LINES) (n)=0; }
#define Hist_Dec(n)   { if (--(n)<0) (n)=MAX_HISTORY_LINES-1; }
#define Hist_First(n) { (n)=Hist_Start_Entry(); }
#define Hist_Last(n)  { (n)=Hist_End_Entry(); }

#define Hist_Start_Entry()    (hist_start)
#define Hist_End_Entry()      (hist_end)

#define Hist_Is_Empty()       (hist_start==hist_end)



#if defined(__unix__) || defined(__CYGWIN__)

#define Emit_Signal(sig)      kill(getpid(),sig)

#elif defined(_MSC_VER)

#define Emit_Signal(sig)      raise(sig)

#else

#define Emit_Signal(sig)      printf("\nlinedit: should send a signal(%d) - to do\n",sig)

#endif




/*-------------------------------------------------------------------------*/
/* LE_GETS                                                                 */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Gets(char *str)

{
 int l;
 int big_size=((unsigned) -1)>>1;


 if ((str=LE_FGets(str,big_size,0,1,NULL,0))!=NULL)
    {
     l=strlen(str)-1;                           /* for gets remove last \n */
     if (l>=0 && str[l]=='\n')
         str[l]='\0';
    }

 return str;
}




/*-------------------------------------------------------------------------*/
/* LE_FGETS                                                                */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_FGets(char *str,int size,int fd_in,int fd_out,char *prompt,
               int display_prompt)

{
 char *pos=str;
 char *end=str;
 char *mark=NULL;
 char *p,*q,*start,*stop;
 char  w;
 int   c,n;
 int   last_was_eof=0;
 int   h_no=Hist_End_Entry();
 int   rest_length;
 int   tab_count=0;
 int   sig;
 char *open_bracket ="([{";
 char *close_bracket=")]}";
 int   count_bracket[3];

 size--;                                                    /* -1 for '\0' */

 LE_Init(fd_in,fd_out);
 global_str=str;

#if 1                           /* avoid to redisplay a buffered full line */
 while(LE_Kbd_Is_Not_Empty(fd_in))
    {
     if (end-str>=size)
        {
         *end='\0';
         goto finish;
        }

     c=LE_Get_Char(fd_in);
     if (c=='\n' || c=='\r')
        {
         *end++='\n';
         *end='\0';
         goto finish;
        }
     *end++=c;
    }
 if (end!=str)
    {
     pos=end;
     goto re_display_line;
    }
#endif

 if (prompt && display_prompt)
     Display_String(fd_out,prompt);

 for(;;)
    {
     global_pos=pos;
     global_end=end;

     c=LE_Get_Char(fd_in);
one_char:
     *end=' ';                              /* to allow for separator test */

     if ((sig=LE_Which_Signal(c))!=0)
        {
         LE_Term(fd_in,fd_out);
         c=*end;
         *end='\0';                /* to allow the handler to use/test str */
         Emit_Signal(sig);
         LE_Init(fd_in,fd_out);
         *end=c;

re_display_line:                             /* display prompt + full line */
         if (prompt)
             Display_String(fd_out,prompt);
         p=pos;
         pos=str;
         Displ(fd_out,end-str,pos);
         pos=p;
         Backd(fd_out,end-pos);
         continue;
        }

     if (KEY_IS_EOF(c))            /* to avoid EOF when typing too much ^D */
        {
         if (end==str)
            {
             if (c==KEY_CTRL('D') && last_was_eof)
                 goto error;
              else
                {
                 str=NULL;
                 goto finish;
                }
            }
         last_was_eof=(c==KEY_CTRL('D'));
        }
      else
         last_was_eof=0;

     if (c!='\t')
         tab_count=0;

     switch(c)
        {
         case KEY_CTRL('A'):                        /* go to begin of line */
         case KEY_EXT_HOME:
             Backd(fd_out,pos-str);
             pos=str;
             continue;


         case KEY_CTRL('E'):                          /* go to end of line */
         case KEY_EXT_END:
             Forwd(fd_out,end-pos,pos);
             pos=end;
             continue;


         case KEY_CTRL('B'):                      /* go to 1 char backward */
         case KEY_EXT_LEFT:
             if (pos==str)
                 goto error;
             Backd(fd_out,1);
             pos--;
             continue;


         case KEY_CTRL('F'):                       /* go to 1 char forward */
         case KEY_EXT_RIGHT:
             if (pos==end)
                 goto error;
             Forwd(fd_out,1,pos);
             pos++;
             continue;


         case KEY_BACKSPACE:                        /* erase previous char */
         case KEY_DELETE:
             if (pos==str)
                 goto error;
del_last:
             for(p=pos;p<end;p++)
                 p[-1]=*p;
             Backd(fd_out,1);
             pos--;
             end--;
             Displ(fd_out,end-pos,pos);
             Erase(fd_out,1);
             Backd(fd_out,end-pos);
             continue;


         case KEY_CTRL('D'):                         /* erase current char */
         case KEY_EXT_DELETE:
             if (pos==end)
                 goto error;
                                    /* simply equivalent to ^F + BACKSPACE */
             Forwd(fd_out,1,pos);
             pos++;
             goto del_last;


         case KEY_CTRL('U'):                        /* erase begin of line */
         case KEY_CTRL_EXT_HOME:
             q=clipboard;
             p=str;
             while(p<pos)                 /* add deleted part to clipboard */
                 *q++=*p++;
             *q='\0';
             n=pos-str;
             for(p=pos;p<end;p++)
                 p[-n]=*p;

             pos=str;
             end-=n;
             Backd(fd_out,n);
             Displ(fd_out,end-pos,pos);
             Erase(fd_out,n);
             Backd(fd_out,end-pos);
             continue;


         case KEY_CTRL('K'):                          /* erase end of line */
         case KEY_CTRL_EXT_END:
             q=clipboard;
             p=pos;
             while(p<end)                 /* add deleted part to clipboard */
                 *q++=*p++;
             *q='\0';
             Erase(fd_out,end-pos);
             end=pos;
             continue;


         case KEY_CTRL('Y'):                       /* paste from clipboard */
             for(p=clipboard;*p;p++)
                 if (!New_Char(*p,ins_mode,str,size,fd_out,&pos,&end))
                     goto error;
             continue;


         case KEY_CTRL(' '):                       /* mark begin selection */
             mark=pos;
             continue;


         case KEY_ESC('W'):               /* copy (from mark) to clipboard */
         case KEY_CTRL('W'):               /* cut (from mark) to clipboard */
             if (mark==NULL)
                 goto error;

             if (mark<pos)
                {
                 start=mark;
                 stop=pos;
                }
              else
                {
                 start=pos;
                 stop=mark;
                }
             q=clipboard;
             p=start;
             while(p<stop)
                 *q++=*p++;
             *q='\0';
             if (c==KEY_ESC('W'))
                 continue;

             n=stop-start;
             for(p=stop;p<end;p++)
                 p[-n]=*p;

             if (mark<pos)
                 Backd(fd_out,n);
             pos=start;
             end-=n;
             Displ(fd_out,end-pos,pos);
             Erase(fd_out,n);
             Backd(fd_out,end-pos);
             continue;


         case KEY_ESC('B'):                         /* go to previous word */
         case KEY_CTRL_EXT_LEFT:
             p=(pos==str) ? pos : pos-1;       /* to avoid start of a word */
             p=Skip(p,str,1,-1);                        /* skip separators */
             p=Skip(p,str,0,-1);                    /* skip non separators */
             p=Skip(p,end,1,+1);                        /* skip separators */
             Backd(fd_out,pos-p);
             pos=p;
             continue;


         case KEY_ESC('F'):                             /* go to next word */
         case KEY_CTRL_EXT_RIGHT:
             p=pos;
             p=Skip(p,end,0,+1);                    /* skip non separators */
             p=Skip(p,end,1,+1);                        /* skip separators */
             Forwd(fd_out,p-pos,pos);
             pos=p;
             continue;


         case KEY_ESC('C'):                             /* capitalize word */
             p=pos;
             p=Skip(p,end,1,+1);                        /* skip separators */
             if (islower(*p))
                 *p=*p-'a'+'A';
             p=Skip(p,end,0,+1);                    /* skip non separators */
             Displ(fd_out,p-pos,pos);
             pos=p;
             continue;


         case KEY_ESC('L'):                       /* convert to lower case */
             p=pos;
             p=Skip(p,end,1,+1);                        /* skip separators */
             for(;p<end && !Is_A_Separator(*p);p++)
                 *p=tolower(*p);
             Displ(fd_out,p-pos,pos);
             pos=p;
             continue;


         case KEY_ESC('U'):                       /* convert to upper case */
             p=pos;
             p=Skip(p,end,1,+1);                        /* skip separators */
             for(;p<end && !Is_A_Separator(*p);p++)
                 *p=toupper(*p);
             Displ(fd_out,p-pos,pos);
             pos=p;
             continue;


         case '\t':                                     /* TAB: completion */
             if (tab_count!=0)                            /* already a TAB */
                {
                 if (++tab_count>NB_TAB_BEFORE_LIST)
                    {
                     NewLn();
                     Completion_Print_All(fd_in,fd_out);
                     goto re_display_line;
                    }
                 goto error;
                }
             p=(pos==str) ? pos : pos-1;       /* to avoid start of a word */
             p=Skip(p,str,0,-1);                    /* skip non separators */
             p=Skip(p,end,1,+1);                        /* skip separators */
             w=*pos;                               /* prefix from p to pos */
             *pos='\0';
             p=Completion_Do_Match(p,pos-p,&rest_length);
             *pos=w;
             if (p==NULL)
                 goto error;

             while(rest_length--)
                 if (!New_Char(*p++,ins_mode,str,size,fd_out,&pos,&end))
                     goto error;

             if (comp_first_match!=comp_last_match)
                {
                 tab_count=1;
                 goto error;                               /* for the beep */
                }
             tab_count=0;
             continue;


         case KEY_CTRL('V'):                /* switch insert mode (on/off) */
         case KEY_EXT_INSERT:
             ins_mode=1-ins_mode;
             LE_Ins_Mode(ins_mode);
             continue;


         case KEY_CTRL('T'):                 /* swap last and current char */
             if (pos==str || pos==end)
                 goto error;
             Backd(fd_out,1);
             pos--;
             w=*pos;
             *pos=pos[1];
             pos[1]=w;
             Displ(fd_out,2,pos);
             pos+=2;
             continue;

         case '\n':
         case '\r':
             *end='\0';
             History_Add_Line(str,end-str);
             if (end-str<size)                        /* '\n' can be added */
                 *end++='\n';
             *end='\0';
             goto finish;


         case KEY_CTRL('P'):              /* history: recall previous line */
         case KEY_EXT_UP:
             if (Hist_Is_Empty() || h_no==Hist_Start_Entry())
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
             Hist_Dec(h_no);
write_hist_line:
             Backd(fd_out,pos-str);
             pos=str;
             p=end;
             end=str+History_Get_Line(str,h_no);
             Displ(fd_out,end-str,pos);
             if (end<p)
                 Erase(fd_out,p-end);
             pos=end;
             continue;


         case KEY_CTRL('N'):                  /* history: recall next line */
         case KEY_EXT_DOWN:
             if (Hist_Is_Empty() || h_no==Hist_End_Entry())
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
             Hist_Inc(h_no);
             goto write_hist_line;


         case KEY_ESC('P'):      /* history: recall previous matching line */
             if (Hist_Is_Empty() || pos==str)
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
try_previous:
             if (h_no==Hist_Start_Entry())
                 goto error;
             Hist_Dec(h_no);
             if (hist_tbl[h_no].line==NULL ||
                 strncmp(str,hist_tbl[h_no].line,pos-str)!=0)
                 goto try_previous;
write_hist_match_line:
             p=end;
             end=str+History_Get_Line(str,h_no);
             Displ(fd_out,end-pos,pos);
             if (end<p)
                 Erase(fd_out,p-end);
             Backd(fd_out,end-pos);
             continue;


         case KEY_ESC('N'):          /* history: recall next matching line */
             if (Hist_Is_Empty() || pos==str)
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
try_next:
             if (Hist_Is_Empty() || h_no==Hist_End_Entry())
                 goto error;
             Hist_Inc(h_no);
             if (hist_tbl[h_no].line==NULL ||
                 strncmp(str,hist_tbl[h_no].line,pos-str)!=0)
                 goto try_next;
             goto write_hist_match_line;


         case KEY_ESC('<'):                  /* history: recall first line */
         case KEY_EXT_PAGE_UP:
             if (Hist_Is_Empty() || h_no==Hist_Start_Entry())
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
             Hist_First(h_no);
             goto write_hist_line;


         case KEY_ESC('>'):                   /* history: recall last line */
         case KEY_EXT_PAGE_DOWN:
             if (Hist_Is_Empty() || h_no==Hist_End_Entry())
                 goto error;
             *end='\0';
             History_Update_Line(str,end-str,h_no);
             Hist_Last(h_no);
             goto write_hist_line;


         case KEY_ESC('?'):                                /* display help */
display_help:
             NewLn();
             Display_Help(fd_out);
             goto re_display_line;


         default:
             if (!isprint(c) || c>255)
                {
                 n=c;
                 LE_Emit_Beep(fd_out);
                 c=LE_Get_Char(fd_in);
                 if (c!=n)
                     goto one_char;
                 goto display_help;
                }

             if (!New_Char(c,ins_mode,str,size,fd_out,&pos,&end))
                 goto error;
                                             /* brackets ([{ }]): matching */
             if (LE_Kbd_Is_Not_Empty(fd_in) || 
                 (q=strchr(close_bracket,c))==NULL)
                 continue;

             n=q-close_bracket;

             count_bracket[0]=count_bracket[1]=count_bracket[2]=0;
             count_bracket[n]--;

             p=pos-1;
             for(;count_bracket[n]!=0;)
                {
                 if (--p<str)
                     goto bracket_exit;

                 c=*p;
                 if ((q=strchr(close_bracket,c))!=NULL)
                    {
                     count_bracket[q-close_bracket]--;
                     continue;
                    }

                 if ((q=strchr(open_bracket,c))!=NULL)
                     if (++count_bracket[q-open_bracket]>0)
                         goto bracket_exit;

#ifdef IGNORE_QUOTED_PART
                 if (p>str && (c=='\'' || c=='"') && p[-1]!='\\')
                    {                                /* ignore quoted part */
                     while(--p>str && (*p!=c || p[-1]=='\\'))
                         ;
                    }
#endif
                }

             if (LE_Kbd_Is_Not_Empty(fd_in))
                 continue;

             n=pos-p;
             q=pos;
             Backd(fd_out,n);
#ifdef _MSC_VER

            {
             long t0=clock(),t1;

             do
                t1=clock();
             while(!LE_Kbd_Is_Not_Empty(fd_in) && 
                   (t1-t0)*1000000/CLOCKS_PER_SEC < BRACKET_TIMMING);
            }

#elif !defined(NO_USE_SELECT)
            {
             fd_set set;
             struct timeval t;

             t.tv_sec=0;
             t.tv_usec=BRACKET_TIMMING;

             FD_ZERO(&set);
             FD_SET(fd_in,&set);
             select(fd_in+1,&set,NULL,NULL,&t);
            }
#else
             usleep(BRACKET_TIMMING);
#endif
             pos=p;
             Displ(fd_out,n,pos);
             pos=q;
bracket_exit:
             continue;
        }
error:
     LE_Emit_Beep(fd_out);
    }

finish:
 NewLn();

 LE_Term(fd_in,fd_out);

 return str;
}




/*-------------------------------------------------------------------------*/
/* NEW_CHAR                                                                */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static int New_Char(int c,int ins_mode,char *str,int size,int fd_out,
                    char **p_pos,char **p_end)

{
 char *pos=*p_pos;
 char *end=*p_end;
 char *p;


 if ((ins_mode || pos==end) && end-str>=size)
     return 0;

 if (!ins_mode)
    {
     *pos=(char) c;
     if (++pos>end)
         end=pos;
     LE_Put_Char(c,fd_out);
    }
  else
    {
     for(p=end;p>pos;p--)
         *p=p[-1];

     *pos=(char) c;
     end++;
     Displ(fd_out,end-pos,pos);
     pos++;
     Backd(fd_out,end-pos);
    }

 *p_pos=pos;
 *p_end=end;

 return 1;
}




/*-------------------------------------------------------------------------*/
/* BACKD                                                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Backd(int fd_out,int n)

{
#ifdef W32_GUI_CONSOLE 
 if (w32gc_backd)
    {
     (*w32gc_backd)(n);
     return;
    }
#endif
 while(n--) 
     LE_Put_Char('\b',fd_out);
}




/*-------------------------------------------------------------------------*/
/* FORWD                                                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Forwd(int fd_out,int n,char *str)

{
#ifdef W32_GUI_CONSOLE 
 if (w32gc_forwd)
    {
     (*w32gc_forwd)(n);
     return;
    }
#endif
 while(n--)
     LE_Put_Char(*str++,fd_out);
}




/*-------------------------------------------------------------------------*/
/* DISPL                                                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Displ(int fd_out,int n,char *str)

{
#ifdef W32_GUI_CONSOLE 
 if (w32gc_displ)
    {
     (*w32gc_displ)(n,str);
     return;
    }
#endif
 Forwd(fd_out,n,str);
}





/*-------------------------------------------------------------------------*/
/* ERASE                                                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Erase(int fd_out,int n)

{
 int n0=n;

#ifdef W32_GUI_CONSOLE 
 if (w32gc_erase)
    {
     (*w32gc_erase)(n);
     return;
    }
#endif
 while(n--)
     LE_Put_Char(' ',fd_out);

 Backd(fd_out,n0);
}




/*-------------------------------------------------------------------------*/
/* LE_GET_CURRENT_WORD                                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void LE_Get_Current_Word(char *word)

{
 char *str=global_str;
 char *pos=global_pos;
 char *end=global_end;
 char *p,*q;

 p=Skip(pos,str,0,-1);                              /* skip non separators */
 if (Is_A_Separator(*p))
     p++;

 q=Skip(pos,end,0,+1);                              /* skip non separators */

 while(p<q)
     *word++ = *p++;

 *word='\0';
}




/*-------------------------------------------------------------------------*/
/* LE_GET_SEPARATORS                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Get_Separators(void)

{
 return separators;
}




/*-------------------------------------------------------------------------*/
/* LE_SET_SEPARATORS                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Set_Separators(char *sep_str)

{
 return strcpy(separators,sep_str);
}




/*-------------------------------------------------------------------------*/
/* SKIP                                                                    */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static char *Skip(char *from,char *limit,int res_sep_cmp,int direction)

{
 while(from!=limit)
    {
     if (Is_A_Separator(*from)!=res_sep_cmp)
         break;           /* exit since *from does not satisfy res_sep_cmp */

     from=from+direction;
    }

 return from; 
}




/*-------------------------------------------------------------------------*/
/* IS_A_SEPARATOR                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static int Is_A_Separator(char c)

{
 char *p;
                                         /* like strchr(separators,c) but  */
 for(p=separators;*p;p++)                /* does not take into account '\0'*/
     if (*p==c)
         return 1;

 return 0;
}




/*-------------------------------------------------------------------------*/
/* HISTORY_ADD_LINE                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void History_Add_Line(char *line,int length)

{
 char *p=line;

 while(*p==' ')
     p++;
 if (*p=='\0')                                 /* do not add an empty line */
     return;

 History_Update_Line(line,length,hist_end);

 Hist_Inc(hist_end);
 if (hist_end==hist_start)
     Hist_Inc(hist_start);
}




/*-------------------------------------------------------------------------*/
/* HISTORY_UPDATE_LINE                                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void History_Update_Line(char *line,int length,int hist_no)

{
 HistCell *h;

 h=hist_tbl+hist_no;

 if (h->line!=NULL && h->buff_length<length)
    {
     free(h->line);
     h->line=NULL;                              /* to ensure future malloc */
    }

 if (h->line==NULL)                                   /* not yet allocated */
    {
     if ((h->line=(char *) malloc(length+1))==NULL)
         exit(1);
     h->buff_length=length;
    }

 strcpy(h->line,line);
 h->line_length=length;
}




/*-------------------------------------------------------------------------*/
/* HISTORY_GET_LINE                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static int History_Get_Line(char *str,int hist_no)

{
 HistCell *h=hist_tbl+hist_no;

 strcpy(str,h->line);

 return h->line_length;
}




/*-------------------------------------------------------------------------*/
/* LE_COMPL_ADD_WORD                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Compl_Add_Word(char *word,int word_length)

{
 CompNode **p;
 CompNode  *q;
 int        cmp;

 for(p=&comp_start;*p;p=&(*p)->next)
    {
     cmp=strcmp((*p)->word,word);
     if (cmp==0)
         return word;

     if (cmp>0)
         break;
    }

 if ((q=(CompNode *) malloc(sizeof(CompNode)))==NULL)
     exit(1);

 q->word=word;
 q->word_length=word_length;
 q->next=*p;
 *p=q;

 return word;
}




/*-------------------------------------------------------------------------*/
/* LE_COMPL_DEL_WORD                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Compl_Del_Word(char *word)

{
 CompNode **p;
 CompNode  *q;
 int        cmp;

 for(p=&comp_start;*p;p=&(*p)->next)
    {
     cmp=strcmp((*p)->word,word);
     if (cmp==0)
         break;

     if (cmp>0)
         return NULL;
    }

 q=*p;
 *p=q->next;
 free(q);

 return word;
}




/*-------------------------------------------------------------------------*/
/* LE_COMPL_INIT_MATCH                                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Compl_Init_Match(char *prefix,int *nb_match,int *max_lg)

{
 int   prefix_length,rest_length;
 char *str;

 prefix_length=strlen(prefix);

 if (Completion_Do_Match(prefix,prefix_length,&rest_length)==NULL)
     return NULL;

 if ((str=(char *) malloc(prefix_length+rest_length+1))==NULL)
     exit(1);

 *nb_match=comp_nb_match;
 *max_lg=comp_match_max_lg;
 comp_cur_match=comp_first_match;

 strncpy(str,comp_first_match->word,prefix_length+rest_length);
 return str;
}




/*-------------------------------------------------------------------------*/
/* LE_COMPL_FIND_MATCH                                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
char *LE_Compl_Find_Match(int *is_last)

{
 char *str;

 if (comp_cur_match==NULL)
     return NULL;

 str=comp_cur_match->word;
 if (comp_cur_match!=comp_last_match)
    {
     comp_cur_match=comp_cur_match->next;
     *is_last=0;
    }
  else
    {
     comp_cur_match=NULL;
     *is_last=1;
    }

 return str;
}




/*-------------------------------------------------------------------------*/
/* COMPLETION_DO_MATCH                                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static char *Completion_Do_Match(char *prefix,int prefix_length,
                                 int *rest_length)

{
 CompNode *p;
 int       cmp;
 int       l;
 char      w;


 comp_first_match=NULL;
 comp_nb_match=0;
 comp_match_max_lg=0;
 
 for(p=comp_start;p;p=p->next)
    {
     cmp=strncmp(p->word,prefix,prefix_length);
     if (cmp==0)
        {
         if (comp_first_match==NULL)
             comp_first_match=p;

         comp_last_match=p;
         comp_nb_match++;
         if (p->word_length>comp_match_max_lg)
             comp_match_max_lg=p->word_length;
        }
      else
         if (cmp>0)
             break;
    }

 if (comp_first_match==NULL)
     return NULL;

 if (comp_first_match==comp_last_match)
     *rest_length=comp_first_match->word_length-prefix_length;
  else
    {                                   /* determine longest common suffix */
     l=prefix_length;
     for(;;)
        {
         w=comp_first_match->word[l];
         p=comp_first_match->next;
         for(;;)
            {
             if (p->word[l]!=w)                    /* also deals with '\0' */
                 goto diff_found;

             if (p==comp_last_match)
                 break;
             p=p->next;
            }

         l++;
        }
diff_found:
     *rest_length=l-prefix_length;
    }

 return comp_first_match->word+prefix_length;
}




/*-------------------------------------------------------------------------*/
/* COMPLETION_PRINT_ALL                                                    */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Completion_Print_All(int fd_in,int fd_out)

{
 CompNode *p,*p1;
 int       row,col;
 int       nb_in_a_line,nb_lines;
 int       nb_in_last_line,nb_miss_in_last_line;
 int       spaces,skip;
 int       k;
 char      buff[512];
 int       l,c;


 LE_Screen_Size(fd_out,&row,&col);

 nb_in_a_line=col/(comp_match_max_lg+2);   /* at least 2 chars to separate */

 if (nb_in_a_line<=1)
     nb_in_a_line=1;

 nb_lines            =(comp_nb_match+nb_in_a_line-1) / nb_in_a_line; 
 nb_in_last_line     =((comp_nb_match-1) % nb_in_a_line) + 1;
 nb_miss_in_last_line=nb_in_a_line-nb_in_last_line;

 spaces=(nb_in_a_line==1) 
             ? 0
             : (col-nb_in_a_line*comp_match_max_lg) / nb_in_a_line;


 if (nb_lines>NB_MATCH_LINES_BEFORE_ASK)           /* too many matchings ? */
    {
     sprintf(buff,"Show all %d possibilities (y/n) ? ",comp_nb_match);
     Display_String(fd_out,buff);
     c=LE_Get_Char(fd_in);
     NewLn();
     if (c!='y')
         return;
    }

 p=comp_first_match;
 l=0;
 for(;;)
    {
     p1=p;
     c=0;
     for(;;)
        {
         Display_String(fd_out,p1->word);

         if (++c==((l<nb_lines-1) ? nb_in_a_line : nb_in_last_line))
             break;

         sprintf(buff,"%*s",comp_match_max_lg-p1->word_length+spaces,"");
         Display_String(fd_out,buff);

         skip=nb_lines;
         if (c>nb_in_a_line-nb_miss_in_last_line)
             skip--;
         for(k=0;k<skip;k++)
             p1=p1->next;
        }

     NewLn();
     if (++l==nb_lines)
         break;

     p=p->next;
    }
}




/*-------------------------------------------------------------------------*/
/* DISPLAY_STRING                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Display_String(int fd_out,char *s)

{
#ifdef W32_GUI_CONSOLE
 if (w32gc_display_string)
    {
     (*w32gc_display_string)(s);
     return;
    }
#endif

 while(*s)
     LE_Put_Char(*s++,fd_out);
}




/*-------------------------------------------------------------------------*/
/* DISPLAY_HELP                                                            */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static void Display_Help(int fd_out)

#define L(msg)     { Display_String(fd_out,msg); NewLn(); }

{
 char buff[80];

 L("")
 sprintf(buff,
   "   linedit %-25s Copyright (C) 1999,2000 Daniel Diaz",
   LINEDIT_VERSION);
 L(buff)
 L("")
 L("                              Moving")
 L("   Ctl-B   previous char             Ctl-F   next char")
 L("   Esc-B   previous word             Esc-F   next word")
 L("   Ctl-A   begin of line             Ctl-E   end of line")
 L("")
 L("                             Deleting")
 L("   Ctl-U   delete begin of line      Ctl-K   delete end of line")
 L("   Ctl-H   delete previous char      Ctl-D   delete current char")
 L("")
 L("                             Changing")
 L("   Esc-L   downcase word             Esc-U   upcase word")
 L("   Esc-C   capitalize word           Ctl-T   reverse last two chars")
 L("")
 L("                             History")
 L("   Esc-<   first line                Esc->   last line")
 L("   Ctl-P   previous line             Ctl-N   next line")
 L("   Esc-P   previous matching line    Esc-N   next matching line")
 L("")
 L("                             Selection")
 L("   Ctl-spc mark selection            Ctl-W   cut  selection")
 L("   Esc-W   copy selection            Ctl-Y   past selection")
 L("")
 L("                           Miscellaneous")
 L("   Ctl-V   insert mode switch        Ctl-I   completion (twice=all)")
 L("   Esc-?   display this help")
 L("")
}

#undef L




/*-------------------------------------------------------------------------*/
/* LE_FGETC_NO_ECHO                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
int LE_FGetc_No_Echo(int fd_in,int fd_out)

{
 int c;
 int sig;


start:
 LE_Init(fd_in,fd_out);

 c=LE_Get_Char(fd_in);
 if ((sig=LE_Which_Signal(c))!=0)
    {
     LE_Term(fd_in,fd_out);
     Emit_Signal(sig);
     goto start;
    }

 if (KEY_IS_EOF(c))
     c=EOF;

 LE_Term(fd_in,fd_out);

 return c;
}




/*-------------------------------------------------------------------------*/
/* LE_PRINTF                                                               */
/*                                                                         */
/*-------------------------------------------------------------------------*/
int LE_Printf(char *format, ...)

{
 va_list arg_ptr;
 int     ret;

 va_start(arg_ptr,format);
 if (w32gc_present)
    {
     char buff[1024];
     ret=vsprintf(buff,format,arg_ptr);
     (*w32gc_display_string)(buff);
    }
  else
     ret=vprintf(format,arg_ptr);
 va_end(arg_ptr);

 return ret;
}

