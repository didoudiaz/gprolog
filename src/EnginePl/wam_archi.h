/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog engine                                                   */
/* File  : wam_archi.def (gives rise to wam_archi.h)                       */
/* Descr.: Wam architecture definition - description file                  */
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

/*---------------------------------*/
/* Register Descriptions           */
/*---------------------------------*/



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





/*---------------------------------*/
/* Tag Descriptions                */
/*---------------------------------*/




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





/*---------------------------------*/
/* Stack Descriptions              */
/*---------------------------------*/


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

