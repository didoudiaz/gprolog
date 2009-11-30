/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_archi.def (gives rise to wam_archi.h)                       *
 * Descr.: Wam architecture definition - description file                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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

/*---------------------------------*
 * Register Descriptions           *
 *---------------------------------*/


typedef long WamWord;		/* a wamword is a long (32/64 bits) */

typedef void (*CodePtr) ();	/* a code pointer is a ptr to fct */

typedef CodePtr WamCont;	/* a continuation is a code pointer */

#ifndef ONLY_TAG_PART

#define X(x)                       (pl_reg_bank[x])
#define A(a)                       (pl_reg_bank[a])

typedef WamWord *WamWordP;




   /*--- Begin Register Generation ---*/

register WamWord 		*pl_reg_bank asm ("r12");

register WamWordP		TR  asm ("r13");
register WamWordP		B   asm ("r14");
register WamWordP		H   asm ("r15");


#define HB1			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+0])
#define CP			(((WamCont  *) pl_reg_bank)[NB_OF_X_REGS+1])
#define E			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+2])
#define CS			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+3])
#define S			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+4])
#define STAMP			(((WamWord  *) pl_reg_bank)[NB_OF_X_REGS+5])
#define BCI			(((WamWord  *) pl_reg_bank)[NB_OF_X_REGS+6])
#define LSSA			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+7])


#define NB_OF_REGS          	11
#define NB_OF_ALLOC_REGS    	3
#define NB_OF_NOT_ALLOC_REGS	8
#define REG_BANK_SIZE       	(NB_OF_X_REGS+NB_OF_NOT_ALLOC_REGS)




#define NB_OF_USED_MACHINE_REGS 4

#ifdef ENGINE_FILE

WamWord *save_reg_bank;

WamWord pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS + 1];

char *pl_reg_tbl[] = { "TR", "B", "H", "HB1", "CP", "E", "CS", "S", "STAMP", "BCI", "LSSA"};

#else

extern WamWord *save_reg_bank;

extern WamWord pl_buff_signal_reg[];

extern char *pl_reg_tbl[];

#endif

#define Init_Reg_Bank(x)  save_reg_bank = pl_reg_bank = x


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
    buff_save[0] = (WamWord) pl_reg_bank; \
    buff_save[1] = (WamWord) TR; \
    buff_save[2] = (WamWord) B; \
    buff_save[3] = (WamWord) H; \
  } while(0)


#define Restore_Machine_Regs(buff_save) \
  do { \
    pl_reg_bank = (WamWordP) buff_save[0]; \
    TR = (WamWordP) buff_save[1]; \
    B = (WamWordP) buff_save[2]; \
    H = (WamWordP) buff_save[3]; \
  } while(0)




#define Start_Protect_Regs_For_Signal \
  do { \
    Save_Machine_Regs(pl_buff_signal_reg); \
    pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 1; \
  } while(0)


#define Stop_Protect_Regs_For_Signal \
  pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 0; \


#define Restore_Protect_Regs_For_Signal \
  do { \
    if (pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS]) { \
      Restore_Machine_Regs(pl_buff_signal_reg); \
      Stop_Protect_Regs_For_Signal; \
    } \
    pl_reg_bank = save_reg_bank; \
  } while(0)


   /*--- End Register Generation ---*/


#endif


/*---------------------------------*
 * Tag Descriptions                *
 *---------------------------------*/




   /*--- Begin Tag Generation ---*/

#define TAG_SIZE     		3
#define TAG_SIZE_LOW 		3
#define TAG_SIZE_HIGH		0
#define VALUE_SIZE   		61
#define TAG_MASK     		0x7UL
#define VALUE_MASK   		0xfffffffffffffff8UL
#define Tag_Mask_Of(w)		((unsigned long) (w) & (TAG_MASK))
#define Tag_From_Tag_Mask(w) 	(((unsigned long) (w) >> 61) | ((w) & 7))
#define Tag_Of(w)     		Tag_Mask_Of(w)
#define TAG_REF_MASK		0UL
#define TAG_LST_MASK		0x1UL
#define TAG_STC_MASK		0x2UL
#define TAG_ATM_MASK		0x3UL
#define TAG_FLT_MASK		0x4UL
#define TAG_FDV_MASK		0x5UL
#define TAG_INT_MASK		0x7UL

#define NB_OF_TAGS       	7
#define REF        		0 
#define LST        		1 
#define STC        		2 
#define ATM        		3 
#define FLT        		4 
#define FDV        		5 
#define INT        		7 

	/* General Tag/UnTag macros */

#define Tag_Long_Int(tm, v)  	((((unsigned long) ((v) << 3)) >> 0) | (tm))
#define Tag_Short_Uns(tm, v)	(((unsigned long) (v) << 3) + (tm))
#define Tag_Address(tm, v)  	((unsigned long) (v) + (tm))

#define UnTag_Long_Int(w)    	((long) ((w) << 0) >> 3)
#define UnTag_Short_Uns(w)	UnTag_Long_Int(w)
#define UnTag_Address(w)  	((WamWord *) ((w) & VALUE_MASK))


	/* Specialized Tag/UnTag macros */


#define Tag_REF(v)  		Tag_Address(TAG_REF_MASK, v)
#define Tag_LST(v)  		Tag_Address(TAG_LST_MASK, v)
#define Tag_STC(v)  		Tag_Address(TAG_STC_MASK, v)
#define Tag_ATM(v)  		Tag_Short_Uns(TAG_ATM_MASK, v)
#define Tag_FLT(v)  		Tag_Address(TAG_FLT_MASK, v)
#define Tag_FDV(v)  		Tag_Address(TAG_FDV_MASK, v)
#define Tag_INT(v)  		(((unsigned long) (v) << 3) | TAG_MASK)

#define UnTag_REF(w)  		((WamWord *) (w))
#define UnTag_LST(w)  		UnTag_Address(w)
#define UnTag_STC(w)  		UnTag_Address(w)
#define UnTag_ATM(w)  		((unsigned long) (w) >> 3)
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

InfTag pl_tag_tbl[] =
{
  { "REF", ADDRESS, 0, 0UL },
  { "LST", ADDRESS, 1, 0x1UL },
  { "STC", ADDRESS, 2, 0x2UL },
  { "ATM", SHORT_UNS, 3, 0x3UL },
  { "FLT", ADDRESS, 4, 0x4UL },
  { "FDV", ADDRESS, 5, 0x5UL },
  { "INT", LONG_INT, 7, 0x7UL }
};

#else

extern InfTag pl_tag_tbl[];

#endif


   /*--- End Tag Generation ---*/





/*---------------------------------*
 * Stack Descriptions              *
 *---------------------------------*/

#ifndef ONLY_TAG_PART

#define KBytes_To_Wam_Words(kb)    ((1024 * kb + sizeof(WamWord) - 1) / sizeof(WamWord))

#define Wam_Words_To_KBytes(ww)    (ww * sizeof(WamWord) / 1024)

#define Local_Top                  ((B >= E) ? B : E)



   /*--- Begin Stack Generation ---*/

#define NB_OF_STACKS 		4

#define Trail_Stack       	(pl_stk_tbl[0].stack)
#define Trail_Size        	(pl_stk_tbl[0].size)
#define Trail_Offset(adr) 	((WamWord *)(adr) - Trail_Stack)
#define Trail_Used_Size   	Trail_Offset(TR)

#define Cstr_Stack       	(pl_stk_tbl[1].stack)
#define Cstr_Size        	(pl_stk_tbl[1].size)
#define Cstr_Offset(adr) 	((WamWord *)(adr) - Cstr_Stack)
#define Cstr_Used_Size   	Cstr_Offset(CS)

#define Global_Stack       	(pl_stk_tbl[2].stack)
#define Global_Size        	(pl_stk_tbl[2].size)
#define Global_Offset(adr) 	((WamWord *)(adr) - Global_Stack)
#define Global_Used_Size   	Global_Offset(H)

#define Local_Stack       	(pl_stk_tbl[3].stack)
#define Local_Size        	(pl_stk_tbl[3].size)
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

    /* these variables can be overwritten by top_comp.c (see stack size file) */
long pl_def_trail_size;
long pl_def_cstr_size;
long pl_def_global_size;
long pl_def_local_size;
long pl_fixed_sizes;

InfStack pl_stk_tbl[] =
{
 { "trail", "TRAILSZ", &pl_def_trail_size, 1048576, 0, NULL },
 { "cstr", "CSTRSZ", &pl_def_cstr_size, 1048576, 0, NULL },
 { "global", "GLOBALSZ", &pl_def_global_size, 2097152, 0, NULL },
 { "local", "LOCALSZ", &pl_def_local_size, 1048576, 0, NULL }
};

#else

extern long pl_def_trail_size;
extern long pl_def_cstr_size;
extern long pl_def_global_size;
extern long pl_def_local_size;
extern long pl_fixed_sizes;


extern InfStack pl_stk_tbl[];

#endif


   /*--- End Stack Generation ---*/


#endif
