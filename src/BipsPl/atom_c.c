/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : atom_c.c                                                        */
/* Descr.: atom manipulation management - C part                           */
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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/

static 
Bool      Compute_Next_BLA      (int mask,AtomInf *patom,AtomInf *psub_atom,
                                 int b,int l,int a,int *b1,int *l1,int *a1);


static
int       Create_Malloc_Atom    (char *str);

static 
Bool      String_To_Number      (char *str,WamWord number_word);




#define ATOM_CONCAT_ALT            X2461746F6D5F636F6E6361745F616C74
#define SUB_ATOM_ALT               X247375625F61746F6D5F616C74
#define CURRENT_ATOM_ALT           X2463757272656E745F61746F6D5F616C74


Prolog_Prototype(ATOM_CONCAT_ALT,0)
Prolog_Prototype(SUB_ATOM_ALT,0)
Prolog_Prototype(CURRENT_ATOM_ALT,0)




#define MALLOC_STR(n)                                                       \
 if (n<0)                                                                   \
     return FALSE;                                                          \
 str=(char *) Malloc(n+1);




/*-------------------------------------------------------------------------*/
/* ATOM_LENGTH_2                                                           */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Length_2(WamWord atom_word,WamWord length_word)

{
 int atom;

 atom=Rd_Atom_Check(atom_word);
 return Un_Positive_Check(atom_tbl[atom].prop.length,length_word);
}




/*-------------------------------------------------------------------------*/
/* ATOM_CONCAT_3                                                           */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Concat_3(WamWord atom1_word,WamWord atom2_word,WamWord atom3_word)

{
 WamWord  word,tag,*adr;
 int      tag1,tag2,tag3;
 AtomInf *patom1,*patom2,*patom3;
 char    *str;
 int      l;


 Deref(atom1_word,word,tag,adr)
 if (tag!=REF && tag!=ATM)
     Pl_Err_Type(type_atom,atom1_word);
 tag1=tag;
 atom1_word=word;


 Deref(atom2_word,word,tag,adr)
 if (tag!=REF && tag!=ATM)
     Pl_Err_Type(type_atom,atom2_word);
 tag2=tag;
 atom2_word=word;


 Deref(atom3_word,word,tag,adr)
 if (tag!=REF && tag!=ATM)
     Pl_Err_Type(type_atom,atom3_word);
 tag3=tag;
 atom3_word=word;


 if (tag3==REF && (tag1==REF || tag2==REF))
     Pl_Err_Instantiation();


 if (tag1==ATM)
    {
     patom1=atom_tbl+UnTag_ATM(atom1_word);

     if (tag2==ATM)
        {
         patom2=atom_tbl+UnTag_ATM(atom2_word);
         l=patom1->prop.length+patom2->prop.length;
         MALLOC_STR(l)
         strcpy(str,patom1->name);
         strcpy(str+patom1->prop.length,patom2->name);
         return Get_Atom(Create_Malloc_Atom(str),atom3_word);
        }

     patom3=atom_tbl+UnTag_ATM(atom3_word);
     l=patom3->prop.length-patom1->prop.length;
     MALLOC_STR(l)
     strcpy(str,patom3->name+patom1->prop.length);

     return strncmp(patom1->name,patom3->name,patom1->prop.length)==0 &&
            Get_Atom(Create_Malloc_Atom(str),atom2_word);
    }

 if (tag2==ATM)                                          /* here tag1==REF */
    {
     patom2=atom_tbl+UnTag_ATM(atom2_word);
     patom3=atom_tbl+UnTag_ATM(atom3_word);
     l=patom3->prop.length-patom2->prop.length;
     MALLOC_STR(l)
     strncpy(str,patom3->name,l);
     str[l]='\0';

     return strncmp(patom2->name,patom3->name+l,patom2->prop.length)==0 &&
            Get_Atom(Create_Malloc_Atom(str),atom1_word);
    }

                        /* A1 and A2 are variables: non deterministic case */

 patom3=atom_tbl+UnTag_ATM(atom3_word);
 
 if (patom3->prop.length>0)
    {
     A(0)=          atom1_word;
     A(1)=          atom2_word;
     A(2)=(WamWord) patom3;
     A(3)=(WamWord) (patom3->name+1);
     Create_Choice_Point((CodePtr) Prolog_Predicate(ATOM_CONCAT_ALT,0),4);
    }
 
 Get_Atom(atom_void,atom1_word);
 Get_Atom(UnTag_ATM(atom3_word),atom2_word);

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* ATOM_CONCAT_ALT_0                                                       */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Concat_Alt_0(void)

{
 WamWord  atom1_word,atom2_word;
 AtomInf *patom3;
 char    *name;
 char    *p;
 char    *str;
 int      l;

 Update_Choice_Point((CodePtr) Prolog_Predicate(ATOM_CONCAT_ALT,0),0);

 atom1_word=            AB(B,0);
 atom2_word=            AB(B,1);
 patom3    =(AtomInf *) AB(B,2);
 p         =(char *)    AB(B,3);

 if (*p=='\0')
     Delete_Last_Choice_Point();
  else                                           /* non deterministic case */
    {
/*   AB(B,0)=          atom1_word;                             not changed */
/*   AB(B,1)=          atom2_word;                             not changed */
/*   AB(B,2)=(WamWord) patom3;                                 not changed */
     AB(B,3)=(WamWord) (p+1);
    }

 name=patom3->name;

 l=p-name;
 MALLOC_STR(l)
 strncpy(str,name,l+1);
 str[l]='\0';
 Get_Atom(Create_Malloc_Atom(str),atom1_word);

 l=patom3->prop.length-l;
 MALLOC_STR(l)
 strcpy(str,p);
 Get_Atom(Create_Malloc_Atom(str),atom2_word);

 return TRUE;
}




#define DEREF_LG(lg_word,lg)                                                \
 Deref(lg_word,word,tag,adr)                                                \
 mask<<=1;                                                                  \
 if (tag==INT)                                                              \
    {                                                                       \
     if ((lg=UnTag_INT(word))<0)                                            \
         Pl_Err_Domain(domain_not_less_than_zero,word);                     \
     mask|=1;                                                               \
    }                                                                       \
  else                                                                      \
    {                                                                       \
     lg=0;                                                                  \
     if (tag!=REF)                                                          \
         Pl_Err_Type(type_integer,word);                                    \
    }                                                                       \
 lg_word=word;




/*-------------------------------------------------------------------------*/
/* SUB_ATOM_5                                                              */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Sub_Atom_5(WamWord atom_word,WamWord before_word,WamWord length_word,
                WamWord after_word,WamWord sub_atom_word)

{
 WamWord  word,tag,*adr;
 AtomInf *patom;
 AtomInf *psub_atom=NULL;                         /* only for the compiler */
 int      length;
 int      b,l,a;
 int      b1,l1,a1;
 Bool     nondet;
 int      mask=0;
 char    *str;

 patom=atom_tbl+Rd_Atom_Check(atom_word);
 length=patom->prop.length;


 DEREF_LG(before_word,b)
 DEREF_LG(length_word,l)
 DEREF_LG(after_word,a)


 Deref(sub_atom_word,word,tag,adr)
 if (tag!=REF && tag!=ATM)
     Pl_Err_Type(type_atom,word);
 sub_atom_word=word;
 if (tag==ATM)
    {
     psub_atom=atom_tbl+UnTag_ATM(word);
     l=psub_atom->prop.length;
     if (!Get_Integer(l,length_word))
         return FALSE;

     if ((mask&5)==5 && length!=b+l+a)                    /* B and A fixed */
         return FALSE;

     if (mask&4)                                                /* B fixed */
        {
         a=length-b-l;
         return strncmp(patom->name+b,psub_atom->name,l)==0 &&
                Get_Integer(a,after_word);
        }

     if (mask&1)                                                /* A fixed */
        {
         b=length-l-a;
         return strncmp(patom->name+b,psub_atom->name,l)==0 &&
                Get_Integer(b,before_word);
        }
     mask=8;                                      /* set sub_atom as fixed */
    }


 switch(mask)            /* mask <=7 (3 bits) B L A (1 if fixed, 0 if var) */
    {
     case 0:                                              /* nothing fixed */
     case 2:                                                    /* L fixed */
     case 4:                                                    /* B fixed */
         a=length-b-l;
         nondet=TRUE;
         break;

     case 1:                                                    /* A fixed */
         l=length-b-a;
         nondet=TRUE;
         break;

     case 3:                                                  /* L A fixed */
         b=length-l-a;
         nondet=FALSE;
         break;

     case 5:                                                  /* B A fixed */
         l=length-b-a;
         nondet=FALSE;
         break;

     case 6:                                                  /* B L fixed */
     case 7:                                                /* B L A fixed */
         a=length-b-l;
         nondet=FALSE;
         break;

     default:                                            /* sub_atom fixed */
         if ((str=strstr(patom->name+b,psub_atom->name))==NULL)
             return FALSE;

         b=str-patom->name;
         a=length-b-l;
         nondet=TRUE;
         break;
    }

 if (b<0 || l<0 || a<0)
     return FALSE;

 if (nondet && Compute_Next_BLA(mask,patom,psub_atom,b,l,a,&b1,&l1,&a1))
    {                                            /* non deterministic case */
     A(0)=          before_word;
     A(1)=          length_word;
     A(2)=          after_word;
     A(3)=          sub_atom_word;
     A(4)=(WamWord) patom;
     A(5)=(WamWord) psub_atom;
     A(6)=          mask;
     A(7)=          b1;
     A(8)=          l1;
     A(9)=          a1;

     Create_Choice_Point((CodePtr) Prolog_Predicate(SUB_ATOM_ALT,0),10);
    }

 if (mask<=7)
    {
     MALLOC_STR(l)
     strncpy(str,patom->name+b,l);
     str[l]='\0';
     Get_Atom(Create_Malloc_Atom(str),sub_atom_word);
     Get_Integer(l,length_word);
    }

 return Get_Integer(b,before_word) && Get_Integer(a,after_word);
}




/*-------------------------------------------------------------------------*/
/* SUB_ATOM_ALT_0                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Sub_Atom_Alt_0(void)

{
 WamWord  before_word,length_word,after_word,sub_atom_word;
 AtomInf *patom;
 AtomInf *psub_atom;
 int      b,l,a;
 int      b1,l1,a1;
 int      mask;
 char    *str;

 Update_Choice_Point((CodePtr) Prolog_Predicate(SUB_ATOM_ALT,0),0);

 before_word  =            AB(B,0);
 length_word  =            AB(B,1);
 after_word   =            AB(B,2);
 sub_atom_word=            AB(B,3);
 patom        =(AtomInf *) AB(B,4);
 psub_atom    =(AtomInf *) AB(B,5);
 mask         =            AB(B,6);
 b            =            AB(B,7);
 l            =            AB(B,8);
 a            =            AB(B,9);


 if (!Compute_Next_BLA(mask,patom,psub_atom,b,l,a,&b1,&l1,&a1))
     Delete_Last_Choice_Point();
  else                                           /* non deterministic case */
    {
/*   AB(B,0)=          before_word;                            not changed */
/*   AB(B,1)=          length_word;                            not changed */
/*   AB(B,2)=          after_word;                             not changed */
/*   AB(B,3)=          sub_atom_word;                          not changed */
/*   AB(B,4)=(WamWord) patom;                                  not changed */
/*   AB(B,5)=(WamWord) psub_atom;                              not changed */
/*   AB(B,6)=          mask;                                   not changed */
     AB(B,7)=          b1;
     AB(B,8)=          l1;
     AB(B,9)=          a1;
    }

 if (mask<=7)
    {
     MALLOC_STR(l)
     strncpy(str,patom->name+b,l);
     str[l]='\0';
     Get_Atom(Create_Malloc_Atom(str),sub_atom_word);
     Get_Integer(l,length_word);
    }

 return Get_Integer(b,before_word) && Get_Integer(a,after_word);
}




/*-------------------------------------------------------------------------*/
/* COMPUTE_NEXT_BLA                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static Bool Compute_Next_BLA(int mask,AtomInf *patom,AtomInf *psub_atom,
                             int b,int l,int a,int *b1,int *l1,int *a1)

{
 int   length=patom->prop.length;
 char *str;


 switch(mask)                /* mask (3 bits) B L A (1 if fixed, 0 if var) */
    {
     case 0:                                              /* nothing fixed */
         if (++l>length-b)
            {
             l=0;
             if (++b>length)
                 return FALSE;
            }
         a=length-b-l;
         break;

     case 1:                                                    /* A fixed */
         if (++b>length-a)
             return FALSE;
         l=length-b-a;
         break;

     case 2:                                                    /* L fixed */
         if (++b>length-l)
             return FALSE;
         a=length-b-l;
         break;

     case 4:                                                    /* B fixed */
         if (++l>length-b)
             return FALSE;
         a=length-b-l;
         break;

     default:                                            /* sub_atom fixed */
         if (++b>length-l)
             return FALSE;
         if ((str=strstr(patom->name+b,psub_atom->name))==NULL)
             return FALSE;
         b=str-patom->name;
         a=length-b-l;
         break;
    }

 *b1=b;
 *l1=l;
 *a1=a;

 return TRUE;
}




/*-------------------------------------------------------------------------*/
/* CREATE_MALLOC_ATOM                                                      */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static int Create_Malloc_Atom(char *str)

{
 int atom;
 int nb=nb_atom;

 atom=Create_Atom(str);
 if (nb==nb_atom)
     Free(str);
 return atom;
}




/*-------------------------------------------------------------------------*/
/* ATOM_CHARS_2                                                            */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Chars_2(WamWord atom_word,WamWord chars_word)

{
 WamWord word,tag,*adr;

 Deref(atom_word,word,tag,adr)
 if (tag!=REF)
     return Un_Chars_Check(Rd_String_Check(word),chars_word);

 return Un_String_Check(Rd_Chars_Check(chars_word),atom_word);
}




/*-------------------------------------------------------------------------*/
/* ATOM_CODES_2                                                            */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Codes_2(WamWord atom_word,WamWord codes_word)

{
 WamWord word,tag,*adr;

 Deref(atom_word,word,tag,adr)
 if (tag!=REF)
     return Un_Codes_Check(Rd_String_Check(word),codes_word);

 return Un_String_Check(Rd_Codes_Check(codes_word),atom_word);
}




/*-------------------------------------------------------------------------*/
/* NUMBER_ATOM_2                                                           */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Number_Atom_2(WamWord number_word,WamWord atom_word)

{
 WamWord  word,tag,*adr;
 char    *str;

 Deref(atom_word,word,tag,adr)
 if (tag==ATM)
     return String_To_Number(atom_tbl[UnTag_ATM(word)].name,number_word);

 if (tag!=REF)
     Pl_Err_Type(type_atom,word);

 Deref(number_word,word,tag,adr)
 if (tag==INT)
    {
     sprintf(glob_buff,"%d",UnTag_INT(word));
     return Un_String_Check(glob_buff,atom_word);
    }

 str=Float_To_String(Rd_Number_Check(word));
 return Un_String_Check(str,atom_word);
}




/*-------------------------------------------------------------------------*/
/* NUMBER_CHARS_2                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Number_Chars_2(WamWord number_word,WamWord chars_word)

{
 WamWord  word,tag,*adr;
 WamWord *lst_adr,list_word;
 char    *str=glob_buff;
 int      atom;

 list_word=chars_word;
 for(;;)
    {
     Deref(list_word,word,tag,adr)

     if (word==NIL_WORD)
         break;

     if (tag!=LST)
         goto from_nb;

     lst_adr=UnTag_LST(word);
     Deref(Car(lst_adr),word,tag,adr)
     atom=UnTag_ATM(word);
     if (tag!=ATM || atom_tbl[atom].prop.length!=1)
         goto from_nb;

     *str++=atom_tbl[atom].name[0];
     list_word=Cdr(lst_adr);
    }

 *str='\0';
 return String_To_Number(glob_buff,number_word);

from_nb:
 Deref(number_word,word,tag,adr)
 if (tag==INT)
    {
     sprintf(glob_buff,"%d",UnTag_INT(word));
     return Un_Chars(glob_buff,chars_word);
    }

 if (tag!=REF)
    {
     str=Float_To_String(Rd_Number_Check(word));
     return Un_Chars(str,chars_word);
    }

 Rd_Chars_Check(chars_word);            /* only to raise the correct error */
 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* NUMBER_CODES_2                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Number_Codes_2(WamWord number_word,WamWord codes_word)

{
 WamWord  word,tag,*adr;
 WamWord *lst_adr,list_word;
 char    *str=glob_buff;
 int      c;

 list_word=codes_word;
 for(;;)
    {
     Deref(list_word,word,tag,adr)

     if (word==NIL_WORD)
         break;

     if (tag!=LST)
         goto from_nb;

     lst_adr=UnTag_LST(word);
     Deref(Car(lst_adr),word,tag,adr)
     c=UnTag_INT(word);
     if (tag!=INT || !Is_Valid_Code(c))
         goto from_nb;

     *str++=c;
     list_word=Cdr(lst_adr);
    }

 *str='\0';
 return String_To_Number(glob_buff,number_word);

from_nb:
 Deref(number_word,word,tag,adr)
 if (tag==INT)
    {
     sprintf(glob_buff,"%d",UnTag_INT(word));
     return Un_Codes(glob_buff,codes_word);
    }

 if (tag!=REF)
    {
     str=Float_To_String(Rd_Number_Check(word));
     return Un_Codes(str,codes_word);
    }

 Rd_Codes_Check(codes_word);            /* only to raise the correct error */
 return FALSE;
}




/*-------------------------------------------------------------------------*/
/* CHAR_CODE_2                                                             */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Char_Code_2(WamWord char_word,WamWord code_word)

{
 WamWord word,tag,*adr;

 Deref(char_word,word,tag,adr)
 if (tag!=REF)
     return Un_Code_Check(Rd_Char_Check(word),code_word);

 return Un_Char_Check(Rd_Code_Check(code_word),char_word);
}




/*-------------------------------------------------------------------------*/
/* NAME_2                                                                  */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Name_2(WamWord atomic_word,WamWord codes_word)

{
 WamWord  word,tag,*adr;
 int      syn_flag;
 Bool     is_number;
 char    *str;


 Deref(atomic_word,word,tag,adr)
 if (tag==ATM)
     return Atom_Codes_2(word,codes_word);

 if (tag==INT || tag==FLT)
     return Number_Codes_2(word,codes_word);

 if (tag!=REF)
     Pl_Err_Type(type_atomic,word);


 str=Rd_Codes_Check(codes_word);

 syn_flag=Flag_Value(FLAG_SYNTAX_ERROR);
 Flag_Value(FLAG_SYNTAX_ERROR)=FLAG_VALUE_FAIL;

 is_number=String_To_Number(str,word);            /* only fails on syn err */

 Flag_Value(FLAG_SYNTAX_ERROR)=syn_flag;

 if (is_number)
     return TRUE;

 return Un_String(str,word);
}




/*-------------------------------------------------------------------------*/
/* LOWER_UPPER_2                                                           */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Lower_Upper_2(WamWord lower_word,WamWord upper_word)

{
 WamWord word,tag,*adr;

 Deref(lower_word,word,tag,adr)
 if (tag!=REF)
     return Un_Char_Check(toupper(Rd_Char_Check(word)),upper_word);

 return Un_Char_Check(tolower(Rd_Char_Check(upper_word)),lower_word);
}




/*-------------------------------------------------------------------------*/
/* STRING_TO_NUMBER                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
static Bool String_To_Number(char *str,WamWord number_word)

{
 WamWord  word;
 int      stm;
 StmInf  *pstm;
 Bool     eof;

 Check_For_Un_Number(number_word);

#if 0  /* #if 0 since layout leading chars allowed in ISO cf. number_chars */
 if (!isdigit(*str) && *str!='-')
    {
     Set_Last_Syntax_Error("",1,1,"non numeric character");
     goto err;
    }
#endif

 stm=Add_Str_Stream(TRUE,str);
 pstm=stm_tbl+stm;

 word=Read_Number(pstm);
 eof=(Stream_Peekc(pstm)==EOF);

 if (word==NOT_A_WAM_WORD || !eof)
     Set_Last_Syntax_Error(atom_tbl[pstm->atom_file_name].name,
                           pstm->line_count+1,pstm->line_pos+1,
                           "non numeric character");

 Delete_Str_Stream(stm);

 if (word==NOT_A_WAM_WORD || !eof)
    {
#if 0
err:
#endif
     Syntax_Error(Flag_Value(FLAG_SYNTAX_ERROR));
     return FALSE;
    }

 return Unify(word,number_word);
}




/*-------------------------------------------------------------------------*/
/* ATOM_HASH_2                                                             */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Atom_Hash_2(WamWord atom_word,WamWord hash_word)

{
 WamWord word,tag,*adr;
 int     hash;

 Deref(atom_word,word,tag,adr)
 atom_word=word;

 if (tag!=REF)
     return Un_Positive_Check(Rd_Atom_Check(word),hash_word);

 hash=Rd_Positive_Check(hash_word);

 return Is_Valid_Atom(hash) && Un_Atom_Check(hash,atom_word);
}




/*-------------------------------------------------------------------------*/
/* CURRENT_ATOM_2                                                          */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Current_Atom_2(WamWord atom_word,WamWord hide_word)

{
 WamWord word,tag,*adr;
 Bool    hide;
 int     atom;

 hide=Rd_Integer_Check(hide_word);
 
 Deref(atom_word,word,tag,adr)
 if (tag!=REF)
     return *Rd_String_Check(word)!='$' || !hide;

 atom=-1;
 for(;;)
    {
     atom=Find_Next_Atom(atom);
     if (atom== -1)
         return FALSE;

     if (!hide || atom_tbl[atom].name[0]!='$')
         break;
    }
                                                 /* non deterministic case */
 A(0)=atom_word;
 A(1)=hide;
 A(2)=atom;
 Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ATOM_ALT,0),3);

 return Get_Atom(atom,atom_word);
}




/*-------------------------------------------------------------------------*/
/* CURRENT_ATOM_ALT_0                                                      */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Current_Atom_Alt_0(void)

{
 WamWord atom_word;
 Bool    hide;
 int     atom;

 Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ATOM_ALT,0),0);

 atom_word=AB(B,0);
 hide     =AB(B,1);
 atom     =AB(B,2);

 for(;;)
    {
     atom=Find_Next_Atom(atom);
     if (atom== -1)
        {
         Delete_Last_Choice_Point();
         return FALSE;
        }

     if (!hide || atom_tbl[atom].name[0]!='$')
         break;
    }
                                                 /* non deterministic case */

/* AB(B,0)=atom_word;                                          not changed */
/* AB(B,1)=hide;                                               not changed */
 AB(B,2)=atom;

 return Get_Atom(atom,atom_word);
}




/*-------------------------------------------------------------------------*/
/* ATOM_PROPERTY_6                                                         */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void Atom_Property_6(WamWord atom_word,
                     WamWord prefix_op_word,WamWord infix_op_word,
                     WamWord postfix_op_word,
                     WamWord needs_quote_word,WamWord needs_scan_word)

{
 WamWord  word,tag,*adr;
 int      atom;
 AtomInf *patom;

 Deref(atom_word,word,tag,adr)
 atom=UnTag_ATM(word);
 patom=atom_tbl+atom;

 Get_Integer(Check_Oper(atom,PREFIX)!=0, prefix_op_word);
 Get_Integer(Check_Oper(atom,INFIX)!=0,  infix_op_word);
 Get_Integer(Check_Oper(atom,POSTFIX)!=0,postfix_op_word);

 Get_Integer(atom_tbl[atom].prop.needs_quote,needs_quote_word);
 Get_Integer(atom_tbl[atom].prop.needs_scan,needs_scan_word);
}




/*-------------------------------------------------------------------------*/
/* NEW_ATOM_3                                                              */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool New_Atom_3(WamWord prefix_word,WamWord hash_word,WamWord atom_word)

{
 int atom;
 int hash=-1;

 atom=Rd_Atom_Check(prefix_word);
 Check_For_Un_Variable(atom_word);

 if (SYS_VAR_OPTION_MASK)
    {
     hash=Rd_Positive_Check(hash_word);
     if (Is_Valid_Atom(hash) || hash>=MAX_ATOM)
         return FALSE;
    }

 return Get_Atom(Gen_New_Atom(atom_tbl[atom].name,hash),atom_word);
}
