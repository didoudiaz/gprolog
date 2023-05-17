/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Tools                                                           *
 * File  : map_example.c                                                   *
 * Descr.: example using the map data structure with red-black trees       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
#include <string.h>

#if 1
#define MAP_KEY_TYPE int
#define MAP_KEY_CMP(x, y) ((x) - (y))
#define MAP_SHOW_ENTRY(map, entry) printf("%d", entry->key)
#include "map_rbtree.h"

void
example1(void)
{
  struct map_rbt map = MAP_INIT;

  map_init(&map);		/* redundant but to test the two macros */

  for(int i = 0; i < 100; i += 2)
    map_put(&map, i, NULL);

  for(int i = 0; i < 100; i += 2)
    if (i % 3 == 0)
      map_remove(&map, i);

  printf("values: ");
  for(struct map_entry *entry = map_first(&map); entry != NULL; entry = map_next_entry(&map, entry))
    //  map_foreach(&map, entry)
    printf("%d ", entry->key);

  printf("\nmap size: %d\n", map_size(&map));

  printf("is 10 inside: %s\n", map_contains(&map, 10) ? "Yes" : "No");
  printf("is 25 inside: %s\n", map_contains(&map, 15) ? "Yes" : "No");
  printf("is 36 inside: %s\n", map_contains(&map, 36) ? "Yes" : "No");

  map_show_tree(&map);
  
  map_clear(&map);
}
#endif



struct str_len
{
  char *str;
  int len;
};

int
str_len_cmp(struct str_len *x, struct str_len *y)
{
  int i = 0;
  while(i < x->len && i < y->len && x->str[i] == y->str[i])
    i++;

  if (i < x->len && i < y->len)
    return (unsigned char) x->str[i] - (unsigned char) y->str[i];

  return x->len - y->len;
}


/* key is a struct, it can be passed either by value (copy of the content) 
 * or by pointer (faster). 
 * In both cases the space for the whole struct is reserved in the entry.
 */
#if 1
#define MAP_KEY_PASSED_BY_PTR
#endif

#undef MAP_KEY_TYPE
#undef MAP_KEY_CMP
#undef MAP_SHOW_ENTRY
#define MAP_KEY_TYPE struct str_len
#ifndef MAP_KEY_PASSED_BY_PTR
#define MAP_KEY_CMP(x, y) (str_len_cmp(&x, &y))
#else
#define MAP_KEY_CMP(x, y) (str_len_cmp(x, y))
#endif
#define MAP_VALUE_TYPE char *
#define MAP_SHOW_ENTRY(map, entry) printf("%.*s = %s", entry->key.len, entry->key.str, entry->value)
#define MAP_NAME map_sl
#define MAP_RE_INCLUDE
#include "map_rbtree.h"

char *t[] =  {
  "lackLACK", "unavailabilityUNAVAILABILITY", "academicACADEMIC", "scholasticSCHOLASTIC",
  "boundaryBOUNDARY", "absenceABSENCE", "boundsBOUNDS", "confinesCONFINES", "borrowBORROW",
  "takeTAKE", "characteristicCHARACTERISTIC", "typicalTYPICAL", "dialogueDIALOGUE",
  "conversationCONVERSATION", "embraceEMBRACE", "acceptACCEPT", "foreignFOREIGN",
  "overseasOVERSEAS", "generateGENERATE", "producePRODUCE", "causeCAUSE",
  "highlightHIGHLIGHT", "callCALL", "incorporateINCORPORATE", "includeINCLUDE",
  "justifyJUSTIFY", "defendDEFEND", "maintainMAINTAIN", NULL };
    

void
example2()
{
  struct map_rbt map;
  struct str_len sl;

  map_sl_init(&map);

  for(int i = 0; t[i]; i++)
    {
      int len = (int) strlen(t[i]) / 2;
      sl.str = t[i];
      sl.len = len;
#ifndef MAP_KEY_PASSED_BY_PTR
      map_sl_put(&map, sl, NULL)->value = t[i] + len;
#else
      map_sl_put(&map, &sl, NULL)->value = t[i] + len;      
#endif
    }
  map_sl_show_tree(&map);
}


int
main(int argc, char *argv[])
{
  example1();
  printf("\n");
  example2();
  /* struct str_len x = { "abcd", 4 }; */
  /* struct str_len y = { "abcdef", 3 }; */
  /* printf("cmp: %d\n", str_len_cmp(&x, &y)); */

  return 0;
}
