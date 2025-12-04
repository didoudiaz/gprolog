/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Tools                                                           *
 * File  : map_test.c                                                      *
 * Descr.: test for the map data structure with red-black trees            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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
#include <time.h>

#include <assert.h>


#ifdef _MSC_VER
#define _CRT_NONSTDC_NO_DEPRECATE 1
#pragma warning(disable : 4996)
#endif

#define UNUSED(x) ((void)(x))


struct my_info
{
  int key_copy_as_int;
  double dbl;
  int no;
};


#if 1

#define MAP_KEY_TYPE                 char *
#define MAP_KEY_CMP(k1, k2)          strcmp(k1, k2)
#define MAP_VALUE_TYPE               struct my_info
#define MAP_PUT_ENTRY(map, entry)    entry->value.no = map_counter_add(map)
#define MAP_REMOVE_ENTRY(map, entry) free(entry->key)
#define MAP_CLEAR_ENTRY(map, entry)  free(entry->key)
#define MAP_NAME                     map_str

#include "map_rbtree.h"

void
test1_once(struct map_rbt *map)
{

  struct map_entry *entry;
  char key[80];
  int key_copy_as_int;
  double dbl;
  double factor = 12.5;
  bool b;

  //printf("--- put (string) ---\n");
  for(int i = 0; i < 10; i++)
    {
      key_copy_as_int = i * 5;
      sprintf(key, "%03d", key_copy_as_int);
      dbl = key_copy_as_int * factor;

      entry = map_str_put(map, strdup(key), NULL);
      
      entry->value.key_copy_as_int = key_copy_as_int;
      entry->value.dbl = dbl;

      // printf("key: %3s  no: %3d  key_copy_as_int: %3d  dbl: %f\n", entry->key, entry->value.no, entry->value.key_copy_as_int, entry->value.dbl);
    }

  assert(map_size(map) == 10);
  assert(map_counter_add(map) == 10);
  assert(map_counter_del(map) == 0);

  //printf("--- get ---\n");
  for(int i = 0; i < 10; i++)
    {
      key_copy_as_int = i * 5;
      sprintf(key, "%03d", key_copy_as_int);
      dbl = key_copy_as_int * factor;

      entry = map_str_get(map, key);
      assert(entry != NULL);
      assert(strcmp(entry->key, key) == 0);
      assert(entry->value.no == i+1);
      assert(entry->value.key_copy_as_int == key_copy_as_int);
      assert(entry->value.dbl == dbl);
    }
  assert(map_size(map) == 10);
  assert(map_counter_add(map) == 10);
  assert(map_counter_del(map) == 0);

  //printf("--- remove ---\n");
  for(int i = 0; i < 10; i+=2)
    {
      key_copy_as_int = i * 5;
      sprintf(key, "%03d", key_copy_as_int);
      dbl = key_copy_as_int * factor;

      b = map_str_remove(map, key); UNUSED(b);
      assert(b);
    }
  assert(map_size(map) == 5);
  assert(map_counter_add(map) == 10);
  assert(map_counter_del(map) == 5);

  //printf("--- read ---\n");
  for(int i = 0; i < 10; i++)
    {
      key_copy_as_int = i * 5;
      sprintf(key, "%03d", key_copy_as_int);
      dbl = key_copy_as_int * factor;

      entry = map_str_get(map, key);
      if (entry == NULL)
	{
	  assert(i % 2 == 0);
	  continue;
	}
      assert(strcmp(entry->key, key) == 0);
      assert(entry->value.no == i+1);
      assert(entry->value.key_copy_as_int == key_copy_as_int);
      assert(entry->value.dbl == dbl);

      //printf("key: %3s  no: %3d  key_copy_as_int: %3d  dbl: %f\n", entry->key, entry->value.no, entry->value.key_copy_as_int, entry->value.dbl);
    }

  assert(map_size(map) == 5);
  assert(map_counter_add(map) == 10);
  assert(map_counter_del(map) == 5);

  //printf("--- clear ---\n");
  map_str_clear(map);
  assert(map_size(map) == 0);
  assert(map_counter_add(map) == 0);
  assert(map_counter_del(map) == 0);
}

/* to test declaration + initialization for global var (pb in MSVC if cast) */
struct map_rbt map_dummy = MAP_INIT;  

void
test1(void)
{
  struct map_rbt map = MAP_INIT; /* to test declaration + initialization */
  test1_once(&map);
  test1_once(&map);		/* recheck after a clear */
}


#undef MAP_NAME
#undef MAP_KEY_TYPE
#undef MAP_KEY_CMP
#undef MAP_VALUE_TYPE
#undef MAP_PUT_ENTRY
#undef MAP_REMOVE_ENTRY
#undef MAP_CLEAR_ENTRY

#endif


#define MAP_KEY_TYPE                 int
#define MAP_KEY_CMP(k1, k2)          ((k1) - (k2))
#define MAP_VALUE_TYPE               double
#define MAP_SHOW_ENTRY(map, entry)   printf("%d", entry->key);
#define MAP_NAME                     map_int
#define MAP_RE_INCLUDE

#include "map_rbtree.h"

int cmp(const void *a, const void *b) { return *(int *) a - *(int *) b; }

#define N 10000
#define PCT_RND 20
void
test2_once(struct map_rbt *map)
{
  struct map_entry *entry;
  double factor = 12.5;
  static int key[N], s[N];
  bool creat;
  int i;

  for(i = 0; i < N; i++)
    {
      if (rand() % 100 <= PCT_RND)
	key[i] = rand() % (100 * N);
      else
	key[i] = i * 10;
    }

  map_init(map);

  
  //printf("--- PUT ---\n");
  for(i = 0; i < N; i++)
    {
      for(;;)
	{
	  entry = map_int_put(map, key[i], &creat); /* check if duplicates in rand */
	  if (creat)
	    break;
	  key[i] = rand() % (100 * N);
	}
      entry->value = key[i] * factor;
    }

  for(i = 0; i < N; i++)
    s[i] = key[i];
  qsort(s, N, sizeof(key[0]), cmp);
  
  assert(map_size(map) == N);
  assert(map_counter_add(map) == N);
  assert(map_counter_del(map) == 0);

  //printf("--- REMOVE ---\n");
  int remov = 0;
  for(i = 0; i < N; i+=5)
    {
      assert(map_int_remove(map, key[i]));
      remov++;
    }
  assert(map_size(map) == N - remov);
  assert(map_counter_add(map) == N);
  assert(map_counter_del(map) == remov);

  //printf("--- RE-PUT ---\n");
  for(i = 0; i < N; i++)
    {
      entry = map_int_put(map, key[i], &creat);
      assert(entry);
      if (creat)
	entry->value = key[i] * factor;
      else
	{
	  assert(entry->key == key[i]);
	  assert(entry->value == key[i] * factor);
	}
    }
  assert(map_size(map) == N);
  assert(map_counter_add(map) == N + remov);
  assert(map_counter_del(map) == remov);
  
  if (N < 100)
    {
      printf("\n--- TREE ---\n\n");
      map_int_show_tree(map);
    }


  //printf("\n--- NAVIG ASCENDING ---\n");

  //printf("first: %d = %f\n", map_int_first(map)->key, map_int_first(map)->value);
  assert(map_int_first(map)->key == s[0]);
  for(i = 0; i < N; i++)
    {
      entry = map_int_next(map, s[i], true);
      if (i < N - 1)
	{
	  assert(entry);
	  assert(entry->key == s[i + 1]);
	}
      else
	assert(entry == NULL);
      assert(entry == map_int_next(map, s[i] + 1, false));
      entry = map_int_next(map, s[i] - 1, true);
      assert(entry);
      assert(entry->key == s[i]);
    }
  assert(map_int_next(map, s[N - 1], true) == NULL);
  assert(map_int_next(map, s[N - 1] + 1, false) == NULL);

  
  //printf("\n--- NAVIG DESCENDING ---\n");
  
  //printf("last:  %d = %f\n", map_int_last(map)->key, map_int_last(map)->value);
  assert(map_int_last(map)->key == s[N - 1]);
  for(i = N - 1; i >= 0; i--)
    {
      entry = map_int_prev(map, s[i], true);
      if (i > 0)
	{
	  assert(entry);
	  assert(entry->key == s[i - 1]);
	}
      else
	assert(entry == NULL);
      assert(entry == map_int_prev(map, s[i] - 1, false));
      entry = map_int_prev(map, s[i] + 1, true);
      assert(entry);
      assert(entry->key == s[i]);
    }
  assert(map_int_prev(map, s[0], true) == NULL);
  assert(map_int_prev(map, s[0] - 1, false) == NULL);

  //printf("\n--- ITER ASCENDING ---\n");
  i = 0;
  //  for(entry = map_int_first(map) ; entry; entry = map_int_next_entry(map, entry))
  map_foreach(map, entry)
    {
      assert(entry->key == s[i]);
      i++;
    }
  assert(i == N);
  
  //printf("\n--- ITER DESCENDING ---\n");
  i = N - 1;
  //  for(entry = map_int_last(map) ; entry; entry = map_int_prev_entry(map, entry))
  map_foreach_rev(map, entry)
    {
      assert(entry->key == s[i]);
      i--;
    }
  assert(i == -1);

  //printf("\n--- ITER ASCENDING + REMOVE\n");
  i = 0;
  for(entry = map_int_first(map) ; entry; )
    {
      assert(entry->key == s[i]);
      struct map_entry *entry0 = entry;
      entry = map_int_next_entry(map, entry);
      if (i % 2 == 1)
	{
	  map_int_remove_entry(map, entry0);
	  remov++;
	}
      i++;
    }
  assert(map_size(map) == (N + 1) / 2);
  assert(map_counter_del(map) == remov);

  //printf("--- RE-PUT ---\n");
  for(i = 0; i < N; i++)
    {
      entry = map_int_put(map, key[i], &creat);
      assert(entry);
      if (creat)
	entry->value = key[i] * factor;
      else
	{
	  assert(entry->key == key[i]);
	  assert(entry->value == key[i] * factor);
	}
    }
  assert(map_size(map) == N);
  assert(map_counter_del(map) == remov);

  //printf("\n--- ITER DESCENDING + REMOVE\n");
  i = N - 1;
  for(entry = map_int_last(map) ; entry; )
    {
      assert(entry->key == s[i]);
      struct map_entry *entry0 = entry;
      entry = map_int_prev_entry(map, entry);
      if (i % 2 == 1)
	{
	  map_int_remove_entry(map, entry0);
	  remov++;
	}
      i--;
    }
  assert(map_size(map) == (N + 1) / 2);
  assert(map_counter_del(map) == remov);

  //printf("\n--- CLEAR ---\n\n");
  map_int_clear(map);
}


void
test2(void)
{
  struct map_rbt map;
  long seed = (long) time(NULL);

  //printf("\nTest 2\n");
  //printf("N: %d\n", N);
  //printf("seed: %ld\n", seed);
  srand(seed);


  map_init(&map);
  test2_once(&map);
  test2_once(&map);		/* recheck after a clear */
}




int
main(int argc, char *argv[])
{
  test1();
  test2();
  puts("Map with Red-Black Tree checks succeeded");
  return 0;
}
