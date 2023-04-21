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
#include <string.h>

#define MAP_KEY_TYPE int
#define MAP_KEY_CMP(x, y) ((x) - (y))
#define MAP_SHOW_ENTRY(map, entry) printf("%d", entry->key)
#include "map_rbtree.h"

void
example1(void)
{
  struct map_rbt map;

  map_init(&map);

  for(int i = 0; i < 100; i+=2)
    map_put(&map, i, NULL);

  for(int i = 0; i < 100; i+=2)
    if (i % 3 == 0)
      map_remove(&map, i);

  printf("values: ");
  map_foreach(&map, entry)
    printf("%d ", entry->key);

  printf("\nmap size: %d\n", map_size(&map));

  printf("is 10 inside: %s\n", map_contains(&map, 10) ? "Yes" : "No");
  printf("is 25 inside: %s\n", map_contains(&map, 15) ? "Yes" : "No");
  printf("is 36 inside: %s\n", map_contains(&map, 36) ? "Yes" : "No");

  map_show_tree(&map);
  
  map_clear(&map);
}

int
main(int argc, char *argv[])
{
  example1();
  return 0;
}
