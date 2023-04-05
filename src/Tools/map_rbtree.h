// SPDX-License-Identifier: GPL-2.0-or-later OR MIT
/*
  An Efficient C Sorted Map Implementation Based on Red Black Trees.

  (C) 2023  Daniel Diaz <didou.diaz@gmail.com>

  file: map_rbtree.h
*/

/* 
 * A Map is a data structure which holds key-value pairs. Keys are unique and
 * are maintained ordered. This implementation uses Red-Black trees ensuring
 * log(n) time cost for the put, get, remove operations.
 *
 * This implementation is based on a the linux kernel rbtree tool (version 6.2):
 *    https://github.com/torvalds/linux/blob/master/tools/lib/rbtree.c
 *
 * This rbtree tool uses an invasive approach for efficiency: it only maintains
 * the tree (as linked nodes) and ensures it is well balanced (wrt RB Tree). 
 * The user code (here the Map) is responsible for defining a data type for the 
 * pairs key-value which must also include a rbtree node structure (see MapEntry). 
 * The user has to allocates/frees these nodes. More info on the rbtree API:
 *    https://www.kernel.org/doc/Documentation/rbtree.txt
 *    https://docs.kernel.org/core-api/rbtree.html
 *
 * API description
 *
 * Define some macros before the inclusion of the header file:
 *
 *   MAP_KEY_TYPE:      type of the key
 *   MAP_KEY_CMP(x, y): compare 2 keys and return a value <,=,> 0 if x <,=,> y
 *   MAP_VALUE_TYPE:    type of  value (any C type, including a struct).
 *                      Can be omitted if there is no attached value (even if
 *                      not really a map can be usefull to store sorted sets).
 *
 *   Other optional macros will be detailled below.
 *
 * Include the header file:
 *
 *   #include "map_rbtree.h"
 *
 *   This header file, defines 2 types: 
 *
 *   Map:      the general type of a Map (same as struct map_rb)
 *   MapEntry: to store a key-value pair (same as struct map_entry)
 *             A C struct containing the fields: 
 *             - key.
 *             - value (if MAP_VALUE_TYPE is defined).
 *             (there is also the rbtree node for internal only purpose)
 *
 * Declaration of a Map variable: 
 *
 *   Map my_map;
 *
 * Initialization:
 * 
 *   void map_init(Map *map)
 *
 *   map:     the map.
 *
 *   This initializes map as a new (empty) map.
 *
 * Insertion:
 *
 *   MapEntry *map_put(Map *map, MAP_KEY_TYPE key, bool *created)
 *
 *   map:     the map.
 *   key:     the key to insert.
 *   created: if != NULL will contain true if the key has been added and
 *            false if the key was already present in the map.
 *   return:  the entry associated to the key (created or already existing). 
 *            The value field can then be modified by the user code.
 *
 *   When a new key is added, the optional macro MAP_PUT_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 * Retrieval:
 *
 *   MapEntry *map_get(Map *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  the entry associated to the key or NULL if there is no mapping 
 *            for the key.
 *
 *   bool map_contains(Map *map, MAP_KEY_TYPE key)   defined as a macro
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  true iff the key exists.
 *
 *   MapEntry *map_locate(Map *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  the entry associated to the closest key or NULL if the map is empty.
 *
 *   This function is used internally (e.g. by map_get, map_next, map_prev).
 *
 * Deletions:
 *
 *   bool map_remove(Map *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to remove.
 *   return:  true iff the key was found (and thus removed).
 *
 *   When a key is removed, the optional macro MAP_REMOVE_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 *   void map_remove_entry(Map *map, MapEntry *entry)
 *
 *   map:     the map.
 *   entry:   an entry (should be a valid and existing entry of the map).
 *
 *   When the entry is removed, the optional macro MAP_REMOVE_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 *   NB: This function is faster than map_remove() since there is no need to locate 
 *   the entry in the tree.
 *
 * Information:
 *
 *   KEY_TYPE map_entry_key(MapEntry *entry)  a macro (same as entry->key)
 *   VALUE_TYPE map_entry_value(MapEntry *entry) a macro (same as entry->value)
 *
 *   bool map_is_empty(Map *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  true iff the map is empty.
 *
 *   int map_size(Map *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number of key-value mappings in the map.
 *
 *   int map_counter_add(Map *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number insertion operations.
 *
 *   int map_counter_del(Map *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number remove operations.
 *
 *   void map_show_tree(Map *map)
 *
 *   map:     the map.
 *
 *   Print a tree representation of the map (mainly for debug purpose). Only
 *   available if the macro MAP_SHOW_ENTRY(map, entry) is defined for printing
 *   an entry on stdout.
 *
 * Clearing:
 *
 *   void map_clear(Map *map)
 *
 *   map:     the map.
 *
 *   When a key will be cleared, the optional macro MAP_CLEAR_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 * Navigation:
 *
 *   MapEntry *map_first(Map *map)
 *
 *   map:     the map.
 *   return:  the entry associated with the first key (lowest) in the map
 *            (or NULL if the map is empty).
 *
 *   MapEntry *map_last(Map *map)
 *
 *   map:     the map.
 *   return:  the entry associated with the last key (highest) in the map
 *            (or NULL if the map is empty).
 *
 *   MapEntry *map_next(Map *map, MAP_KEY_TYPE key, bool strict)
 *
 *   map:     the map.
 *   key:     the given key.
 *   strict:  if true look for the next key strictly greater than 
 *            (if false for a key greater then or equal to).
 *   return:  the entry associated with the least key greater than 
 *            (greater than or equal to if !strict) the given key 
 *            (or NULL if there is no such key).
 *
 *   MapEntry *map_prev(Map *map, MAP_KEY_TYPE key, bool strict)
 *
 *   map:     the map.
 *   key:     the given key.
 *   strict:  if true look for the next key strictly less than 
 *            (if false for a key less than or equal to).
 *   return:  the entry associated with the greatest key less than 
 *            (less than or equal to if !strict) the given key 
 *            (or NULL if there is no such key).
 *
 *   MapEntry *map_next_entry(MapEntry *entry)
 *
 *   map:     the map.
 *   key:     the given key.
 *   entry:   an entry (should be a valid and existing entry of the map).
 *   return:  the entry associated with the least key greater than the given key
 *            (or NULL if there is no such key).
 *
 *   NB: This function is faster than map_next() since there is no need to locate 
 *   the entry in the tree.
 *
 *   MapEntry *map_prev_entry(MapEntry *entry)
 *
 *   map:     the map.
 *   key:     the given key.
 *   entry:   an entry (should be a valid and existing entry of the map).
 *   return:  the entry associated with the greatest key less than the given key
 *            (or NULL if there is no such key).
 *
 *   NB: This function is faster than map_prev() since there is no need to locate 
 *   the entry in the tree.
 *
 *   The following macros (based on above functions) provide simple foreach loops
 *
 *   map_foreach(map, entry):    from least to greatest
 *   map_foreach_rev(map, entry) from greatest to least
 *
 * All these functions are defined in the header file (the associated symbol will
 * be in the associated user object file). Their scope can be controlled with a
 * macro:
 *
 *   MAP_STATIC: each API function is defined as static.
 * 
 * Using mutiple maps in a same source:
 *
 *   If several Maps are needed in a same source, it is possible to include 
 *   several times map_rbtree.h. Since all above methodes are defined in the 
 *   header, at each re-include it is necessary to provide different function 
 *   names (and also for the MapEntry header which depends on the type of the 
 *   key and the value. For this purpose some optional macros can be defined:
 *
 *   MAP_PREFIX:     a prefix added at the beginning of all function names.
 *   MAP_SUFFIX:     a suffix added at the end of all function names and at the end
 *                   if the MapEntry type name. Thus MAP_SUFFIX must be defined
 *                   before a re-inclusion of the header file.
 *   MAP_RE_INCLUDE: must be defined before each new re-inclusion of the header file
 *                   (to avoid multiple accidental inclusions).
 *
 *   The schema is thus as follows:
 *
 *   #define ... macros for the first map ...
 *   #include "map_rbtree.h"
 *   ... code using the first map ...
 *
 *   #undef  ... macros which must be redefined ...
 *   #define ... macros for the second map ...
 *   #define MAP_SUFFIX ...
 *   #define MAP_RE_INCLUDE
 *   #include "map_rbtree.h"
 *   ... code using the second map ...
 *
 *   NB: these macros are designed to simplify the usage: whatever MAP_PREFIX
 *   and MAP_SUFFIX, simply use the above API names.
 */


#if !defined(_MAP_H) || defined(MAP_RE_INCLUDE)

#ifndef _MAP_H	       /* first include (things to be defined only once) */

#include <stdlib.h>
#include "rbtree.h"

typedef struct map_rb
{
  int size;			/* number of entries in the map */
  int counter_add; 		/* number of insertions */
  int counter_del; 		/* number of deletions */
  struct rb_root root;		/* root of the RB tree */
}Map;

#else  /* _MAP_H (re-include) */

#ifndef MAP_SUFFIX
#error MAP_SUFFIX must be defined for multiple inclusion of map_rbtree.h
#endif

#undef MAP_STATIC_API
#undef MAP_MK_TYPE
#undef MAP_MK_FCT

#endif	/* !_MAP_H */

#define CAT21(a, b)    a ## b
#define CAT2(a, b)     CAT21(a, b)

#define CAT31(a, b, c) a ## b ## c
#define CAT3(a, b, c)  CAT31(a, b, c)

#ifdef MAP_PREFIX
#   ifdef MAP_SUFFIX
#      define MAP_MK_FCT(name)  CAT3(MAP_PREFIX, name, MAP_SUFFIX)
#   else
#      define MAP_MK_FCT(name)  CAT2(MAP_PREFIX, name)
#   endif
#else
#   ifdef MAP_SUFFIX
#      define MAP_MK_FCT(name)  CAT2(name, MAP_SUFFIX)
#   else
#      define MAP_MK_FCT(name)  name
#   endif
#endif

#ifdef MAP_SUFFIX
#   define MAP_MK_TYPE(name)  CAT2(name, MAP_SUFFIX)
#else
#   define MAP_MK_TYPE(name)  name
#endif


#ifdef MAP_STATIC
#   define MAP_STATIC_API static
#   ifdef __GNUC__
#      pragma GCC diagnostic push
#      pragma GCC diagnostic ignored "-Wunused-function"
#   endif
#else
#   define MAP_STATIC_API
#endif

/* add prefix and suffix to API function names (not macros else redefine warnings) */

#define map_entry          MAP_MK_TYPE(map_entry)
#define MapEntry           MAP_MK_TYPE(MapEntry)

#define map_init           MAP_MK_FCT(map_init)
#define map_put            MAP_MK_FCT(map_put)
#define map_locate         MAP_MK_FCT(map_locate)
#define map_get            MAP_MK_FCT(map_get)
 //#define map_contains    MAP_MK_FCT(map_contains)
#define map_remove_entry   MAP_MK_FCT(map_remove_entry)
#define map_remove         MAP_MK_FCT(map_remove)
 //#define map_size        MAP_MK_FCT(map_size)
 //#define map_counter_add MAP_MK_FCT(map_counter_add)
 //#define map_counter_del MAP_MK_FCT(map_counter_del)
 //#define map_is_empty    MAP_MK_FCT(map_is_empty)
#define map_first          MAP_MK_FCT(map_first)
#define map_last           MAP_MK_FCT(map_last)
#define map_next           MAP_MK_FCT(map_next)
#define map_next_entry     MAP_MK_FCT(map_next_entry)
#define map_prev           MAP_MK_FCT(map_prev)
#define map_prev_entry     MAP_MK_FCT(map_prev_entry)
#define map_clear          MAP_MK_FCT(map_clear)
#define map_show_tree_rec  MAP_MK_FCT(map_show_tree_rec)
#define map_show_tree      MAP_MK_FCT(map_show_tree)

#define map_foreach(map, entry) \
  for(MapEntry *entry = map_first(map); entry; entry = map_next_entry(entry))

#define map_foreach_rev(map, entry) \
  for(MapEntry *entry = map_last(map); entry; entry = map_prev_entry(entry))


/* the MapEntry node */

typedef struct map_entry
{
  MAP_KEY_TYPE key;
#ifdef MAP_VALUE_TYPE
  MAP_VALUE_TYPE value;
#endif
  struct rb_node node_inv;	/* the invasive rbtree node */
} MapEntry;


/* API: macros */

#define map_size(map)          ((map)->size)
#define map_counter_add(map)   ((map)->counter_add)
#define map_counter_del(map)   ((map)->counter_del)
#define map_is_empty(map)      ((map)->size == 0)
#define map_contains(map, key) (map_get((map), (key)) != NULL)


/* API: functions */

MAP_STATIC_API void
map_init(Map *map)
{
  map->root = RB_ROOT;
  map->size = 0;
  map->counter_add = 0;
  map->counter_del = 0;
}




MAP_STATIC_API MapEntry *
map_put(Map *map, MAP_KEY_TYPE key, bool *created)
{
  struct rb_node **node = &(map->root.rb_node), *parent = NULL;
  MapEntry *entry;

  /* Figure out where to put new node */
  while (*node)
    {
      entry = container_of(*node, MapEntry, node_inv);
      int result = MAP_KEY_CMP(key, entry->key);

      parent = *node;
      if (result < 0)
	node = &((*node)->rb_left);
      else if (result > 0)
	node = &((*node)->rb_right);
      else
	{
	  if (created)
	    *created = false;
	  return entry;
	}
    }


  /* Add new node and rebalance tree */
  entry = (MapEntry *) calloc(1, sizeof(MapEntry));
  entry->key = key;

  map->size++;
  map->counter_add++;

#ifdef MAP_PUT_ENTRY
  MAP_PUT_ENTRY(map, entry);
#endif

  rb_link_node(&entry->node_inv, parent, node);
  rb_insert_color(&entry->node_inv, &(map->root));

  if (created)
    *created = true;
  
  return entry;
}



MAP_STATIC_API MapEntry *
map_locate(Map *map, MAP_KEY_TYPE key)
{
  struct rb_node *node = map->root.rb_node;
  MapEntry *entry = NULL;

  while (node)
    {
      entry = container_of(node, MapEntry, node_inv);
      int result = MAP_KEY_CMP(key, entry->key);

      if (result < 0)
	node = node->rb_left;
      else if (result > 0)
	node = node->rb_right;
      else
	break;
    }

  return entry;			/* returns the closest entry (or NULL if tree is empty) */
}



MAP_STATIC_API MapEntry *
map_get(Map *map, MAP_KEY_TYPE key)
{
  MapEntry *entry = map_locate(map, key);
  return (entry && MAP_KEY_CMP(key, entry->key) == 0) ? entry : NULL;
}



MAP_STATIC_API void
map_remove_entry(Map *map, MapEntry *entry)
{
  rb_erase(&(entry->node_inv), &(map->root));
#ifdef MAP_REMOVE_ENTRY
  MAP_REMOVE_ENTRY(map, entry);
#endif
  free(entry);
  map->size--;
  map->counter_del++;
}



MAP_STATIC_API bool
map_remove(Map *map, MAP_KEY_TYPE key)
{
  MapEntry *entry = map_get(map, key);

  if (entry)
    {
      map_remove_entry(map, entry);
      return true;
    }
  return false;
}



/* Navigation */
MAP_STATIC_API MapEntry *
map_first(Map *map)
{
  struct rb_node *node = rb_first(&map->root);
  return (node) ? container_of(node, MapEntry, node_inv) : NULL;
}




MAP_STATIC_API MapEntry *
map_last(Map *map)
{
  struct rb_node *node = rb_last(&map->root);
  return (node) ? container_of(node, MapEntry, node_inv) : NULL;
}


MAP_STATIC_API MapEntry *
map_next_entry(MapEntry *entry)
{
  struct rb_node *node = &entry->node_inv;
  node = rb_next(node);
  entry = (node) ? container_of(node, MapEntry, node_inv) : NULL;

  return entry;
}



MAP_STATIC_API MapEntry *
map_next(Map *map, MAP_KEY_TYPE key, bool strict)
{
  MapEntry *entry = map_locate(map, key);

  if (entry)
    {
      int result = MAP_KEY_CMP(key, entry->key);
      if (result > 0 || (result == 0 && strict))
	entry = map_next_entry(entry);
    }

  return entry;
}



MAP_STATIC_API MapEntry *
map_prev_entry(MapEntry *entry)
{
  struct rb_node *node = &entry->node_inv;
  node = rb_prev(node);
  entry = (node) ? container_of(node, MapEntry, node_inv) : NULL;

  return entry;
}



MAP_STATIC_API MapEntry *
map_prev(Map *map, MAP_KEY_TYPE key, bool strict)
{
  MapEntry *entry = map_locate(map, key);

  if (entry)
    {
      int result = MAP_KEY_CMP(key, entry->key);
      if (result < 0 || (result == 0 && strict))
	entry = map_prev_entry(entry);
    }

  return entry;
}



MAP_STATIC_API void
map_clear(Map *map)
{
#if 1

  struct rb_node *node = rb_first_postorder(&map->root);
  
  while(node)
    {
      MapEntry *entry = container_of(node, MapEntry, node_inv);
      node = rb_next_postorder(node);
#ifdef MAP_CLEAR_ENTRY
      MAP_CLEAR_ENTRY(map, entry);
#endif
      free(entry);
    }

#else  /* Before I saw there was a postorder facility in linux rbtree */

  struct rb_node *node = map->root.rb_node;
  struct rb_node *prev, *next;

  /* Postorder traversal (free a node after its 2 children have been freed) */

  while(node)
    {
      while((next = node->rb_left ? node->rb_left : node->rb_right) != NULL) // go down
	node = next;

      do		 /* delete and go up along the right branch */
	{
	  MapEntry *entry = container_of(node, MapEntry, node_inv);
	  prev = node;
	  node = rb_parent(node);
#ifdef MAP_CLEAR_ENTRY
	  MAP_CLEAR_ENTRY(map, entry);
#endif
	  free(entry);
	}
      while(node && (node->rb_right == prev || node->rb_right == NULL));

      if (node)
	node = node->rb_right;
    }

#endif

  map_init(map);
}

#ifdef MAP_SHOW_ENTRY
static void
map_show_tree_rec(Map *map, struct rb_node *node, char *buff, bool left_child)
{
  if (node == NULL)
    return;

  MapEntry *entry = container_of(node, MapEntry, node_inv);

  int n = (int) strlen(buff);

  sprintf(buff + n, (left_child && n > 0) ? "|  " : "   "); /* n > 0 to avoid a | on first column */
  map_show_tree_rec(map, node->rb_right, buff, false);

  buff[n] = '\0';
  printf("%s+- ", buff);
  MAP_SHOW_ENTRY(map, entry);
  printf("\n");
  
  sprintf(buff + n, (!left_child) ? "|  " : "   ");
  map_show_tree_rec(map, node->rb_left, buff, true);
}

MAP_STATIC_API void
map_show_tree(Map *map)
{
  static char buff[65536];
  *buff = '\0';
  map_show_tree_rec(map, map->root.rb_node, buff, true);
}
#endif

#ifdef MAP_STATIC
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#endif

#define _MAP_H
#endif
