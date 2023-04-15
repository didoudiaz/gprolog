// SPDX-License-Identifier: GPL-2.0-or-later OR MIT
/*
  An Efficient C Sorted Map Implementation Based on Red Black Trees.

  (C) 2023  Daniel Diaz <didou.diaz@gmail.com>

  file: map_rbtree.h
*/

/* 
 * Introduction
 * ------------
 *
 * A map is a data structure which holds key-value pairs. Keys are unique and
 * are maintained ordered. This implementation uses Red-Black Trees ensuring
 * log(n) time cost for the put, get, remove operations. 
 * Keys are ordered and it is possible to navigate in the map. In particular,
 * it is possible to visit the map in ascending or descending order of keys.
 *
 * Implementation
 * --------------
 *
 * This implementation is based on a the linux kernel rbtree tool (version 6.2):
 *    https://github.com/torvalds/linux/blob/master/tools/lib/rbtree.c
 *
 * This rbtree tool uses an invasive approach for efficiency: it only maintains
 * the tree (as linked nodes) and ensures it is well balanced (wrt RB Tree). 
 * The user code (here this map code) is responsible for defining a data type 
 * for the pair key-value which must also include a rbtree node structure, 
 * (see struct map_rbt_entry). 
 * The user has to allocates/frees these nodes. More info on the rbtree API:
 *    https://www.kernel.org/doc/Documentation/rbtree.txt
 *    https://docs.kernel.org/core-api/rbtree.html
 *
 * Simple Usage
 * ------------
 *
 * This section describes a simple usage when only one type of map is needed
 * in a Csource file. Basically, the user code defines some macros before
 * including the header file map_rbtree.h and then uses the intuitive API.
 *
 * Define some macros before the inclusion of the header file
 *
 *   MAP_KEY_TYPE:      type of the key
 *   MAP_KEY_CMP(x, y): compare 2 keys and return a value <,=,> 0 if x <,=,> y
 *   MAP_VALUE_TYPE:    type of  value (any C type, including a struct).
 *                      Can be omitted if there is no attached value (can be
 *                      usefull to store sorted sets).
 *
 *   Other optional macros can be defined before the inclusion (see below).
 *
 * Include the header file
 *
 *   #include "map_rbtree.h"
 *
 *   This header file, defines 2 types (structures):
 *
 *   struct map_rbt: type of a map, contains fields:
 *     size:        number of keys in the map
 *     counter_add: total number of insertions
 *     counter_del: total number of deletions
 *           -- the following fields are mainly for internal use --
 *     root:        root of the rbtree
 *     for_typeof:  a map_entry * field (used by map_foreach macros)
 *     put:         pointer to associated function map_put
 *     locate:      same for other functions
 *     get:
 *     contains:
 *     remove_entry:
 *     remove:
 *     first:
 *     last:
 *     next:
 *     next_entry:
 *     prev:
 *     prev_entry:
 *     clear:
 *     show_tree:   
 *
 *   struct map_entry: type of a pair key-value, contains fields:
 *     key:         the key
 *     value        the value (only if MAP_VALUE_TYPE is defined)
 *           -- the following field is mainly for internal use --
 *     node_inv:    the rbtree node
 *
 * Declaration of a map variable
 *
 *   struct map_rbt my_map;
 *
 * Initialization
 * 
 *   void map_init(struct map_rbt *map)
 *
 *   map:     the map.
 *
 *   This initializes map as a new (empty) map (basically sets all fields to 0).
 *
 * Insertion:
 *
 *   struct map_entry *map_put(struct map_rbt *map, MAP_KEY_TYPE key, bool *created)
 *
 *   map:     the map.
 *   key:     the key to insert.
 *   created: if != NULL will contain true if the key has been added and
 *            false if the key was already present in the map.
 *   return:  the entry associated to the key (created or already existing). 
 *            The value field can then be modified by the user code.
 *
 *   When a new key is added, the optional macro MAP_PUT_ENTRY(map, entry) 
 *   is expanded if defined (after the size and counter_add have been updated).
 *
 * Retrieval:
 *
 *   struct map_entry *map_get(struct map_rbt *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  the entry associated to the key or NULL if there is no mapping 
 *            for the key.
 *
 *   bool map_contains(struct map_rbt *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  true iff the key exists.
 *
 *   struct map_entry *map_locate(struct map_rbt *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to search for.
 *   return:  the entry associated to the closest key or NULL if the map is empty.
 *
 *   This function is used internally (e.g. by map_get, map_next, map_prev).
 *
 * Deletions:
 *
 *   bool map_remove(struct map_rbt *map, MAP_KEY_TYPE key)
 *
 *   map:     the map.
 *   key:     the key to remove.
 *   return:  true iff the key was found (and thus removed).
 *
 *   When a key is removed, the optional macro MAP_REMOVE_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 *   void map_remove_entry(struct map_rbt *map, struct map_entry *entry)
 *
 *   map:     the map.
 *   entry:   an entry (should be a valid existing entry).
 *
 *   When the entry is removed, the optional macro MAP_REMOVE_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 *   NB: This function is faster than map_remove() since there is no need to locate 
 *   the entry in the tree.
 *
 * Information:
 *
 *   KEY_TYPE map_entry_key(struct map_entry *entry)     a macro (same as entry->key)
 *   VALUE_TYPE map_entry_value(struct map_entry *entry) a macro (same as entry->value)
 *
 *   bool map_is_empty(struct map_rbt *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  true iff the map is empty.
 *
 *   int map_size(struct map_rbt *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number of key-value mappings in the map.
 *
 *   int map_counter_add(struct map_rbt *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number insertion operations.
 *
 *   int map_counter_del(struct map_rbt *map)   defined as a macro
 *
 *   map:     the map.
 *   return:  the number remove operations.
 *
 *   void map_show_tree(struct map_rbt *map)
 *
 *   map:     the map.
 *
 *   Print a tree representation of the map (mainly for debug purpose). Only
 *   available if the macro MAP_SHOW_ENTRY(map, entry) is defined for printing
 *   an entry on stdout.
 *
 * Clearing:
 *
 *   void map_clear(struct map_rbt *map)
 *
 *   map:     the map.
 *
 *   This frees all entries and reset counters (as after map_init()). When a 
 *   key is about to be cleared the optional macro MAP_CLEAR_ENTRY(map, entry) 
 *   is expanded if defined.
 *
 * Navigation:
 *
 *   struct map_entry *map_first(struct map_rbt *map)
 *
 *   map:     the map.
 *   return:  the entry associated with the first key (lowest) in the map
 *            (or NULL if the map is empty).
 *
 *   struct map_entry *map_last(struct map_rbt *map)
 *
 *   map:     the map.
 *   return:  the entry associated with the last key (highest) in the map
 *            (or NULL if the map is empty).
 *
 *   struct map_entry *map_next(struct map_rbt *map, MAP_KEY_TYPE key, bool strict)
 *
 *   map:     the map.
 *   key:     the given key.
 *   strict:  if true look for the next key strictly greater than 
 *            (if false for a key greater then or equal to).
 *   return:  the entry associated with the least key greater than 
 *            (greater than or equal to if !strict) the given key 
 *            (or NULL if there is no such key).
 *
 *   struct map_entry *map_prev(struct map_rbt *map, MAP_KEY_TYPE key, bool strict)
 *
 *   map:     the map.
 *   key:     the given key.
 *   strict:  if true look for the next key strictly less than 
 *            (if false for a key less than or equal to).
 *   return:  the entry associated with the greatest key less than 
 *            (less than or equal to if !strict) the given key 
 *            (or NULL if there is no such key).
 *
 *   struct map_entry *map_next_entry(struct map_rbt *map, struct map_entry *entry)
 *
 *   map:     the map.
 *   entry:   an entry (should be a valid existing entry).
 *   return:  the entry associated with the least key greater than the given key
 *            (or NULL if there is no such key).
 *
 *   NB: This function is faster than map_next() since there is no need to locate 
 *   the entry in the tree.
 *
 *   struct map_entry *map_prev_entry(struct map_rbt *map, struct map_entry *entry)
 *
 *   map:     the map.
 *   entry:   an entry (should be a valid existing entry).
 *   return:  the entry associated with the greatest key less than the given key
 *            (or NULL if there is no such key).
 *
 *   NB: This function is faster than map_prev() since there is no need to locate 
 *   the entry in the tree.
 *
 *   The following macros (based on above functions) provide simple foreach loops
 *   (expanded as a for-loop defining the entry variable in the scope of the for).
 *
 *   map_foreach(map, entry):    from least to greatest. Basically expands to:
 *      for(map_entry *entry = map_first(map); 
 *          entry != NULL; 
 *          entry = map_next_entry(map, entry))
 *
 *   map_foreach_rev(map, entry) from greatest to least. Basically expands to:
 *      for(map_entry *entry = map_last(map); 
 *          entry != NULL; 
 *          entry = map_prev_entry(map, entry))
 *
 * Advanced Usage
 * --------------
 *
 * This section describes how to use multiple map types in the same source file. 
 * This will be done by including the header file several times (with different 
 * macro definitions each time). The API functions described above are defined
 * (not just declared) in the header file (the names of these functions will 
 * appear as symbols in the object file). To handle multiple inclusions, the
 * function names must be different for each inclusion. The same goes for the
 * type names (map_rbt and map_entry). The actual name of a function (the one 
 * that appears as a symbol in the object file) is created from a prefix 
 * controlled by the MAP_NAME macro:
 * 
 *   MAP_NAME: a prefix replacing the default prefix of each API function name
 *             types struct map_rbt and struct map_entry.
 *             MAP_NAME is optional but becomes mandatory when working with
 *             multiple maps.
 *
 * For instance, with MAP_NAME defined as map_str, the real names are:
 * struct map_str_rbt, struct map_str_entry, map_str_init(), map_str_put(), ...
 *
 * Some macros are designed to simplify the usage: whatever MAP_NAME you can
 * simply use the above API names (prefixed by map_). NB: if you want the best
 * efficiency use native names (e.g. map_str_put() is faster than map_put()).
 * Indeed, the macros use pointers to functions (stored in the map_rbt) to call
 * the adequate function (in most cases this the overhead is insignificant).
 *
 * The scope of the functions can be restricted defining a macro:
 *
 *   MAP_STATIC: each API function is defined as static.
 *
 * If several map types are needed in a same source, it is possible to include
 * several times map_rbtree.h with adequate macros (MAP_KEY_TYPE,...). 
 * To avoid name clashs, at each re-include it is necessary to define the
 * MAP_NAME macro. To avoid accidental multiple inclusions of the header also
 * define the following macro:
 *
 *   MAP_RE_INCLUDE: must be defined before each new re-inclusion of the header 
 *
 * Using multiple maps sequentially
 *
 * In the presence of N includes, at least N-1 should define MAP_NAME. Each
 * chunk can use the simple API names (or with the explicit prefix MAP_NAME).
 *
 *   #define MAP_KEY_TYPE   char *
 *   #define MAP_VALUE_TYPE int
 *   #define MAP_NAME       map_str
 *   #include "map_rbtree.h"
 *   ... code using the first map ... (can use simple API names map_...)
 *
 *   #undef  MAP_KEY_TYPE
 *   #undef  MAP_VALUE_TYPE
 *   #undef  MAP_NAME
 *   #define MAP_KEY_TYPE   long
 *   #define MAP_VALUE_TYPE struct my_info
 *   #define MAP_NAME       map_lng
 *   #define MAP_RE_INCLUDE
 *   #include "map_rbtree.h"
 *   ... code using the second map ... (can use simple API names map_...)
 *
 * Using multiple maps simultaneously
 *
 * It is possible to mix codes using several maps. For this, include the header
 * several times (each with its adequate macro definitions). To differenciate 
 * the function to call, you can use the explicit names (prefixed with MAP_NAME).
 * But the simple API works most of the time. There are some exceptions where the
 * explicit name is mandatory (examples with MAP_NAME = map_str) :
 *
 *   To declare a map variable: struct map_str_rbt map_s;
 *   To initialize a map:       map_str_init(&map_s)
 *   To declare an entry:       struct map_str_entry *e = map_get(&map_s, "foo");
 * 
 * For the other cases generic API names can be used (thanks to pointers to 
 * functions stored in the map). Simple example with 2 maps (only keys):
 *
 *   #define MAP_KEY_TYPE   char *
 *   #define MAP_VALUE_TYPE int
 *   #define MAP_NAME       map_str
 *   #include "map_rbtree.h"
 *
 *   #undef  MAP_KEY_TYPE
 *   #undef  MAP_VALUE_TYPE
 *   #undef  MAP_NAME
 *   #define MAP_KEY_TYPE   long
 *   #define MAP_VALUE_TYPE struct my_info
 *   #define MAP_NAME       map_lng
 *   #define MAP_RE_INCLUDE
 *   #include "map_rbtree.h"
 *
 *   map_str_rbt map_s;
 *   map_lng_rbt map_l;
 *   map_str_init(&map_s);
 *   map_lng_init(&map_l);
 *   map_put(&map_s, "foo", NULL)->value = 12;
 *   map_lng_entry *entry = map_put(&map_l, 1234, NULL);
 */


#if !defined(_MAP_H) || defined(MAP_RE_INCLUDE)

#ifndef _MAP_H	       /* first include (things to be defined only once) */

#include <stdlib.h>
#include "rbtree.h"

#define CAT21(a, b)    a ## _ ## b
#define CAT2(a, b)     CAT21(a, b)

#else  /* _MAP_H (re-include) */

#undef MAP_STATIC_API
#undef MAP_MK_NAME

#endif	/* !_MAP_H */


#ifdef MAP_NAME
#   define MAP_MK_NAME(name)  CAT2(MAP_NAME, name)
#else
#   define MAP_MK_NAME(name)  CAT2(map_, name)
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

/* add prefix to API function/type names (not macros else redefine warnings) */

#define map_rbt            MAP_MK_NAME(rbt)
#define map_entry          MAP_MK_NAME(entry)

#define map_init           MAP_MK_NAME(init)

#define map_put(map, key, created)   ((map)->put(map, key, created))
#define map_locate(map, key)         ((map)->locate(map, key))
#define map_get(map, key)            ((map)->get(map, key))
#define map_contains(map, key)       ((map)->contains(map, key))
#define map_remove_entry(map, entry) ((map)->remove_entry(map, entry))
#define map_remove(map, key)         ((map)->remove(map, key))
#define map_first(map)               ((map)->first(map))
#define map_last(map)                ((map)->last(map))
#define map_next(map, key, strict)   ((map)->next(map, key, strict))
#define map_next_entry(map, entry)   ((map)->next_entry(map, entry))
#define map_prev(map, key, strict)   ((map)->prev(map, key, strict))
#define map_prev_entry(map, entry)   ((map)->prev_entry(map, entry))
#define map_clear(map)               ((map)->clear(map))
#define map_show_tree(map)           ((map)->show_tree(map))

#define map_foreach(map, entry) \
  for(typeof((map)->for_typeof) entry = map_first(map); entry != NULL; entry = map_next_entry(map, entry))

#define map_foreach_rev(map, entry) \
  for(typeof((map)->for_typeof) entry = map_last(map); entry != NULL; entry = map_prev_entry(map, entry))


struct map_entry
{
  MAP_KEY_TYPE key;
#ifdef MAP_VALUE_TYPE
  MAP_VALUE_TYPE value;
#endif
  struct rb_node node_inv;	/* the invasive rbtree node */
};


struct map_rbt
{
  int size;			/* number of entries in the map */
  int counter_add; 		/* number of insertions */
  int counter_del; 		/* number of deletions */
  
  struct rb_root root;		/* root of the RB tree */

  struct map_entry *for_typeof;	/* dummy field, only to get the correct type */
				/* pointer to functions */
  struct map_entry *(*put)         (struct map_rbt *map, MAP_KEY_TYPE key, bool *created);
  struct map_entry *(*locate)      (struct map_rbt *map, MAP_KEY_TYPE key);
  struct map_entry *(*get)         (struct map_rbt *map, MAP_KEY_TYPE key);
  bool              (*contains)    (struct map_rbt *map, MAP_KEY_TYPE key);
  void              (*remove_entry)(struct map_rbt *map, struct map_entry *entry);
  bool              (*remove)      (struct map_rbt *map, MAP_KEY_TYPE key);
  struct map_entry *(*first)       (struct map_rbt *map);
  struct map_entry *(*last)        (struct map_rbt *map);
  struct map_entry *(*next)        (struct map_rbt *map, MAP_KEY_TYPE key, bool strict);
  struct map_entry *(*next_entry)  (struct map_rbt *map, struct map_entry *entry);
  struct map_entry *(*prev)        (struct map_rbt *map, MAP_KEY_TYPE key, bool strict);
  struct map_entry *(*prev_entry)  (struct map_rbt *map, struct map_entry *entry);
  void              (*clear)       (struct map_rbt *map);
  void              (*show_tree)   (struct map_rbt *map);
};

/* API: macros */

#define map_size(map)          ((map)->size)
#define map_counter_add(map)   ((map)->counter_add)
#define map_counter_del(map)   ((map)->counter_del)
#define map_is_empty(map)      ((map)->size == 0)

/* API: functions */

MAP_STATIC_API struct map_entry *
MAP_MK_NAME(put)(struct map_rbt *map, MAP_KEY_TYPE key, bool *created)
{
  struct rb_node **node = &(map->root.rb_node), *parent = NULL;
  struct map_entry *entry;

  /* Figure out where to put new node */
  while (*node)
    {
      entry = container_of(*node, struct map_entry, node_inv);
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
  entry = (struct map_entry *) calloc(1, sizeof(struct map_entry));
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


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(locate)(struct map_rbt *map, MAP_KEY_TYPE key)
{
  struct rb_node *node = map->root.rb_node;
  struct map_entry *entry = NULL;

  while (node)
    {
      entry = container_of(node, struct map_entry, node_inv);
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


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(get)(struct map_rbt *map, MAP_KEY_TYPE key)
{
  struct map_entry *entry = map_locate(map, key);
  return (entry && MAP_KEY_CMP(key, entry->key) == 0) ? entry : NULL;
}


MAP_STATIC_API bool
MAP_MK_NAME(contains)(struct map_rbt *map, MAP_KEY_TYPE key)
{
  return (MAP_MK_NAME(get)(map, key) != NULL);
}


MAP_STATIC_API void
MAP_MK_NAME(remove_entry)(struct map_rbt *map, struct map_entry *entry)
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
MAP_MK_NAME(remove)(struct map_rbt *map, MAP_KEY_TYPE key)
{
  struct map_entry *entry = MAP_MK_NAME(get)(map, key);

  if (entry)
    {
      MAP_MK_NAME(remove_entry)(map, entry);
      return true;
    }
  return false;
}


/* Navigation */
MAP_STATIC_API struct map_entry *
MAP_MK_NAME(first)(struct map_rbt *map)
{
  struct rb_node *node = rb_first(&map->root);
  return (node) ? container_of(node, struct map_entry, node_inv) : NULL;
}


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(last)(struct map_rbt *map)
{
  struct rb_node *node = rb_last(&map->root);
  return (node) ? container_of(node, struct map_entry, node_inv) : NULL;
}


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(next_entry)(struct map_rbt *map, struct map_entry *entry)
{
  struct rb_node *node = &entry->node_inv;
  node = rb_next(node);
  entry = (node) ? container_of(node, struct map_entry, node_inv) : NULL;

  return entry;
}


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(next)(struct map_rbt *map, MAP_KEY_TYPE key, bool strict)
{
  struct map_entry *entry = MAP_MK_NAME(locate)(map, key);

  if (entry)
    {
      int result = MAP_KEY_CMP(key, entry->key);
      if (result > 0 || (result == 0 && strict))
	entry = MAP_MK_NAME(next_entry)(map, entry);
    }

  return entry;
}


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(prev_entry)(struct map_rbt *map, struct map_entry *entry)
{
  struct rb_node *node = &entry->node_inv;
  node = rb_prev(node);
  entry = (node) ? container_of(node, struct map_entry, node_inv) : NULL;

  return entry;
}


MAP_STATIC_API struct map_entry *
MAP_MK_NAME(prev)(struct map_rbt *map, MAP_KEY_TYPE key, bool strict)
{
  struct map_entry *entry = MAP_MK_NAME(locate)(map, key);

  if (entry)
    {
      int result = MAP_KEY_CMP(key, entry->key);
      if (result < 0 || (result == 0 && strict))
	entry =  MAP_MK_NAME(prev_entry)(map, entry);
    }

  return entry;
}


MAP_STATIC_API void
MAP_MK_NAME(clear)(struct map_rbt *map)
{
#if 1

  struct rb_node *node = rb_first_postorder(&map->root);
  
  while(node)
    {
      struct map_entry *entry = container_of(node, struct map_entry, node_inv);
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
	  struct map_entry *entry = container_of(node, struct map_entry, node_inv);
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

  map->size = 0;
  map->counter_add = 0;
  map->counter_del = 0;
}


#ifdef MAP_SHOW_ENTRY
static void
MAP_MK_NAME(show_tree_rec)(struct map_rbt *map, struct rb_node *node, char *buff, bool left_child)
{
  if (node == NULL)
    return;

  struct map_entry *entry = container_of(node, struct map_entry, node_inv);

  int n = (int) strlen(buff);

  sprintf(buff + n, (left_child && n > 0) ? "|  " : "   "); /* n > 0 to avoid a | on first column */
  MAP_MK_NAME(show_tree_rec)(map, node->rb_right, buff, false);

  buff[n] = '\0';
  printf("%s+- ", buff);
  MAP_SHOW_ENTRY(map, entry);
  printf("\n");
  
  sprintf(buff + n, (!left_child) ? "|  " : "   ");
  MAP_MK_NAME(show_tree_rec)(map, node->rb_left, buff, true);
}

MAP_STATIC_API void
MAP_MK_NAME(show_tree)(struct map_rbt *map)
{
  static char buff[65536];
  *buff = '\0';
  MAP_MK_NAME(show_tree_rec)(map, map->root.rb_node, buff, true);
}
#endif


MAP_STATIC_API void
MAP_MK_NAME(init)(struct map_rbt *map)
{
  map->size = 0;
  map->counter_add = 0;
  map->counter_del = 0;
  
  map->root = RB_ROOT;

  map->for_typeof = NULL;

  map->put = MAP_MK_NAME(put);
  map->locate = MAP_MK_NAME(locate);
  map->get = MAP_MK_NAME(get);
  map->contains = MAP_MK_NAME(contains);
  map->remove_entry = MAP_MK_NAME(remove_entry);
  map->remove = MAP_MK_NAME(remove);
  map->first = MAP_MK_NAME(first);
  map->last = MAP_MK_NAME(last);
  map->next = MAP_MK_NAME(next);
  map->next_entry = MAP_MK_NAME(next_entry);
  map->prev = MAP_MK_NAME(prev);
  map->prev_entry = MAP_MK_NAME(prev_entry);
  map->clear = MAP_MK_NAME(clear);
#ifdef MAP_SHOW_ENTRY
  map->show_tree = MAP_MK_NAME(show_tree);
#else
  map->show_tree = NULL;
#endif
}




#ifdef MAP_STATIC
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#endif

#define _MAP_H
#endif
