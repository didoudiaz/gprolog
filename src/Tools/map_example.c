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
