#ifndef __virtual_machine_h__
#define __virtual_machine_h__

#include <stdbool.h>

typedef struct {
  char *content;
  int size;
  int pos;
  int used;
} file;

typedef struct {
  char A;
  bool M;
  struct {
    int pos;
    char list[5];
  } P;
  file *S;
  file *W;
} machine;

machine *new_machine(void);
void insert(machine *m);
void delete(machine *m);
void free_machine(machine *m);
void weave(machine *m);
void jump(machine *m, bool forward);
void swap(machine *m);

#endif // __virtual_machine_h__
