#include "virtual_machine.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static const char weaveMap[8] = {4, 1, 16, 2, 64, 8, 128, 32};

machine *new_machine(void) {
  file *W = malloc(sizeof(file));
  machine *m = malloc(sizeof(machine));
  m->A = 0;
  m->M = true;
  // Pental
  m->P.pos = 0;
  for (int i = 0; i<5; i++)
    m->P.list[i] = 0;
  // Wheel
  W->pos = 0;
  W->size = 100;
  W->used = 1;
  W->content = calloc(W->size, sizeof(char));
  m->W = W;
  // Source
  m->S = NULL;
  return m;
}

void insert(machine *m) {
  if (m->W->used >= m->W->size) {
    m->W->size += 100;
    m->W->content = realloc(m->W->content, m->W->size * sizeof(char));
  }
  for (int i = m->W->used; i > m->W->pos; i--)
    m->W->content[i] = m->W->content[i-1];
  m->W->content[m->W->pos] = 0;
}

void delete(machine *m) {
  for (int i = m->W->pos; i<m->W->used-1; i++)
    m->W->content[i] = m->W->content[i+1];
  m->W->used--;
  if (m->W->used <= 0) {
    m->W->used = 1;
    m->W->pos = 0;
    m->W->content[m->W->pos] = 0;
  }
}

void free_machine(machine *m) {
  free(m->W->content);
  m->W->content = NULL;
  free(m->W);
  free(m->S->content);
  m->S->content = NULL;
  free(m->S);
  free(m);
}

void weave(machine *m) {
  int mask = 1;
  int answer = 0;
  for (int i = 0; i<8; i++) {
    if ((m->A & mask) != 0)
      answer = answer|weaveMap[i];
    mask = mask << 1;
  }
  m->A = answer;
}

void jump(machine *m, bool forward) {
  char c = m->M ? 'm' : 'j';
  if (forward)
  while (m->S->pos < m->S->size && m->S->content[m->S->pos] != c)
      m->S->pos++;
  else
    while (m->S->pos >= 0 && m->S->content[m->S->pos] != c)
      m->S->pos++;
}

void swap(machine *m) {
  file *temp = m->S;
  m->S = m->W;
  m->W = temp;
}
