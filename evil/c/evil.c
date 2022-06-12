#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "virtual_machine.h"

struct file {
  size_t size;
  char *content;
};

static void usage(void) {
  fprintf(stderr, "usage: evil [-hit] file1 [file2..]\n\
  -i: interpret\n\
  -t: transpile (to C)\n");
  exit(EXIT_SUCCESS);
}

static char *remove_ext(char *myStr, char extSep, char pathSep) {
  char *retStr, *lastExt, *lastPath;

  if (myStr == NULL)
    return NULL;
  if ((retStr = malloc(strlen(myStr) + 1)) == NULL)
    return NULL;

  strcpy(retStr, myStr);
  lastExt = strrchr(retStr, extSep);
  lastPath = (pathSep == 0) ? NULL : strrchr(retStr, pathSep);

  if (lastExt != NULL) {
    if (lastPath != NULL) {
      if (lastPath < lastExt) {
        *lastExt = '\0';
      }
    } else {
      *lastExt = '\0';
    }
  }

  return retStr;
}

static void read_file(const char *const filename, file *S) {
  FILE *f = fopen(filename, "r");
  if (f == NULL) {
    fprintf(stderr, "Couldn't open file");
    exit(EXIT_FAILURE);
  }

  assert(!fseek(f, 0, SEEK_END));
  long file_size = ftell(f);
  rewind(f);
  size_t code_size = sizeof(char) * file_size;
  S->content = malloc(code_size);
  fread(S->content, 1, file_size, f);
  S->size = code_size;
  S->used = code_size;
  S->pos = 0;
  assert(!fclose(f));
}

static void interpret(const char *filename) {
  machine* m = new_machine();
  m->S = malloc(sizeof(file));
  read_file(filename, m->S);
  char current, temp;
  while (m->S->pos >= 0 && m->S->pos < m->S->size) {
    current = m->S->content[m->S->pos];
    switch (current) {
      case 'a':
        m->A++;
        break;
      case 'b':
        jump(m, false);
        break;
      case 'c':
        insert(m);
        break;
      case 'd':
        delete(m);
        break;
      case 'e':
        weave(m);
        break;
      case 'f':
        jump(m, true);
        break;
      case 'g':
        m->A = m->P.list[m->P.pos];
        break;
      case 'h':
        m->P.pos = (m->P.pos + 1) % 5;
        break;
      case 'i':
        m->W->pos = (m->W->pos + 1) % m->W->used;
        break;
      case 'j':
        break;
      case 'k':
        m->P.list[m->P.pos] = m->A;
        break;
      case 'l':
        temp = m->W->content[m->W->pos];
        m->W->content[m->W->pos] = m->A;
        m->A = temp;
        break;
      case 'm':
        break;
      case 'n':
        m->P.pos -= 1;
        if (m->P.pos<0)
          m->P.pos = 4;
        break;
      case 'o':
        m->W->pos -= 1;
        if (m->W->pos < 0)
          m->W->pos = m->W->used - 1;
      case 'p':
        m->A = m->W->content[m->W->pos];
      case 'q':
        swap(m);
        break;
      case 'r':
        m->A = getchar();
      case 's':
        if (m->A == 0)
          m->S->pos++;
        break;
      case 't':
        if (m->A != 0)
          m->S->pos++;
        break;
      case 'u':
        m->A--;
        break;
      case 'v':
        temp = m->P.list[m->P.pos];
        m->P.list[m->P.pos] = m->A;
        m->A = temp;
        break;
      case 'w':
        putchar(m->A);
        break;
      case 'x':
        m->M = !m->M;
        break;
      case 'y':
        m->W->content[m->W->pos] = m->A;
        break;
      case 'z':
        m->A = 0;
    }
    m->S->pos++;
  }
  free_machine(m);
}

int main(int argc, char *argv[]) {
  bool interpreter = false, transpiler = false;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-i"))
      interpreter = true;
    else if (!strcmp(argv[i], "-t"))
      transpiler = true;
    else if (!strcmp(argv[i], "-h"))
      usage();
    else {
      char *file = argv[i];
      if (interpreter)
        interpret(file);
    }
  }
}
