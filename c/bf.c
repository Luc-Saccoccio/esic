#include<libgen.h>
#include<stdbool.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#include<unistd.h>

#define OUT_START "extern int putchar(int);\nextern char getchar();\n\nchar array[30000] = {0}; char *ptr = array;\nint main (int argc, char *argv[]) {"

static void usage(void) {
  fputs("usage: bf [-hit] file1 [file2..]\n\
  -i: interpret\n\
  -t: transpile (to C)\n", stderr);
  exit(EXIT_SUCCESS);
}

inline int mod(int x, int y) {
  return ((x%y) + y)%y;
}

// Thanks https://stackoverflow.com/questions/2736753/how-to-remove-extension-from-file-name
static char *remove_ext (char* myStr, char extSep, char pathSep) {
  char *retStr, *lastExt, *lastPath;

  if (myStr == NULL) return NULL;
  if ((retStr = malloc (strlen (myStr) + 1)) == NULL) return NULL;

  strcpy (retStr, myStr);
  lastExt = strrchr (retStr, extSep);
  lastPath = (pathSep == 0) ? NULL : strrchr (retStr, pathSep);

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

static char* read_file(const char* const filename) {
	if (filename == NULL)
    return NULL;
  FILE *f = fopen(filename, "r");
	if (f == NULL)
    return NULL;

	assert(!fseek(f, 0, SEEK_END));
	long file_size = ftell(f);
	rewind(f);
	size_t code_size = sizeof(char) * file_size;
	char *code = malloc(code_size);
	if (code == NULL)
		return NULL;
	fread(code, 1, file_size, f);
	assert(!fclose(f));
	return code;
}

static const char* equiv(char c) {
	switch (c) {
    case '>': return "++ptr;";
		case '<': return "--ptr;";
		case '+': return "++*ptr;";
		case '-': return "--*ptr;";
		case '.': return "putchar(*ptr);";
		case ',': return "*ptr = getchar();";
		case '[': return "while (*ptr) {";
		case ']': return "}";
		default:  return "";
	}
}

static void translate(FILE* source, FILE* output) {
	char* a = malloc(17 * sizeof(char));
	char c;
	while((c = fgetc(source)) != EOF) {
		strcpy(a, equiv(c));
		if (strcmp(a, "")) {
			fputs("\t", output);
			fputs(a, output);
			fputs("\n", output);
		}
	}
	free(a);
}

static void interpret(const char* const input) {
	char array[30000] = {0}; char *ptr = array;
	char c;
	for (int i = 0; (c = input[i]) != '\0'; i++) {
		switch (c) {
			case '>': ++ptr; break;
			case '<': --ptr; break;
			case '+': ++(*ptr); break;
			case '-': --(*ptr); break;
			case '.': putchar(*ptr); break;
			case ',': *ptr = getchar(); break;
			case '[':
				if (!*ptr) {
					int loop = 1;
					while (loop > 0) {
						c = input[++i];
						if (c == ']')
							--loop;
						else if (c == '[')
							++loop;
					}
				}
				break;
			case ']':
				if (*ptr) {
					int loop = 1;
					while (loop > 0) {
						c = input[--i];
						if (c == '[')
							--loop;
						else if (c == ']')
							++loop;
					}
				}
				break;
		}
	}
}

static void transpile(char* input, FILE* output) {
	FILE *source = fopen(input, "r");
	if (source == NULL) {
		printf("File %s is not available.\n", input);
		exit(EXIT_FAILURE);
	}
	fputs(OUT_START, output);
	translate(source, output);
	fputs("}", output);
	fclose(source);
	fclose(output);
}

static void compile(char* input) {
	char* output_file = remove_ext(basename(input), '.', '/');
	char* template = malloc(1025 * sizeof(char));

	strcpy(template, output_file);
	strcat(template, "XXXXXX.c");

	int output_fd = mkstemps(template, 2);

	if (output_fd == -1) {
		printf("Error while creating temporary file");
		exit(EXIT_FAILURE);
	}

	FILE* output = fdopen(output_fd, "a");
	transpile(input, output);
	close(output_fd);

	char *command = malloc(sizeof(char) * 2048 + 6);
	sprintf(command, "tcc %s -o %s", template, output_file);

	system(command);
	unlink(template);
	free(output_file);
	free(command);
	free(template);
}

int main(int argc, char *argv[]) {
	bool interpreter = false, transpiler = false;

	for (int i = 1; i<argc; i++) {
		if (!strcmp(argv[i], "-i"))
			interpreter = true;
		else if (!strcmp(argv[i], "-t"))
			transpiler = true;
		else if (!strcmp(argv[i], "-h"))
			usage();
		else {
			char* file = argv[i];
			if (interpreter) {
				interpreter = false;
				char *file_content = read_file(file);
				if (file_content == NULL)
					printf("Couldn't open file");
				else interpret(file_content);
				free(file_content);
        printf("\n%s - Interpreted\n", file);
			} else if (transpiler) {
				transpiler = false;
				char* output_file = strcat(remove_ext(file, '.', '/'), ".c");
				if (output_file == NULL) {
					printf("Error while removing ext\n");
					return 1;
				}
				FILE *target = fopen(output_file, "a");
				transpile(file, target);
				free(output_file);
        printf("%s - Transpiled\n", file);
			} else {
				compile(argv[i]);
        printf("%s - Compiled\n", file);
			}
		}
	}
	return 0;
}
