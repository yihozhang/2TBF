#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG
#define MAX_LENGTH 100000

typedef struct {
  short val;
  unsigned short flag;
} ArrCell;

char* instr;
int instr_len, *jump_table;
short stk[MAX_LENGTH];
ArrCell arr[MAX_LENGTH];
int stk_p, arr_p;
char op;
short v1, v2;
#ifdef DEBUG
FILE* logfile;
#endif
void read_instr(char filename[]) {
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    printf("file %s not found", filename);
    exit(1);
  }
  fseek(file, 0, SEEK_END);
  instr_len = ftell(file);
  fseek(file, 0, SEEK_SET);
  instr = (char*)malloc(instr_len);
  if (instr) {
    fread(instr, 1, instr_len, file);
    fclose(file);
  } else {
    exit(1);
  }
}

void build_jump_table() {
  jump_table = (int*)malloc(instr_len * sizeof(int));
  memset(jump_table, 0, instr_len * sizeof(int));
  int* stack = (int*)malloc(instr_len * sizeof(int));
  int p = 0;
  for (int i = 0; i < instr_len; i++) {
    if (instr[i] == '[') {
      stack[p++] = i;
    } else if (instr[i] == ']') {
      jump_table[jump_table[i] = stack[--p]] = i;
    }
  }
  free(stack);
}

#define has_next_int() (instr_p + 1 < instr_len && isdigit(instr[instr_p + 1]))

int next_int_impl(int* instr_p) {
  int x = 0;
  while ((*instr_p) + 1 < instr_len && isdigit(instr[(*instr_p) + 1])) {
    x = x * 10 + instr[++(*instr_p)] - '0';
  }
  return x;
}
#define next_int() next_int_impl(&instr_p)

#define next_int_or(x) (has_next_int() ? next_int() : (x))
#define pop() stk[--stk_p]
#define top() stk[stk_p - 1]
#define push(x) (stk[stk_p++] = (x))
#define set_val(x) (arr[arr_p].val = (x))
#define get_val() (arr[arr_p].val)
char buffered_input;
char get_char() {
  if (buffered_input) {
    char c = buffered_input;
    buffered_input = 0;
    return c;
  } else {
    return getchar();
  }
}
#define put_char(x) putchar(x)

int get_int() {
  int x = 0;
  while (isdigit(buffered_input = getchar())) {
    x = x * 10 + buffered_input - '0';
  }
  return x;
}
#define put_int(x) printf("%d", (x))
#ifdef DEBUG
void print_config(int instr_p) {
  fprintf(logfile, "instr: %c, instr_p: %d\n", op, instr_p);
  fprintf(logfile, "v1: %hd, v2: %hd\n", v1, v2);
  fprintf(logfile, "arr_p: %d, stk_p: %d\n", arr_p, stk_p);
  fprintf(logfile, "Arr: ");
  for (int i = 0; i <= arr_p; i++) {
    fprintf(logfile, "%hd ", arr[i].val);
  }
  fprintf(logfile, "\nStk: ");
  for (int i = 0; i < stk_p; i++) {
    fprintf(logfile, "%hd ", stk[i]);
  }
  fprintf(logfile, "\n\n");
}
#endif
void run() {
  int instr_p = 0;
  while (instr_p < instr_len) {
    op = instr[instr_p];
    if (op == ';') {
      while (instr_p < instr_len && op != '\n' && op != 0) {
        op = instr[++instr_p];
      }
      if (instr_p == instr_len) break;
    }
    if (op == '+') {
      v1 = pop();
      v2 = next_int_or(pop());
      push(v1 + v2);
    } else if (op == '-') {
      v1 = next_int_or(pop());
      v2 = pop();
      push(v2 - v1);
    } else if (op == 'u') {
      v1 = next_int_or(get_val());
      push(v1);
    } else if (op == 'o') {
      v1 = next_int_or(1);
      if (v1)
        set_val(pop());
      else
        pop();
    } else if (op == '>') {
      v1 = next_int_or(top());
      arr_p += v1;
    } else if (op == '<') {
      v1 = next_int_or(top());
      arr_p -= v1;
    } else if (op == 'r') {
      push(get_char());
    } else if (op == 'w') {
      put_char(top());
    } else if (op == 'R') {
      push(get_int());
    } else if (op == 'W') {
      put_int(top());
    } else if (op == '[') {
      if (!top()) instr_p = jump_table[instr_p];
    } else if (op == ']') {
      if (top()) instr_p = jump_table[instr_p];
    } else if (op == '?') {
      char syb = instr[++instr_p];
      syb = isdigit(syb) ? syb - '0' : syb - 'a' + 10;
      push((arr[arr_p].flag & (1 << syb)) && 1);
    } else if (op == '!') {
      char syb = instr[++instr_p];
      syb = isdigit(syb) ? syb - '0' : syb - 'a' + 10;
      arr[arr_p].flag ^= 1 << syb;
    } else if (op == 's') {
      run();
    } else if (!isspace(op)) {
      printf("WARNING: unrecognized char %c", op);
    }
#ifdef DEBUG
    if (!isspace(op)) print_config(instr_p);
#endif
    ++instr_p;
  }
}

int main(int argc, char* argv[]) {
  if (argc == 0) {
    printf("not enough parameters!");
    return 0;
  }
  read_instr(argv[1]);
  build_jump_table();
#ifdef DEBUG
  logfile = fopen("interpreter.log", "w");
  fprintf(logfile, "jump table\n");
  for (int i = 0; i < instr_len; i++) {
    if (jump_table[i]) {
      fprintf(logfile, "from: %d, to: %d\n", i, jump_table[i]);
    }
  }
#endif
  run();
}
