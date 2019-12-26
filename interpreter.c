#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#define MAX_LENGTH 100000

typedef struct {
    short val;
    unsigned short flag;
} ArrCell;

char *instr;
int instr_len, *jump_table;
short stk[MAX_LENGTH];
ArrCell arr[MAX_LENGTH];
int stk_p, arr_p, instr_p;
char op; short v1, v2;

void read_instr(char filename[]) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("file %s not found", filename);
        exit(1);
    }
    fseek(file, 0, SEEK_END);
    instr_len = ftell(file);
    fseek(file, 0, SEEK_SET);
    instr = (char*) malloc(instr_len);
    if (instr) {
        fread(instr, 1, instr_len, file);
        fclose(file);
    } else {
        exit(1);
    }
}

void build_jump_table() {
    jump_table = (int*) malloc(instr_len * sizeof(int));
    int *stack = (int*) malloc(instr_len * sizeof(int));
    int p = 0;
    for (int i = 0; i < instr_len; i++) {
        if (instr[i] == '(') {
            stack[p++] = i;
        } else if (instr[i] == '(') {
            jump_table[stack[--p]] = i;
        }
    }
    free(stack);
}

bool has_next_int() {
    return instr_p + 1 < instr_len && isdigit(instr[instr_p + 1]);
}

int next_int() {
    int x = 0;
    while (instr_p + 1 < instr_len && isdigit(instr[instr_p + 1])) {
        x = x * 10 + instr[++instr_p] - '0';
    }
    return x;
}

#define NEXT_INT_OR(x) (has_next_int() ? next_int() : (x))
#define POP() stk[--stk_p]
#define PUSH(x) (stk[stk_p++] = (x))
#define SET_VAL(x) (arr[arr_p].val = (x))
#define GET_VAL() (arr[arr_p].val)
char buffered_input;
char GET_CHAR() {
    if (buffered_input) {
        char c = buffered_input;
        buffered_input = 0;
        return c;
    } else {
        return getchar();
    }
}
#define PUT_CHAR(x) putchar(x)

int GET_INT() {
    int x = 0;
    while (isdigit(buffered_input=getchar())) {
        x = x * 10 + buffered_input - '0';
    }
    return x;
}
#define PUT_INT(x) printf("%d", (x))

void print_config() {
    printf("instr: %c, instr_p: %d\n", instr[instr_p], instr_p);
    printf("v1: %hd, v2: %hd\n", v1, v2);
    printf("arr_p: %d, stk_p: %d\n", arr_p, stk_p);
    printf("Arr: ");
    for (int i = 0; i < arr_p; i++) {
        printf("%hd ", arr[i].val);
    }
    printf("\nStk: ");
    for (int i = 0; i < stk_p; i++) {
        printf("%hd ", stk[i]);
    }
    puts("\n");
}
// #define DEBUG
int main(int argc, char* argv[]) {
    if (argc == 0) {
        printf("not enough parameters!");
        return 0;
    }
    read_instr(argv[1]);
    build_jump_table();
    while (instr_p < instr_len) {
        op = instr[instr_p];
        if (op == ';') {
            while (instr_p < instr_len && op != '\n' && op != 0) {
                op = instr[++instr_p];
            }
            if (instr_p == instr_len) break;
        }
        if (op == '+') {
            v1 = POP();
            v2 = NEXT_INT_OR(POP());
            PUSH(v1 + v2);
        } else if (op == '-') {
            v1 = NEXT_INT_OR(POP());
            v2 = POP();
            PUSH(v2 - v1);
        } else if (op == 'u') {
            v1 = NEXT_INT_OR(GET_VAL());
            PUSH(v1);
        } else if (op == 'o') {
            v1 = NEXT_INT_OR(0);
            if (v1) POP(); else SET_VAL(POP());
        } else if (op == '>') {
            v1 = NEXT_INT_OR(GET_VAL());
            arr_p += v1;
        } else if (op == '<') {
            v1 = NEXT_INT_OR(GET_VAL());
            arr_p -= v1;
        } else if (op == 'r') {
            SET_VAL(GET_CHAR());
        } else if (op == 'w') {
            PUT_CHAR(GET_VAL());
        } else if (op == 'R') {
            SET_VAL(GET_INT());
        } else if (op == 'W') {
            PUT_INT(GET_VAL());
        } else if (!isspace(op)) {
            printf("WARNING: unrecognized char %c", op);
        }
# ifdef DEBUG
        print_config();
# endif
        ++instr_p;
    }
}


