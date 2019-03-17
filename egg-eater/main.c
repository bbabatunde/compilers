#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

extern int our_code_starts_here() asm("our_code_starts_here");
extern int equal(int a, int b) asm("equal");
extern int print(int val) asm("print");
extern void error(int errCode, int val) asm("error");

void* printer(int val, int tuple_size );

int* HEAP;

const int BOOL_TAG   = 0x00000001;
const int BOOL_TRUE  = 0xFFFFFFFF; // These must be the same values
const int BOOL_FALSE = 0x7FFFFFFF; // as chosen in compile.ml
const int TUPLE_TAG  = 0x00000001;

const int ERR_LOGIC_NOT_BOOLEAN = 1;
const int ERR_IF_PRED_BOOL = 2;
const int ERR_ARITH_NUM = 3;
const int ERR_CMP_NUM = 4;
const int ERR_OVERFLOW = 5;
const int ERR_NOT_NUMBER = 10;


int equal(int left, int right) {
   
  if(left == right ) 
  { return BOOL_TRUE; }
  else
   { return BOOL_FALSE; }
}


int input(){
    char val[20];
    scanf("%d", &input);

    if(isdigit(val)){
      int num = atoi(val);
      return num << 1;
    }
    else if(isalpha(val)){
      if(strcmp(val ,"true") == 0)
        return BOOL_TRUE;
      else if (strcmp(val ,"false") == 0)
        return BOOL_FALSE;
      else
        exit(1);
    }
  exit(1);
}

int print(int val) {
  int tuple_size = 2;
  if ((val & TUPLE_TAG) == 1){
     int* tupptr = (int*)(val - 1);
     tuple_size = *(tupptr);
  }
  printer(val, tuple_size);
  printf("\n");

  return val;
}

void* printer(int val , int tuple_size){

   if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%d", val >> 1); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else if ((val & TUPLE_TAG) == 1) {
         if( (val - 1) == 0){

          }else{
         
          int* tupptr = (int*)(val - 1);
          if(tuple_size == 2) {
          printf("(");
          printer(*(tupptr + 1),tuple_size);
          printf(", ");
          printer(*(tupptr + 2), tuple_size);
          printf(")");
          //TODO 1 OR 2
          *(tupptr) -= 2;
          }
          else {
            int i;
            printf("(");
            for(i = 1; i < tuple_size + 1; i++){
               printer(*(tupptr + i),tuple_size);
                if(i == tuple_size){

                }
                else
                printf(", ");
            }
            printf(")");

          }

        }

  }

  else {
    printf("Unknown value: %#010x\n", val); // print unknown val in hex
  }
}

void error(int errCode, int val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %010x\n", val);
  } else if (errCode == ERR_LOGIC_NOT_BOOLEAN) {
    fprintf(stderr, "logic expected a boolean, but got %d\n", val >> 1);
  } else  if (errCode == ERR_OVERFLOW) {
    fprintf(stderr, "overflow value %010x\n", val);
  }else  if (errCode == ERR_IF_PRED_BOOL) {
    fprintf(stderr, "if expected a boolean, but got %d\n", val >> 1);
  }else  if (errCode == ERR_ARITH_NUM) {
    fprintf(stderr, "arithmetic expected a number, but got %010x\n", val);
  }else  if (errCode == ERR_CMP_NUM) {
    fprintf(stderr, "comparison expected a number, but got %010x\n", val);
  }


  exit(errCode);
}


// main should remain unchanged
// You can pass in a numeric argument to your program when you run it,
// to specify the size of the available heap.  You may find this useful
// for debugging...
int main(int argc, char** argv) {
  int size = 100000;
  if (argc > 1) { size = atoi(argv[1]); }
  if (size < 0 || size > 1000000) { size = 0; }
  HEAP = calloc(size, sizeof (int));
  int result = our_code_starts_here(HEAP);
  print(result);
  free(HEAP);
  return 0;
}
