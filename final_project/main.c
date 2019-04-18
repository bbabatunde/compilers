#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "uthash.h"
//#include "gc.h"



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;


extern int our_code_starts_here(int* HEAP) asm("our_code_starts_here");
extern void insert(int, int , int ) asm("insert");
extern int search(int key, int name ) asm("search");

extern void error() asm("error");
extern int print(int val) asm("print");
extern int g_PrintStack(int val, int* esp, int* ebp, int args) asm("print_stack");
extern int equal(int val1, int val2) asm("equal");
extern int* try_gc(int* alloc_ptr, int amount_needed, int* first_frame, int* stack_top) asm("try_gc");
extern int* HEAP_END asm("HEAP_END");
extern int* HEAP asm("HEAP");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");



const int NUM_TAG_MASK     = 0x00000001;
const int BOOL_TAG_MASK    = 0x00000007;
const int TUPLE_TAG_MASK   = 0x00000007;
const int CLOSURE_TAG_MASK = 0x00000007;
const int NUM_TAG          = 0x00000000;
const int BOOL_TAG         = 0x00000007;
const int TUPLE_TAG        = 0x00000001;
const int CLOSURE_TAG      = 0x00000005;
const int BOOL_TRUE        = 0xFFFFFFFF;
const int BOOL_FALSE       = 0x7FFFFFFF;
const int NIL              = ((int)NULL | TUPLE_TAG);

const int ERR_COMP_NOT_NUM     = 1;
const int ERR_ARITH_NOT_NUM    = 2;
const int ERR_LOGIC_NOT_BOOL   = 3;
const int ERR_IF_NOT_BOOL      = 4;
const int ERR_OVERFLOW         = 5;
const int ERR_GET_NOT_TUPLE    = 6;
const int ERR_GET_LOW_INDEX    = 7;
const int ERR_GET_HIGH_INDEX   = 8;
const int ERR_NIL_DEREF        = 9;
const int ERR_OUT_OF_MEMORY    = 10;
const int ERR_SET_NOT_TUPLE    = 11;
const int ERR_SET_LOW_INDEX    = 12;
const int ERR_SET_HIGH_INDEX   = 13;
const int ERR_CALL_NOT_CLOSURE = 14;
const int ERR_CALL_ARITY_ERR   = 15;

size_t HEAP_SIZE;
int* STACK_BOTTOM;
int* HEAP;
int* HEAP_END;

int* FROM_S;
int* FROM_E;
int* TO_S;
int* TO_E;

#define SIZE 200

struct my_data {
   int id;
   int offset;
   UT_hash_handle hh;         /* makes this structure hashable */

};

struct my_struct {
    int id;                    /* key */
    struct my_data* name;
    UT_hash_handle hh;         /* makes this structure hashable */
};

struct my_struct *users = NULL;

int search(int key, int fieldid) {



   struct my_struct *s;
   struct my_data *y;
   HASH_FIND_INT( users, &key, s);  /* s: output pointer */  
   HASH_FIND_INT(s->name, &fieldid, y);  /* s: output pointer */  
   return y->offset+8;

}

void insert(int offset, int fieldid, int key ) {

     struct my_struct *s;
     struct my_data *y;
  
    
    HASH_FIND_INT(users, &key, s);  /* id already in the hash? */
    if (s==NULL) {
        s = (struct my_struct*)malloc(sizeof(struct my_struct));
        s->id = key;
        s->name = NULL;
        HASH_ADD_INT( users, id, s );  /* id: name of key field */
    }
    
    HASH_FIND_INT(s->name, &fieldid, y);  /* id already in the hash? */
    if (y==NULL) {
        y = (struct my_data*)malloc(sizeof(struct my_data));
        y->id = fieldid;
        y->offset = offset;
        HASH_ADD_INT( s->name, id,y );  /* id: name of key field */
    }
    y->offset = offset;

}



int equal(int val1, int val2) {
  if (val1 == val2) { return BOOL_TRUE; }
  if (val1 == NIL || val2 == NIL) { return BOOL_FALSE; }
  if ((val1 & TUPLE_TAG_MASK) == TUPLE_TAG && (val2 & TUPLE_TAG_MASK) == TUPLE_TAG) {
    int *tup1 = (int*)(val1 - TUPLE_TAG);
    int *tup2 = (int*)(val2 - TUPLE_TAG);
    if (tup1[0] != tup2[0]) { return BOOL_FALSE; }
    for (int i = 1; i <= tup1[0] / 2; i++) {
      if (equal(tup1[i], tup2[i]) == BOOL_FALSE)
        return BOOL_FALSE;
    }
    return BOOL_TRUE;
  }
  return BOOL_FALSE;
}

int tupleCounter = 0;
void printHelp(FILE *out, int val) {
  if (val == NIL) {
    fprintf(out, "nil");
  }
  else if((val & NUM_TAG_MASK) == NUM_TAG) {
    fprintf(out, "%d", val >> 1);
  }
  else if(val == BOOL_TRUE) {
    fprintf(out, "true");
  }
  else if(val == BOOL_FALSE) {
    fprintf(out, "false");
  }
  else if ((val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    int* addr = (int*)(val - CLOSURE_TAG);
    fprintf(out, "[%p - 5] ==> <function arity %d, closed %d, fn-ptr %p>",
            (int*)val, addr[0] / 2, addr[1] / 2, (int*)addr[2]);
    /* fprintf(out, "\nClosed-over values:\n"); */
    /* for (int i = 0; i < addr[1] / 2; i++) { */
    /*   if (i > 0) { fprintf(out, "\n"); } */
    /*   if ((addr[3 + i] & TUPLE_TAG_MASK) == 5) { */
    /*     fprintf(out, "<closure %p>", (int*)addr[3 + i]); */
    /*   } else { */
    /*     printHelp(out, addr[3 + i]); */
    /*   } */
    /* } */
  }
  /* else if ((val & TUPLE_TAG_MASK) == 3) { */
  /*   fprintf(out, "forwarding to "); */
  /*   fflush(out); */
  /*   fprintf(out, "%p", (int*)(val - 3)); */
  /*   fflush(out); */
  /*   return; */
  /* } */
  else if ((val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    int* addr = (int*)(val - 1);
    // Check whether we've visited this tuple already
    if ((*addr & 0x80000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFF));
      return;
    }
    /* if (!(addr >= FROM_S && addr < FROM_E) && !(addr >= TO_S && addr < TO_E)) { */
    /*   fprintf(out, "DANGLING POINTER %p", addr); */
    /*   return; */
    /* } */
    // Mark this tuple: save its length locally, then unmark it
    int len = addr[0]; // length is encoded
    if (len & 0x1) { // actually, it's a forwarding pointer
      fprintf(out, "forwarding to %p", (int*)(len - 1));
      return;
    }
    /* fprintf(out, "Heap is:\n"); */
    /* naive_print_heap(HEAP, HEAP_END); */
    /* fprintf(out, "%p-->(len=%d)", (int*)(val - 1), len / 2); */
    /* fflush(out); */
    *(addr) = 0x80000000 | (++tupleCounter);
    fprintf(out, "(");
    for (int i = 1; i <= len / 2; i++) {
      if (i > 1) fprintf(out, ", ");
      printHelp(out, addr[i]);
    }
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = len; // length is encoded
  }
  else {
    fprintf(out, "Unknown value: %#010x", val);
  }
}

int print(int val) {
  printHelp(stdout, val);
  printf("\n");
  return val;
}

/*
int g_PrintStack(int val, int* esp, int* ebp, int args) {
  printf("ESP: %p\t==>  ", esp); fflush(stdout);
  printHelp(stdout, *esp); fflush(stdout);
  printf("\nEBP: %p\t==>  ", ebp); fflush(stdout);
  printHelp(stdout, *ebp); fflush(stdout);
  printf("\n(difference: %d)\n", esp - ebp); fflush(stdout);
  printf("Requested return val: %p ==> ", (int*)val); fflush(stdout);
  printHelp(stdout, val); fflush(stdout);
  printf("\n"); fflush(stdout);
  printf("Num args: %d\n", args);

  if (esp > ebp) {
    printf("Error: ESP and EBP are not properly oriented\n"); fflush(stdout);
  } else {
    for (int* cur = esp; cur < STACK_BOTTOM + 3; cur++) {
      if (cur == STACK_BOTTOM) {
        printf("BOT %#010x: %#010x\t==>  old ebp\n", (uint)cur, *cur); fflush(stdout);
      } else if (cur == ebp) {
        printf("EBP %#010x: %#010x\t==>  old ebp\n", (uint)cur, *cur); fflush(stdout);
      } else if (cur == ebp + 1) {
        printf("    %#010x: %#010x\t==>  saved ret\n", (uint)cur, *cur); fflush(stdout);
        esp = ebp + 2;
        ebp = (int*)(*ebp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#010x: %p\t==>  heap\n", (uint)cur, (int*)*cur); fflush(stdout);
      } else {
        printf("    %p: %#010x\t==>  ", cur, *cur); fflush(stdout);
        printHelp(stdout, *cur); fflush(stdout);
        printf("\n"); fflush(stdout);
      }
    }
  }
  return val;
}
*/


void error(int i, int val) {
  switch (i) {
  case ERR_COMP_NOT_NUM:
    fprintf(stderr, "Error: comparison expected a number\n");
    break;
  case ERR_ARITH_NOT_NUM:
    fprintf(stderr, "Error: arithmetic expected a number\n");
    break;
  case ERR_LOGIC_NOT_BOOL:
    fprintf(stderr, "Error logic expected a boolean\n");
    break;
  case ERR_IF_NOT_BOOL:
    fprintf(stderr, "Error: if expected a boolean\n");
    break;
  case ERR_OVERFLOW:
    fprintf(stderr, "Error: Integer overflow\n");
    break;
  case ERR_GET_NOT_TUPLE:
    fprintf(stderr, "Error: get expected tuple\n");
    break;
  case ERR_GET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to get\n");
    break;
  case ERR_GET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to get\n");
    break;
  case ERR_NIL_DEREF:
    fprintf(stderr, "Error: tried to access component of nil\n");
    break;
  case ERR_OUT_OF_MEMORY:
    fprintf(stderr, "Error: out of memory\n");
    break;
  case ERR_SET_NOT_TUPLE:
    fprintf(stderr, "Error: set expected tuple\n");
    break;
  case ERR_SET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to set\n");
    break;
  case ERR_SET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to set\n");
    break;
  case ERR_CALL_NOT_CLOSURE:
    fprintf(stderr, "Error: tried to call a non-closure value\n");
    break;
  case ERR_CALL_ARITY_ERR:
    fprintf(stderr, "Error: arity mismatch in call\n");
    break;
  default:
    fprintf(stderr, "Error: Unknown error code: %d\n", i);
  }
  fprintf(stderr, "%p ==> ", (int*)val);
  printHelp(stderr, val);
  fprintf(stderr, "\n");
  fflush(stderr);
  fflush(stdout);
  exit(i);
}

/*
  Try to reserve the desired number of bytes of memory, and free garbage if
  needed.  Fail (and exit the program) if there is insufficient memory.  Does 
  not actually allocate the desired number of bytes of memory; the caller 
  will do that.

  Arguments:

    int* alloc_ptr - the current top of the heap (which we store in ESI), where
                     the next allocation should occur, if possible
    int bytes_needed - the number of bytes of memory we want to allocate
                       (including padding)
    int* cur_frame - the base pointer of the topmost stack frame of our code
                     (i.e., EBP)
    int* cur_stack_top - the stack pointer of the topmost stack frame of our
                         code (i.e., ESP)

  Returns:
    The new top of the heap (i.e. the new value of ESI) after garbage collection.  
    Does not actually allocate bytes_needed space.

  Side effect:
    Also updates HEAP_END to point to the new end of the heap, if it's changed
*/


int main(int argc, char** argv) {
    HEAP_SIZE = 100000;
    
  if (argc > 1) { HEAP_SIZE = atoi(argv[1]); }
  if (HEAP_SIZE < 0 || HEAP_SIZE > 1000000) { HEAP_SIZE = 0; }
  HEAP = (int*)calloc(HEAP_SIZE + 7, sizeof (int));

  int* aligned = (int*)(((int)HEAP + 7) & ~0x7);
  HEAP_END = aligned + HEAP_SIZE;
  /* printf("HEAP = %p, aligned = %p, HEAP_END = %p\n", HEAP, aligned, HEAP_END); */
  int result = our_code_starts_here(aligned);;
  /* smarter_print_heap(aligned, HEAP_END, TO_S, TO_E); */
  /*insert(12,1,1);
  printf("%d\n", search(1,1));*/
  print(result);

  free(HEAP);
  return 0;
}

