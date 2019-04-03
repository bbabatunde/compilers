#include <stdio.h>
#include "gc.h"

const int NUM_TAG_MASK_GC     = 0x00000001;
const int BOOL_TAG_MASK_GC    = 0x00000007;
const int TUPLE_TAG_MASK_GC   = 0x00000007;
const int CLOSURE_TAG_MASK_GC = 0x00000007;
const int FW_PTR_MASK_GC      = 0x00000007;

const int NUM_TAG_GC          = 0x00000000;
const int BOOL_TAG_GC         = 0x00000007;
const int TUPLE_TAG_GC        = 0x00000001;
const int CLOSURE_TAG_GC      = 0x00000005;
const int FW_PTR              = 0x00000008;           
const int BOOL_TRUE_GC        = 0xFFFFFFFF;
const int BOOL_FALSE_GC       = 0x7FFFFFFF;
const int NIL_GC              = ((int)NULL | TUPLE_TAG_GC);


int tupleCounterGC = 0;
void printHelpGC(FILE *out, int val) {
  if (val == NIL_GC) {
    fprintf(out, "nil");
  }
  else if((val & NUM_TAG_MASK_GC) == NUM_TAG_GC) {
    fprintf(out, "%d", val >> 1);
  }
  else if(val == BOOL_TRUE_GC) {
    fprintf(out, "true");
  }
  else if(val == BOOL_FALSE_GC) {
    fprintf(out, "false");
  }
  else if ((val & CLOSURE_TAG_MASK_GC) == CLOSURE_TAG_GC) {
    int* addr = (int*)(val - CLOSURE_TAG_GC);
    fprintf(out, "[%p - 5] ==> <function arity %d, closed %d, fn-ptr %p>",
            (int*)val, addr[0] / 2, addr[1] / 2, (int*)addr[2]);
  }
  else if ((val & TUPLE_TAG_MASK_GC) == TUPLE_TAG_GC) {
    int* addr = (int*)(val - 1);
    if ((*addr & 0x80000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFF));
      return;
    }
    int len = addr[0]; // length is encoded
    if (len & 0x1) { // actually, it's a forwarding pointer
      fprintf(out, "forwarding to %p", (int*)(len - 1));
      return;

    *(addr) = 0x80000000 | (++tupleCounterGC);
    fprintf(out, "(");
    for (int i = 1; i <= len / 2; i++) {
      if (i > 1) fprintf(out, ", ");
      printHelpGC(out, addr[i]);
    }
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = len; // length is encoded
  }else if((val & FW_PTR_MASK_GC) == FW_PTR){
      fprintf(out, "forwarding to %p", (int*)(val - FW_PTR));    
  }
   

  else {
    fprintf(out, "Unknown value: %#010x", val);
  }
}
}


void naive_print_heap(int* heap, int size) {
  for(int i = 0; i < size; i += 1) {
    printf("  %d/%p: %p (%d)\n", i, (heap + i), (int*)(*(heap + i)), *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(int* from_start, int* from_end, int* to_start, int* to_end) {
 
  int* from_val;
  int* to_val;

  printf("printing from space begins\n");
  for(from_val = from_start; from_val < from_end; from_val++){
      printHelpGC(stdout, *(from_val));
      printf("\n");
  }
  printf("printing from space ends\n");


  printf("printing to space begins\n");
  for(to_val = to_start; from_val < to_end; to_start++){
      printHelpGC(stdout, *(to_val));
       printf("\n");
  }
  printf("printing to space ends\n");


}

/*
  Copies a Garter value from the given address to the new heap, 
  but only if the value is heap-allocated and needs copying.

  Arguments:
    garter_val_addr: the *address* of some Garter value, which contains a Garter value,
                     i.e. a tagged word.  
                     It may or may not be a pointer to a heap-allocated value...
    heap_top: the location at which to begin copying, if any copying is needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old location 
    with a forwarding pointer to its new location
 */
int* copy_if_needed(int* garter_val_addr, int* heap_top) {
  
  int garter_val = *(garter_val_addr);
  if(((garter_val & NUM_TAG_MASK_GC) == NUM_TAG_GC) || (garter_val == BOOL_TRUE_GC) || (garter_val == BOOL_FALSE_GC)){

    return heap_top;

  } else if ((garter_val & TUPLE_TAG_MASK_GC) == TUPLE_TAG_GC){

     int* heap_thing_addr = (int*)(garter_val - TUPLE_TAG_GC);
     int size = heap_thing_addr[0];
     int i;
     //1.Copy the full contents of heap_thing to heap_top.
     for(i = 0; i < size; i++){
        heap_top[i]= garter_val_addr[i];
     }
     //2.Update the value at garter_val_addr with the value of heap_top.
     garter_val_addr = heap_top + TUPLE_TAG_GC;

      heap_top[0] = heap_top[0] + NUM_TAG_MASK_GC;
      //create forwarding pointer
       int* fwd_ptr = heap_top  + FW_PTR;

      //3.Replace the value at heap_thing_addr (i.e., the location referred to by garter_val) with a forwarding pointer to heap_top
       heap_thing_addr = fwd_ptr;

       int * curr_heap_top = heap_top;


      //4.Increment heap_top as needed to record the allocation.
       heap_top += size;

       for(i = 0; i < size; i++){
          heap_top = copy_if_needed((curr_heap_top+i), heap_top);
     }

  }else if  ((garter_val & CLOSURE_TAG_MASK_GC) == CLOSURE_TAG_GC){
     int* heap_thing_addr = (int*)(garter_val - CLOSURE_TAG_GC);

     int vals_no = heap_thing_addr[2];
     int size = 3 + vals_no;
     int i;


     //1.Copy the full contents of heap_thing to heap_top.
     for(i = 0; i < size; i++){
        heap_top[i] = heap_thing_addr[i];

     }
     //2.Update the value at garter_val_addr with the value of heap_top.
    int* curr_heap_top = heap_top;
    garter_val_addr = heap_top + CLOSURE_TAG_GC;
    //create forwarding pointer
    int* fwd_ptr = heap_top + FW_PTR;
    //3.Replace the value at heap_thing_addr (i.e., the location referred to by garter_val) with a forwarding pointer to heap_top
     heap_thing_addr = fwd_ptr;
      //4.Increment heap_top as needed to record the allocation.
      heap_top += size;
     for(i = 0; i < size; i++){
          heap_top = copy_if_needed((curr_heap_top+i), heap_top);
     }
  }else if ((garter_val & FW_PTR_MASK_GC) == FW_PTR){
           int * temp = (int*)(garter_val - FW_PTR);

           if(temp[0] & 0x1){
               temp[0] = temp[0] << 1;
               garter_val_addr = temp + TUPLE_TAG_GC;
           }else{
                garter_val_addr = temp + CLOSURE_TAG_GC;
           }
  }

  // no-op for now
  return heap_top;
}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost Garter frame
    top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being compacted
    to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
int* gc(int* bottom_frame, int* top_frame, int* top_stack, int* from_start, int* from_end, int* to_start) {
  for (int* cur_word = top_stack+1 ; cur_word <= top_stack; cur_word++) {
    to_start = copy_if_needed(cur_word, to_start);
  }
  if (top_frame < bottom_frame)
    to_start = gc(bottom_frame,
                  (int*)(*top_frame), // [top_frame] points to the saved EBP, which is the next stack frame
                  top_frame + 2,      // [top_frame+4] points to the return address
                                      // so [top_frame+8] is the next frame's stack-top
                  from_start,
                  from_end,
                  to_start); // to_start has been changed to include any newly copied data
  // after copying the remaining stack frames, return the new allocation starting point
  return to_start;       
}

