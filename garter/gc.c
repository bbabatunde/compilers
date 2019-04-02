#include <stdio.h>

void naive_print_heap(int* heap, int size) {
  for(int i = 0; i < size; i += 1) {
    printf("  %d/%p: %p (%d)\n", i, (heap + i), (int*)(*(heap + i)), *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(int* from_start, int* from_end, int* to_start, int* to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
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
  struct MyStruct *fwd_ptr; // this is a forward reference.

  if(((garter_val & NUM_TAG_MASK) == NUM_TAG) || (garter_val == BOOL_TRUE) || (garter_val == BOOL_FALSE)){

    return heap_top
  } else if ((garter_val & TUPLE_TAG_MASK) == TUPLE_TAG){

     int heap_thing = garter_val;
     int* heap_thing_addr = (int*)(garter_val - TUPLE_TAG);
     int size = heap_thing_addr[0];
  }else if  ((garter_val & CLOSURE_TAG_MASK) == CLOSURE_TAG){
     int* heap_thing_addr = (int*)(garter_val - CLOSURE_TAG);
     int vals_no = heap_thing_addr[2]
     int size = 3 + vals_no;
     int i;
     //1.Copy the full contents of heap_thing to heap_top.
     for(i = 0; i < size; i++){
        heap_top[i]= garter_val_addr[i];

     }
     //2.Update the value at garter_val_addr with the value of heap_top.
     *(garter_val_addr) = *(heap_top);


      //3.Replace the value at heap_thing_addr (i.e., the location referred to by garter_val) with a forwarding pointer to heap_top
       *(heap_thing_addr) = fwd_ptr;
     //4.Increment heap_top as needed to record the allocation.
     heap_top += size;
     int * curr_heap_top = heap_top;
     for(i = 0; i < size; i++){
        heap_top = copy_if_needed(*(curr_heap_top+i), heap_top);
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
  for (int* cur_word = top_stack /* maybe need a +1 here? */; cur_word <= top_stack; cur_word++) {
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

