section .text
  extern error
  extern print
  extern print_stack
  extern equal
  extern insert
  extern search
  extern try_gc
  extern naive_print_heap
  extern HEAP
  extern HEAP_END
  extern STACK_BOTTOM
  global our_code_starts_here
f1:
  ;; Stack_setup for f1
  push ebp
  mov ebp, esp
  sub esp, 0x4
  mov ecx, [ebp+8]
fun_f1_body: ; Body for f1
  mov eax, 2
  ;; Clean up for f1
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f1
f2:
  ;; Stack_setup for f2
  push ebp
  mov ebp, esp
  sub esp, 0x4
  mov ecx, [ebp+8]
fun_f2_body: ; Body for f2
  mov eax, 4
  ;; Clean up for f2
  add esp, 0x4
  mov esp, ebp
  pop ebp
  ret ; Return for f2
our_code_starts_here:
  mov [STACK_BOTTOM], ebp ; This is the bottom of our Garter stack
  ;; heap start
  mov esi, [esp+4]
  add esi, 7
  and esi, 0xfffffff8
  ;; stack start
  push ebp
  mov ebp, esp
  sub esp, 256
  mov eax, DWORD 73
  push DWORD 1
  push DWORD eax
  push DWORD 0
  call insert
  add esp, 12
  mov eax, DWORD 133
  push DWORD 1
  push DWORD eax
  push DWORD 4
  call insert
  add esp, 12
  mov eax, DWORD 150
  push DWORD 1
  push DWORD eax
  push DWORD 8
  call insert
  add esp, 12
  mov eax, DWORD 71
  push DWORD 1
  push DWORD eax
  push DWORD 12
  call insert
  add esp, 12
  mov [esi+0], DWORD 16
  mov [esi+4], DWORD 1
  mov eax, f1
  mov [esi+16], eax
  mov eax, f2
  mov [esi+20], eax
  mov eax, esi
  add eax, 0x1
  add esi, 16
  mov [ebp-4], eax
  ;; body start
  ;; compile bindings start 
  ;; compile bindings end 
  mov eax, [ebp-4]
  mov esi, eax
  add esi, [esi+4]
  mov eax, esi
  mov [ebp-12], eax
  mov eax, [ebp-12]
  mov [ebp-16], eax
  mov eax, [ebp-16]
  sub eax, 0x1
  push 71
  push DWORD [eax+4]
  call search
  add esp, 8
  mov ecx, eax
  mov eax, [ebp-16]
  push DWORD eax
  sub eax, 0x1
  call [eax + ecx]
  add esp, 4
  mov [ebp-20], eax
  mov eax, [ebp-20]
  ;; postlude start
  mov esp, ebp
  pop ebp
  ret
logic_expected_a_boolean:
  push eax
  push 3
  call error
logic_expected_a_boolean_edx:
  push edx
  push 3
  call error
error_not_boolean_if:
  push eax
  push 4
  call error
arithmetic_expected_a_number:
  push eax
  push 2
  call error
arithmetic_expected_a_number_EDX:
  push edx
  push 2
  call error
comparison_expected_a_number:
  push eax
  push 1
  call error
comparison_expected_a_number_EDX:
  push edx
  push 1
  call error
overflow:
  push eax
  push 5
  call error
error_not_tuple:
  push ecx
  push 6
  call error
index_too_high:
  push edx
  push 8
  call error
index_too_low:
  push eax
  push 7
  call error
error_not_closure:
  push ecx
  push 10
  call error
error_wrong_arity:
  push eax
  push 11
  call error
