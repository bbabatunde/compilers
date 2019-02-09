section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, 2
  mov [ebp-8], eax
  mov eax, 2
  mov [ebp-12], eax
  mov eax, [ebp-8]
  mov edx, [ebp-12]
  cmp eax, edx
  mov eax, 0xffffffff
  je equal_3
  mov eax, 0x7fffffff
equal_3:
  mov [ebp-4], eax
  mov eax, 0xffffffff
  mov [ebp-12], eax
  mov eax, 0xffffffff
  mov [ebp-16], eax
  mov eax, [ebp-12]
  mov edx, [ebp-16]
  cmp eax, edx
  mov eax, 0xffffffff
  je equal_8
  mov eax, 0x7fffffff
equal_8:
  mov [ebp-8], eax
  mov eax, [ebp-4]
  mov [ebp-16], eax
  mov eax, [ebp-8]
  mov [ebp-20], eax
  mov eax, [ebp-16]
  mov edx, [ebp-20]
  cmp eax, edx
  mov eax, 0xffffffff
  je equal_13
  mov eax, 0x7fffffff
equal_13:
  mov [ebp-12], eax
  mov eax, [ebp-12]
  mov esp, ebp
  pop ebp
  ret
logic_expected_a_boolean:
  push eax
  push 1
  call error
logic_expected_a_boolean_edx:
  push edx
  push 1
  call error
error_not_boolean_if:
  push eax
  push 2
  call error
arithmetic_expected_a_number:
  push eax
  push 3
  call error
arithmetic_expected_a_number_EDX:
  push edx
  push 3
  call error
comparison_expected_a_number:
  push eax
  push 4
  call error
comparison_expected_a_number_EDX:
  push edx
  push 4
  call error
overflow:
  push eax
  push 5
  call error
