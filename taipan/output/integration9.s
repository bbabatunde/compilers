section .text
extern error
extern print
global our_code_starts_here
f:
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, 2
  mov [ebp-4], eax
  mov edx, [ebp-12]
  mov [ebp-8], edx
  mov eax, [ebp-4]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-8]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  imul eax, edx
  jo overflow
  sar eax, 1
  jo overflow
  mov esp, ebp
  pop ebp
  ret
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, 16
  push eax
  mov eax, 4
  push eax
  mov eax, 2
  push eax
  sub esp, 4
  call f
  add esp, 4
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
