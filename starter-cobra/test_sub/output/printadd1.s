section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, 8
  push eax
  call print
  add esp, 4
  mov [ebp-4], eax
  mov eax, [ebp-4]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  add eax, 2
  jo overflow
  mov [ebp-8], eax
  mov eax, [ebp-8]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  add eax, 2
  jo overflow
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
