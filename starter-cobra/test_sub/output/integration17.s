section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 20
  mov eax, 8
  mov [ebp-4], eax
  mov eax, 2
  mov [ebp-8], eax
  mov eax, 6
  mov [ebp-12], eax
  mov eax, [ebp-12]
  mov [ebp-20], eax
  mov eax, [ebp-8]
  mov [ebp-24], eax
  mov eax, [ebp-20]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-24]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  sub eax, edx
  jo overflow
  mov [ebp-16], eax
  mov eax, [ebp-4]
  mov [ebp-24], eax
  mov eax, [ebp-16]
  mov [ebp-28], eax
  mov eax, [ebp-24]
  test eax, 0x1
  jnz arithmetic_expected_a_number
  mov edx, [ebp-28]
  test edx, 0x1
  jnz arithmetic_expected_a_number_EDX
  imul eax, edx
  jo overflow
  sar eax, 1
  jo overflow
  mov [ebp-20], eax
  mov eax, [ebp-20]
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
