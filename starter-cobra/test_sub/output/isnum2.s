section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 8
  mov eax, 0xffffffff
  push eax
  call print
  add esp, 4
  mov [ebp-4], eax
  mov eax, [ebp-4]
  test eax, 0x1
  jz isNumtrue_7
  mov eax, 0x7fffffff
  jmp isNumdone_7
isNumtrue_7:
  mov eax, 0xffffffff
isNumdone_7:
  mov [ebp-8], eax
  mov eax, [ebp-8]
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
