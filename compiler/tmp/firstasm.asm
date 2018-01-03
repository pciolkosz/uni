section .text
global main
extern printInt

main:
push ebp
mov ebp, esp
sub esp, 16
mov DWORD [ebp - 4], 1
mov eax, [ebp - 4]
mov [ebp - 8], eax
mov DWORD [ebp - 12], 5000000
push DWORD eax
call printInt
add esp, 4
jmp L0
L1:
push DWORD [ebp - 8]
call printInt
add esp, 4
mov eax, [ebp - 4]
add eax, [ebp - 8]
mov [__TMPREGS__ + 4], eax
mov [ebp - 8], eax
sub eax, [ebp - 4]
mov [__TMPREGS__ + 8], eax
mov [ebp - 4], eax
L0:
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [__TMPREGS__ + 12], eax
test eax, eax
jne L1
mov eax, 0
leave
ret


section .bss
__TMPREGS__:
resb 1024


