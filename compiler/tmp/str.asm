section .text
global main
extern printInt
extern printString
extern readInt
extern readString
extern error
extern __CONCAT_STRINGS__

times2:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 8]
imul eax, 2
mov [__TMPREGS__ + 4], eax
leave
ret
times3:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 8]
imul eax, 3
mov [__TMPREGS__ + 8], eax
leave
ret
main:
push ebp
mov ebp, esp
sub esp, 24
push DWORD 1
call times2
add esp, 4
mov edx, eax
push DWORD 3
call times3
add esp, 4
xchg eax, edx
add eax, edx
mov [__TMPREGS__ + 12], eax
push DWORD eax
call printInt
add esp, 4
mov DWORD [ebp - 16], L0
mov DWORD [ebp - 20], L1
push DWORD[ebp - 20]
push DWORD[ebp - 16]
call __CONCAT_STRINGS__
sub esp, 8
mov [__TMPREGS__ + 16], eax
mov [ebp - 20], eax
push DWORD eax
call printString
add esp, 4
mov DWORD [ebp - 4], 1
mov eax, [ebp - 4]
mov [ebp - 8], eax
mov DWORD [ebp - 12], 5000000
push DWORD eax
call printInt
add esp, 4
jmp L2
L3:
push DWORD [ebp - 8]
call printInt
add esp, 4
mov eax, [ebp - 4]
add eax, [ebp - 8]
mov [__TMPREGS__ + 20], eax
mov [ebp - 8], eax
sub eax, [ebp - 4]
mov [__TMPREGS__ + 24], eax
mov [ebp - 4], eax
L2:
mov eax, [ebp - 8]
cmp eax, [ebp - 12]
setl al
and eax, 0xff
mov [__TMPREGS__ + 28], eax
test eax, eax
jne L3
mov eax, 0
leave
ret


section .bss
__TMPREGS__:
resb 32

section .data
L0  db 'a',0
L1  db 'b',0

