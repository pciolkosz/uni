section .text
global main
extern printInt
extern printString
extern readInt
extern readString
extern error
extern __CONCAT_STRINGS__

main:
push ebp
mov ebp, esp
sub esp, 8
push DWORD 10
call fac
add esp, 4
push DWORD eax
call printInt
add esp, 4
push DWORD 10
call rfac
add esp, 4
push DWORD eax
call printInt
add esp, 4
push DWORD 10
call mfac
add esp, 4
push DWORD eax
call printInt
add esp, 4
push DWORD 10
call ifac
add esp, 4
push DWORD eax
call printInt
add esp, 4
mov DWORD [ebp - 4], L0
mov DWORD [ebp - 8], 10
mov DWORD [ebp - 12], 1
jmp L1
L2:
mov eax, [ebp - 12]
imul eax, [ebp - 8]
mov [__TMPREGS__ + 4], eax
mov [ebp - 12], eax
dec DWORD [ebp - 8]
L1:
mov eax, [ebp - 8]
cmp eax, 0
setg al
and eax, 0xff
mov [__TMPREGS__ + 8], eax
test eax, eax
jne L2
push DWORD [ebp - 12]
call printInt
add esp, 4
push DWORD L3
push DWORD 60
call repStr
add esp, 8
push DWORD eax
call printString
add esp, 4
push DWORD L4
call printString
add esp, 4
push DWORD L5
call printString
add esp, 4
mov eax, 0
leave
ret
fac:
push ebp
mov ebp, esp
sub esp, 12
mov DWORD [ebp - 4], 0
mov DWORD [ebp - 8], 0
mov DWORD [ebp - 4], 1
mov eax, [ebp + 8]
mov [ebp - 8], eax
jmp L6
L7:
mov eax, [ebp - 4]
imul eax, [ebp - 8]
mov [__TMPREGS__ + 12], eax
mov [ebp - 4], eax
mov eax, [ebp - 8]
sub eax, 1
mov [__TMPREGS__ + 16], eax
mov [ebp - 8], eax
L6:
mov eax, [ebp - 8]
cmp eax, 0
setg al
and eax, 0xff
mov [__TMPREGS__ + 20], eax
test eax, eax
jne L7
mov eax, [ebp - 4]
leave
ret
rfac:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 8]
cmp eax, 0
sete al
and eax, 0xff
mov [__TMPREGS__ + 24], eax
test eax, eax
jne L8
push DWORD [ebp + 8]
mov eax, [ebp + 8]
sub eax, 1
mov [__TMPREGS__ + 28], eax
push DWORD eax
call rfac
add esp, 4
mov edx, eax
pop DWORD eax
imul eax, edx
mov [__TMPREGS__ + 32], eax
leave
ret
L8:
mov eax, 1
leave
ret
mfac:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 8]
cmp eax, 0
sete al
and eax, 0xff
mov [__TMPREGS__ + 36], eax
test eax, eax
jne L10
push DWORD [ebp + 8]
mov eax, [ebp + 8]
sub eax, 1
mov [__TMPREGS__ + 40], eax
push DWORD eax
call nfac
add esp, 4
mov edx, eax
pop DWORD eax
imul eax, edx
mov [__TMPREGS__ + 44], eax
leave
ret
L10:
mov eax, 1
leave
ret
nfac:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 8]
cmp eax, 0
setne al
and eax, 0xff
mov [__TMPREGS__ + 48], eax
test eax, eax
jne L12
mov eax, 1
leave
ret
L12:
mov eax, [ebp + 8]
sub eax, 1
mov [__TMPREGS__ + 52], eax
push DWORD eax
call mfac
add esp, 4
imul eax, [ebp + 8]
mov [__TMPREGS__ + 56], eax
leave
ret
ifac:
push ebp
mov ebp, esp
sub esp, 4
push DWORD 1
push DWORD [ebp + 8]
call ifac2f
add esp, 8
leave
ret
ifac2f:
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp + 12]
cmp eax, [ebp + 8]
sete al
and eax, 0xff
mov [__TMPREGS__ + 60], eax
xor eax, 1
mov [__TMPREGS__ + 64], eax
test eax, eax
jne L14
mov eax, [ebp + 12]
leave
ret
L14:
mov eax, [ebp + 12]
cmp eax, [ebp + 8]
setg al
and eax, 0xff
mov [__TMPREGS__ + 68], eax
xor eax, 1
mov [__TMPREGS__ + 72], eax
test eax, eax
jne L15
mov eax, 1
leave
ret
L15:
mov DWORD [ebp - 4], 0
mov eax, [ebp + 12]
add eax, [ebp + 8]
mov [__TMPREGS__ + 76], eax
xor edx, edx
mov ecx, 2
idiv ecx
mov [__TMPREGS__ + 80], eax
mov [ebp - 4], eax
push DWORD [ebp + 12]
push DWORD eax
call ifac2f
add esp, 8
push DWORD eax
mov eax, [ebp - 4]
add eax, 1
mov [__TMPREGS__ + 84], eax
push DWORD eax
push DWORD [ebp + 8]
call ifac2f
add esp, 8
mov edx, eax
pop DWORD eax
imul eax, edx
mov [__TMPREGS__ + 88], eax
leave
ret
repStr:
push ebp
mov ebp, esp
sub esp, 12
mov DWORD [ebp - 4], L16
mov DWORD [ebp - 8], 0
jmp L17
L18:
push DWORD [ebp + 12]
push DWORD [ebp - 4]
call __CONCAT_STRINGS__
sub esp, 8
mov [__TMPREGS__ + 92], eax
mov [ebp - 4], eax
inc DWORD [ebp - 8]
L17:
mov eax, [ebp - 8]
cmp eax, [ebp + 8]
setl al
and eax, 0xff
mov [__TMPREGS__ + 96], eax
test eax, eax
jne L18
mov eax, [ebp - 4]
leave
ret


section .bss
__TMPREGS__:
resb 100

section .data
L0  db '',0
L3  db '=',0
L4  db 'hello */',0
L5  db '/* world',0
L16  db '',0

