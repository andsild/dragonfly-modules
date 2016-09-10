let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/dragonfly-modules/playground
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +73 term://.//14028:bash
badd +16 src/Main.hs
badd +1 input.txt
badd +2 output.txt
badd +12 src/Interpreter.hs
badd +0 src/Tokenizer.hs
argglobal
silent! argdel *
argadd src/Main.hs
edit src/Main.hs
set splitbelow splitright
wincmd _ | wincmd |
split
1wincmd k
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
wincmd w
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe '1resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 1resize ' . ((&columns * 81 + 141) / 283)
exe '2resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 2resize ' . ((&columns * 80 + 141) / 283)
exe '3resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 3resize ' . ((&columns * 120 + 141) / 283)
exe '4resize ' . ((&lines * 11 + 28) / 57)
exe 'vert 4resize ' . ((&columns * 141 + 141) / 283)
exe '5resize ' . ((&lines * 11 + 28) / 57)
exe 'vert 5resize ' . ((&columns * 141 + 141) / 283)
argglobal
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 16 - ((12 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
16
normal! 0
wincmd w
argglobal
edit src/Interpreter.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 12 - ((1 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
12
normal! 0
wincmd w
argglobal
edit src/Tokenizer.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
95
normal! zo
96
normal! zo
97
normal! zo
98
normal! zo
let s:l = 68 - ((6 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
68
normal! 011|
wincmd w
argglobal
edit term://.//14028:bash
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 95 - ((10 * winheight(0) + 5) / 11)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
95
normal! 04|
wincmd w
argglobal
edit input.txt
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 5) / 11)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 04|
wincmd w
3wincmd w
exe '1resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 1resize ' . ((&columns * 81 + 141) / 283)
exe '2resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 2resize ' . ((&columns * 80 + 141) / 283)
exe '3resize ' . ((&lines * 41 + 28) / 57)
exe 'vert 3resize ' . ((&columns * 120 + 141) / 283)
exe '4resize ' . ((&lines * 11 + 28) / 57)
exe 'vert 4resize ' . ((&columns * 141 + 141) / 283)
exe '5resize ' . ((&lines * 11 + 28) / 57)
exe 'vert 5resize ' . ((&columns * 141 + 141) / 283)
tabedit input.txt
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 2 - ((1 * winheight(0) + 26) / 53)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
2
normal! 02|
tabnext 1
if exists('s:wipebuf') && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=1 shortmess=aTI
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
