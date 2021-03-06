" General options
set backspace=indent,eol,start
set cindent autoindent
set confirm
set encoding=utf-8
set incsearch
set hidden
set mouse=a
set nocompatible
set noexpandtab
set nohlsearch
set number
set ruler
set showcmd
set showmatch
set showmode
set tags=./tags,tags,/usr/src/sys/arch/amd64/tags,/var/db/libc.tags
set t_Co=256
set ttyfast
source /usr/share/vim/vim81/ftplugin/man.vim

filetype plugin on

nnoremap <C-N> :tag<CR>
nnoremap <C-P> :pop<CR>

nnoremap <C-P> :bprev<CR>

" fix glitches in certain terminals
" backspace
imap ^? ^H

" f7 toggles spelling on/off
nn <F7> :setlocal spell! spell?<CR>

" view binary files as hex
" Convert to hex and back; does not save changes
nn <F5> :%!xxd -g 1<CR>
nn <F6> :%!xxd -g 1 -r<CR>

" makefile magic
" compiler stuff
let g:compiler_gcc_ignore_unmatched_lines=1
let mapleader=','
" quickfix :make
nmap <silent> <Leader>m :wa<CR>:silent! make \| redraw! \| cw<CR><CR>
vmap <silent> <Leader>m :wa<CR>:silent! make \| redraw! \| cw<CR><CR>
nn ,c :silent! make clean \| redraw! \| cw<CR><CR>
" handy shortcuts
map <Leader>h :ccl<CR>
map <Leader>s :cw<CR>
map <Leader>l :cl<CR>
" jump between messages
map <Leader>n :cn<CR>
map <Leader>p :cp<CR>

" format selection
map <Leader>f :!fmt<CR>


" @c comment, @u uncomment, @p print function name
let @u='0xx$xx^['
let @c='I/*^[A*/^['
let @p='ofprintf(stderr, "%s\n", __func__);^['

:ab #d #define
:ab #i #include

autocmd FileType make setlocal noexpandtab
autocmd FileType c setlocal noexpandtab
autocmd FileType cc setlocal noexpandtab
autocmd FileType python setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType ada setlocal expandtab shiftwidth=3 softtabstop=3 tabstop=3

" Plugins

" Initialization
call plug#begin('~/.vim/bundle')

Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf'
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'ambv/black', { 'for': 'python' }
Plug 'mileszs/ack.vim'
Plug 'racer-rust/vim-racer', { 'for': 'rust' }

" Themes
Plug 'KKPMW/oldbook-vim' 
Plug 'agreco/vim-citylights'
Plug 'xdefrag/vim-beelzebub'
Plug 'logico-dev/typewriter'
Plug 'vim-scripts/wombat256.vim'

call plug#end()

" NERDTree
map <Leader>o :NERDTree<CR>

" FZF
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

command! FZFBuffers call fzf#run({'source': map(range(1, bufnr('$')), 'bufname(v:val)'), 'sink': 'e', 'down': '30%'})
map <Leader>b :FZFBuffers<CR>

" Ack
if executable('ag')
	let g:ackprg = 'ag --vimgrep'
endif

" The space is signficant.
map <Leader>/ :Ack 

" Go stuff
map <Leader>i :GoImports<CR>
map <Leader>i :GoImports<CR>

let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"

au FileType rust nmap gd <Plug>(rust-def)
autocmd Filetype c,cpp  inoremap <buffer> <Leader>t :wa<CR>:silent! make test \| redraw! \| cw<CR><CR>
autocmd Filetype go  map <buffer> <Leader>t :wa<CR>:GoTest<CR>
autocmd Filetype go  map <buffer> C-] :w<CR>:GoDef<CR>
autocmd Filetype go  map <buffer> C-\ :w<CR>:GoDefPop<CR>


colorscheme oldbook
