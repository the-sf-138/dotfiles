" Annoying color shit
"let neodark#background='black' " black, gray or brown
highlight Normal ctermfg=grey ctermbg=Black
set t_Co=256
set t_ut=
set background=dark
colorscheme gruvbox
syntax on
set nu
set cursorcolumn
set cursorline
set relativenumber

set tabstop=4
set shiftwidth=4
set expandtab
set hlsearch
set autoindent
set list
retab
nmap H ^
nmap L $
nmap <S-l> <c-w>l
nmap <S-h> <c-w>h
nmap <S-j> <c-w>j
nmap <S-k> <c-w>k
nmap <C-l> gt
nmap <C-h> gT
nmap <S-b> $dl
nmap yy y$

nnoremap j gj
nnoremap k gk

autocmd FileType python set tabstop=8 shiftwidth=8 sts=8
autocmd FileType css set tabstop=8 shiftwidth=8 sts=8
autocmd FileType html set tabstop=2 shiftwidth=2 sts=2
autocmd FileType tex nmap cc :w<Enter>:! pdflatex %<Enter>

nmap cp :! cat % \| xclip -selection c<Enter>

call plug#begin()

Plug 'yuttie/hydrangea-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'lervag/vimtex'

call plug#end()

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled=1
