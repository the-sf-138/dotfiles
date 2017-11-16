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
set listchars=tab:>>,eol:$

nmap 0 ^
nmap ) $

" Better pane switching
nmap <S-l> <c-w>l
nmap <S-h> <c-w>h
nmap <S-j> <c-w>j
nmap <S-k> <c-w>k
" Better tab switching
nmap <C-l> gt
nmap <C-h> gT

nmap yy y$

" I forgot I even made a mapping for this
" So maybe it's trash
nmap <S-b> $dl

" Executing over ranges made easy
vmap <Leader>n :normal
nmap <Leader>i :tabe /usr/include

nmap cd :lcd %:p:h<CR>

" Handle annoying line-wrapping
nnoremap j gj
nnoremap k gk

" Command mode is now cool
nnoremap : q:

" Fast save
nnoremap <Leader>w :w<CR>

" Search for selection
vnoremap // y/<C-R>"<CR>

filetype on
autocmd FileType python set tabstop=8 shiftwidth=8 sts=8
autocmd FileType css set tabstop=8 shiftwidth=8 sts=8
autocmd FileType html set tabstop=2 shiftwidth=2 sts=2
autocmd FileType tex nmap cc :w<CR>:! pdflatex %<CR>
autocmd FileType make setlocal noexpandtab
autocmd BufRead,BufNewFile *.s setfiletype gas

" Copy whole file to clipboard on machines with xclip
nmap cp :! cat % \| xclip -selection c<CR>

" Change directory to this files parent dir
nmap cd :lcd %:p:h<CR>

call plug#begin()

Plug 'tikhomirov/vim-glsl'
Plug 'easymotion/vim-easymotion'
Plug 'vim-syntastic/syntastic'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'lervag/vimtex'

call plug#end()

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled=1

map <space> <Plug>(easymotion-bd-w)

autocmd BufNewFile,BufRead *.vs,*.fs set ft=glsl
