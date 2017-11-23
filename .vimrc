" Annoying color shit
highlight Normal ctermfg=grey ctermbg=Black
set t_Co=256
set t_ut=
set background=dark

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

" ^ and $ sign are so far smh
nnoremap 0 ^
nnoremap ) $

" Better pane switching
nnoremap <S-l> <c-w>l
nnoremap <S-h> <c-w>h
nnoremap <S-j> <c-w>j
nnoremap <S-k> <c-w>k

" Better tab switching
nnoremap <C-l> gt
nnoremap <C-h> gT

nnoremap yy y$

" I forgot I even made a mapping for this
" So maybe it's trash
nnoremap <S-b> $dl

" Executing over ranges made easy
vmap <Leader>n :normal

" Open system headers more easily..
nnoremap <Leader>i q:itabe /usr/include

" man page prefix
nnoremap man q:i! man 

nnoremap cd :lcd %:p:h<CR>

" Handle annoying line-wrapping
nnoremap j gj
nnoremap k gk

" Command mode is now cool
nnoremap : q:

" Fast save
nnoremap <Leader>w :w<CR>

" Save and re-enter insert
inoremap <Leader>w <Esc>:w<CR>a

" Search for selection
vnoremap // y/<C-R>"<CR>

" Copy whole file to clipboard on machines with xclip
nmap cp :! cat % \| xclip -selection c<CR>

" Filetype specifics
filetype on
autocmd FileType python set tabstop=8 shiftwidth=8 sts=8
autocmd FileType css    set tabstop=8 shiftwidth=8 sts=8
autocmd FileType html   set tabstop=2 shiftwidth=2 sts=2
autocmd FileType tex    nmap cc :w<CR>:! pdflatex %<CR>
autocmd FileType make   setlocal noexpandtab

autocmd BufRead,BufNewFile *.s       setfiletype gas
autocmd BufNewFile,BufRead *.vs,*.fs set ft=glsl


call plug#begin()
Plug 'tikhomirov/vim-glsl'
Plug 'dracula/vim'
Plug 'easymotion/vim-easymotion'
Plug 'vim-syntastic/syntastic'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'justinmk/vim-syntax-extra'
Plug 'octol/vim-cpp-enhanced-highlight'
"Plug 'lervag/vimtex'
Plug 'neovimhaskell/haskell-vim'
call plug#end()

" Color ish
colorscheme gruvbox

highlight Normal     ctermfg=white
highlight NonText    ctermfg=grey
highlight SpecialKey ctermfg=grey

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled=1

map <space> <Plug>(easymotion-bd-w)

"let g:cpp_class_scope_highlight = 1
"let g:cpp_member_variable_highlight = 1

