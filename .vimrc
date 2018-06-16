" Annoying color shit

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
set listchars=tab:¦»,eol:⏎

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
nnoremap <Leader>pwd :<CR>

" Handle annoying line-wrapping
nnoremap j gj
nnoremap k gk

" Command mode is now cool
nnoremap : q:

" Search mode is now cool
nnoremap / q/i

" Fast save
nnoremap <Leader>w :w<CR>

" Save and re-enter insert
inoremap <Leader>w <Esc>:w<CR>a

" Search for selection
vnoremap // y/<C-R>"<CR>

" Match angled brackets
set matchpairs+=<:>

" Copy whole file to clipboard on machines with xclip
nnoremap cp :! cat % \| xclip -selection c<CR>

" Filetype specifics
filetype on
autocmd FileType python set tabstop=8 shiftwidth=8 sts=8
autocmd FileType css    set tabstop=8 shiftwidth=8 sts=8
autocmd FileType html   set tabstop=2 shiftwidth=2 sts=2
autocmd FileType tex    nnoremap cc :w<CR>:! pdflatex %<CR>
autocmd FileType cpp    nnoremap cc :SyntasticCheck<CR>
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
"Plug 'hzchris/vim-material'
Plug 'ajmwagar/vim-deus'
Plug 'joshdick/onedark.vim'
call plug#end()

" Turn off auto-checking on syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_mode_map = {
    \ "mode": "passive"}

" Color ish
"highlight Normal ctermfg=grey ctermbg=Black
set t_Co=256
set t_ut=
colorscheme gruvbox
set background=dark
"autocmd FileType haskell set background=dark colorscheme gruvbox

"highlight NonText    ctermfg=17
"highlight SpecialKey ctermfg=17

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled=1
let g:airline_theme='minimalist'


map <space> <Plug>(easymotion-bd-w)

"let g:cpp_class_scope_highlight = 1
"let g:cpp_member_variable_highlight = 1

inoremap <expr> j ((pumvisible())?("\<C-n>"):("j"))
inoremap <expr> k ((pumvisible())?("\<C-p>"):("k"))

" Fast Copy & Paste
nnoremap qc yaw
nnoremap qp vawp

let g:fast_hjkl = 0
function! ToggleFastHJKL()
    if g:fast_hjkl == 0
        let g:fast_hjkl = 1
        nnoremap j 7j
        nnoremap k 7k
        nnoremap h 2b
        nnoremap l 2w
    else
        let g:fast_hjkl = 0
        nunmap j
        nunmap k
        nunmap h
        nunmap l
    endif
endfunction
nnoremap <S-s> :call ToggleFastHJKL()<CR>

let g:smart_words = 0
function! ToggleSmartWords()
    if g:smart_words == 0
        let g:smart_words = 1
        nnoremap w :call search('\<\\|\>\\|\u\\|_', 'W')<CR>
        nnoremap b :call search('\<\\|\>\\|\u\\|_', 'bW')<CR>
        onoremap w :call search('\<\\|\>\\|\u\\|_', 'W')<CR>
        onoremap b :call search('\<\\|\>\\|\u\\|_', 'bW')<CR>
    else
        let g:smart_words = 0
        unmap w
        unmap b
    endif
endfunction
nnoremap <S-m> :call ToggleSmartWords()<CR>

command! GoToStartOfLine :normal 0
function! GetEqColumn()
    execute 'GoToStartOfLine'
    call search('=', 'W')
    return getpos('.')[2]
endfunction

let g:equals_column = 0
function! SetEqMarker()
    let g:equals_column = GetEqColumn()
    echo "Setting marker at" . g:equals_column
endfunction
nnoremap gn :call SetEqMarker()<CR>

command! -nargs=1 InsertSpaces :normal <Esc><args>i <Esc><Esc>
function! AlignEqualsSign()
    let l:eq_col = GetEqColumn()
    if g:equals_column > l:eq_col
        let l:diff = g:equals_column - l:eq_col
        echo "Inserting " . l:diff . " spaces"
        execute 'InsertSpaces' l:diff
    endif
endfunction
nnoremap mn :call AlignEqualsSign()<CR>
vnoremap al :normal mn<CR>

" Annoying to type C++ things
autocmd FileType cpp inoremap <C-t> template<><Esc>i
autocmd FileType cpp inoremap <C-y> typename
autocmd FileType cpp inoremap <C-k> #include
autocmd FileType cpp inoremap <C-c> struct {<CR>};<Esc>h%i

nnoremap dk "ddi(
nnoremap dq "ddi"
nnoremap dv "ddi[
nnoremap dj "ddi<

nnoremap ck "dci(
nnoremap cq "dci"
nnoremap cv "dci[
nnoremap cj "dci<

nnoremap yk "dyi(
nnoremap yq "dyi"
nnoremap yv "dyi[
nnoremap yj "dyi<

nnoremap tk vi("dp
nnoremap tq vi""dp
nnoremap tv vi]"dp
nnoremap tj vi>"dp

