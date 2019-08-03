" by noptrix & elken

" Preamble ---------------------------------------------------------------- {{{

set nocompatible               " be iMproved
filetype off                   " required!
set runtimepath+=~/.vim/bundle/neobundle.vim/

 call neobundle#begin(expand('/home/iliayar/.vim/bundle/'))
" }}}
" Basic options ----------------------------------------------------------- {{{

" file type and syntax highliting on
filetype plugin indent on
syntax on

" specific settings
set nocursorline
set title

set list
set listchars=
set listchars+=tab:>\ 
set listchars+=trail:·
set listchars+=extends:»
"set listchars+=space:⋅
set listchars+=precedes:«
set listchars+=nbsp:⣿
let g:indentLine_leadingSpaceEnabled = 1
let g:indentLine_leadingSpaceChar = '·'

set autoindent
set ruler
set shortmess=aoOTI
set showmode
set splitbelow
set splitright
set laststatus=2
set nomodeline
set showcmd
set showmatch
set noexpandtab
set tabstop=1
set shiftwidth=1
set expandtab
set cinoptions=(0,m1,:1
set formatoptions=tcqr2
set laststatus=2
set nomodeline
set clipboard=unnamed
set softtabstop=4
set showtabline=1
set smartcase
set sidescroll=5
set scrolloff=4
set hlsearch
set ttyfast
set history=10000
set hidden
set number
set backspace=indent,eol,start
set ttimeoutlen=100
" USE TABS INSTEAD OF SPACES
" set noexpandtab
" set copyindent
" set preserveindent
" set softtabstop=0
" set shiftwidth=4
" set tabstop=4
"  CONSTIL`

" Better completion
set complete=.,w,b,u,t
set completeopt=longest,menuone,preview

" Leader
let mapleader = ","
let maplocalleader = "\\"

" Cursorline {{{
" Only show cursorline in the current window and in normal mode.

augroup cline
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" }}}

" Line Return {{{

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}

" Backups {{{

set backup                        " enable backups
set noswapfile                    " it's 2013, Vim.

set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

" }}}

" Color scheme {{{
set background=dark
colorscheme blackarch
hi Normal guibg=NONE ctermbg=NONE
hi NonText guibg=NONE ctermbg=NONE
hi Function guibg=NONE ctermbg=NONE
hi Statement guibg=NONE ctermbg=NONE
hi Special guibg=NONE ctermbg=NONE
hi SpecialChar guibg=NONE ctermbg=NONE
hi Constant guibg=NONE ctermbg=NONE
hi Comment guibg=NONE ctermbg=NONE
hi Preproc guibg=NONE ctermbg=NONE
hi Type guibg=NONE ctermbg=NONE
hi Identifier guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
" }}}
" Statusline {{{
set statusline=
set statusline+=%7*\[%n]                                  "buffernr
set statusline+=%1*\ %<%F\                                "File+path
set statusline+=%2*\ %y\                                  "FileType
set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..)
set statusline+=%5*\ %{&spelllang}\                       "Spellanguage
set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
set statusline+=%9*\ col:%03c\                            "Colnr
set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly?  Top/bot.


" }}}
" Folding {{{
set foldlevelstart=0

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za
" }}}
" }}}
" Mappings ---------------------------------------------------------------- {{{
" Easy buffer navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

noremap <leader>v <C-w>v

" Quick editing {{{
nnoremap <leader>ev :vsplit ~/.vimrc<cr>

" }}}
" }}}
" Plugin settings --------------------------------------------------------- {{{
" NeoBundle {{{
NeoBundleFetch 'Shougo/neobundle.vim'

"Github
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc', {'build' : {'unix' : 'make -f make_unix.mal',},}
" }}}
" NERD Tree {{{
call neobundle#end()

noremap  <F2> :NERDTreeToggle<cr>
inoremap <F2> <esc>:NERDTreeToggle<cr>

augroup ps_nerdtree
     au!

     au Filetype nerdtree setlocal nolist
augroup END

let NERDTreeHighlightCursorline = 1
let NERDTreeIgnore = ['.vim$', '\~$', '.*\.pyc$', 'pip-log\.txt$', 'whoosh_index',
                                 \ 'xapian_index', '.*.pid', 'monitor.py', '.*-fixtures-.*.json',
                                  \ '.*\.o$', 'db.db', 'tags.bak', '.*\.pdf$', '.*\.mid$',
                                  \ '.*\.midi$']

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDChristmasTree = 1
let NERDTreeChDirMode = 2
let NERDTreeMapJumpFirstChild = 'gK'

" }}}
" Tagbar {{{
nnoremap <F9> :TagbarToggle<CR>
" }}}
" Unite {{{
nnoremap <C-s> :Unite file_rec/async<cr>
nnoremap <leader>/ :Unite grep:.<cr>
let g:unite_source_history_yank_enable = 1
nnoremap <leader>y :Unite history/yank<cr>
nnoremap <leader>b :Unite -quick-match buffer<cr>
" }}}
" }}}
