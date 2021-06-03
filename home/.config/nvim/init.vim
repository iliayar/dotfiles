set clipboard=unnamedplus

let mapleader = "\<Space>"

let g:rainbow_active = 1

" set number
set rnu

set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab

set guifont=Fira\ Code:h13

" Display these characters
set list listchars=tab:\ ,eol:\ 

set foldmethod=indent
set nofoldenable

filetype plugin on

" Plugins here
call plug#begin()

" Fzf search plugin
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Info line
" Plug 'itchyny/lightline.vim'
Plug 'vim-airline/vim-airline'

" Surround text with brackets
Plug 'tpope/vim-surround'

" Git marks
Plug 'airblade/vim-gitgutter'

" Configs for projects
Plug 'editorconfig/editorconfig-vim'

" File tree
Plug 'scrooloose/nerdtree'

" Rainbow brackets yey
Plug 'kien/rainbow_parentheses.vim'

" Commenter
Plug 'preservim/nerdcommenter'

" Gruvbox
Plug 'morhetz/gruvbox'

" pywal
Plug 'dylanaraps/wal.vim'

" Improved search
Plug 'easymotion/vim-easymotion'

" Code stats
Plug 'https://gitlab.com/code-stats/code-stats-vim.git'

" Rust
Plug 'rust-lang/rust.vim'

call plug#end()

source ~/.config/nvim/private.vim
let g:codestats_api_key = codestats_key

" Airline code::stats
let g:airline_section_x = airline#section#create_right(['tagbar', 'filetype', '%{CodeStatsXp()}'])

" Comments
let g:NERDSpaceDelims = 1

" Gruvbox colorscheme
" autocmd VimEnter * colorscheme gruvbox

" Monokai colorscheme
colorscheme monokai

" Pywal
" colorscheme wal

" Transparent background
autocmd VimEnter * hi Normal guibg=NONE ctermbg=NONE

" Rainbow brackets
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

nnoremap <ESC> :noh<CR>
nnoremap <leader>op :NERDTreeToggle<CR>
nnoremap <leader>e :new COMMAND_OUTPUT<CR>:read !

map! <S-Down> <Down>
map! <S-Up> <Up>
map <S-Down> <Down>
map <S-Up> <Up>

map <leader>gs <Plug>(easymotion-sn)

" Copy to system clipboard whem exit
autocmd VimLeave * call system("xsel -ib", getreg('+'))
