set encoding=utf-8
syntax on
filetype plugin indent on
let mapleader= ","

set nobackup
set nowritebackup
set noswapfile

" modeline cursor location
set ruler
" show partially complete commands
set showcmd
" incremental search
set incsearch
set laststatus=2
" autowrite before commands
set autowrite
" security precaution or the like
set modelines=0
set nomodeline
set wildmenu
" one space not two after punctuation
set nojoinspaces
set number
set splitbelow
set splitright
set mouse=a
set clipboard^=unnamed


" jump visual lines
nnoremap j gj
nnoremap k gk

" source pywal colors
source ~/.cache/wal/colors-wal.vim
