set encoding=utf-8
syntax on
filetype plugin indent on
let mapleader= ","

set nobackup
set nowritebackup
set noswapfile

set ruler			" modeline cursor location
set showcmd			" show partially complete commands
set incsearch		" incremental search
set laststatus=2
set autowrite		" autowrite before commands
set modelines=0		" security precaution or the like
set nomodeline
set wildmenu
set nojoinspaces	" one space not two after punctuation
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
