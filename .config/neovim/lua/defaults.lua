local M = {}

function M.setup()
  vim.cmd [[
    set encoding=utf-8
    syntax on
    filetype plugin indent on
    let mapleader=" "

    set nobackup
    set nowritebackup
    set noswapfile
    set ruler
    set incsearch
    set laststatus=2
    set modelines=0
    set nomodeline
    set wildmenu
    set nojoinspaces
    set splitbelow
    set splitright
    set clipboard=unnamed
    set number relativenumber
    set expandtab
    set shiftwidth=4 smarttab
    set tabstop=8 softtabstop=0
    set autoindent
    set backspace=indent,eol,start

    nnoremap j gj
    nnoremap k gk
  ]]
end

return M
