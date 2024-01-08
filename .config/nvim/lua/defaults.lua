local M = {}

function M.setup()
    vim.cmd [[
        if !has('gui_running') && &term =~ '^\%(screen\|tmux\)'
            let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
            let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
        endif

        set encoding=utf-8
        set termguicolors
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
        set shiftround
        set cmdheight=0

        nnoremap j gj
        nnoremap k gk

        nnoremap <silent> ff <cmd>lua vim.lsp.buf.format()<CR>
        nnoremap <leader>v <cmd>CHADopen<cr>
    ]]
end

return M
