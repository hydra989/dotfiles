local M = {}

function M.setup()
    vim.cmd [[
        let g:tex_flavor = 'latex'
        let g:vimtex_view_method = 'zathura'
        set conceallevel=1
        let g:vimtex_quickfix_mode=0
        let g:tex_conceal = 'abdmg'
    ]]
end

return M
