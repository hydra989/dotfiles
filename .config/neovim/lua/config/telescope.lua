local M = {}

function M.setup()
    telescope = require('telescope')
    -- telescope.load_extension('emoji')

    vim.cmd [[
        nnoremap <leader>ff <cmd>Telescope find_files hidden=true<cr>
        nnoremap <leader>fg <cmd>Telescope live_grep<cr>
        nnoremap <leader>fb <cmd>Telescope buffers<cr>
        nnoremap <leader>fh <cmd>Telescope help_tags<cr>
    ]]
end

return M
