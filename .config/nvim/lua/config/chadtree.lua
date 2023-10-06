local M = {}

function M.setup()
  require('chadtree')
  vim.cmd [[
    nnoremap <leader>v <cmd>CHADopen<cr>
  ]]
end

return M
