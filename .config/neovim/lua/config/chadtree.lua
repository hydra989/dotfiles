local M = {}

function M.setup()
  local chadtree = require('chadtree')
  vim.cmd [[
    nnoremap <leader>v <cmd>CHADopen<cr>
  ]]
end

return M
