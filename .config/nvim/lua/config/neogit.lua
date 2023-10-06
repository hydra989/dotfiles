local M = {}

function M.setup()
  local neogit = require('neogit')

  neogit.setup {
     use_magit_keybindings = true
  }
end

return M
