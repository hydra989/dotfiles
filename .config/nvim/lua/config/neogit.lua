local M = {}

function M.setup()
  local neogit = require('neogit')

  neogit.setup {
     use_magit_keybindings = true,
     console_timout = 5000
  }
end

return M
