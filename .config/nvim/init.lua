require("plugins").setup()

require("defaults").setup()
require("theme").setup()

-- individual plugin setups
require("config.alpha").setup()
require("config.leap").setup()
require("config.lualine").setup()
require("config.lspconfig").setup()
require("config.neogit").setup()
require("config.telescope").setup()
require("config.treesitter").setup()
