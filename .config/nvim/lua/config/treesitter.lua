local M = {}

function M.setup()
        treesitter = require('nvim-treesitter')
        treesitterconf = require('nvim-treesitter.configs')

        treesitterconf.setup {
                highlight = {
                        enable = true,
                }
        }
end

return M
