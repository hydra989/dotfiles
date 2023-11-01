local M = {}

function M.setup()
    require('nightfox').setup({
        palettes = {
            carbonfox = {
                bg1 = "#000000",
                bg0 = "#1d1d1d",
                bg3 = "#121820",
                se10 = "#121b24",
            },
        },
    })

    -- colors
    vim.opt.background = "dark"
    vim.cmd.colorscheme "oxocarbon"
end

return M
