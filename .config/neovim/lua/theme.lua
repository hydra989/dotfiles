local M = {}

function M.setup()
   -- colors
   vim.opt.background="dark"
   vim.cmd.colorscheme "oxocarbon"
   vim.cmd [[
    hi Normal ctermbg=16 guibg=#000000
    hi LineNr ctermbg=16 guibg=#000000
   ]];
end

return M
