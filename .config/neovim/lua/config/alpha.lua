local M = {}

function M.setup()
  local alpha = require('alpha')
  local dashboard = require('alpha.themes.dashboard')

  dashboard.section.header.val = {
     "            :h-                                  Nhy`               ",
     "           -mh.                           h.    `Ndho               ",
     "           hmh+                          oNm.   oNdhh               ",
     "          `Nmhd`                        /NNmd  /NNhhd               ",
     "          -NNhhy                      `hMNmmm`+NNdhhh               ",
     "          .NNmhhs              ```....`..-:/./mNdhhh+               ",
     "           mNNdhhh-     `.-::///+++////++//:--.`-/sd`               ",
     "           oNNNdhhdo..://++//++++++/+++//++///++/-.`                ",
     "      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:       ",
     " .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+        ",
     " h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`         ",
     " hd+` -NNNy`./dNNNNNhhhh+-://///    -+oo:`  ::-:+////++///:`        ",
     " /Nmhs+oss-:++/dNNNmhho:--::///    /mmmmmo  ../-///++///////.       ",
     "  oNNdhhhhhhhs//osso/:---:::///    /yyyyso  ..o+-//////////:/.      ",
     "   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.     ",
     "     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`    ",
     "       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:    ",
     "       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`   ",
     "       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:   ",
     "       //+++//++++++////+++///::--                 .::::-------::   ",
     "       :/++++///////////++++//////.                -:/:----::../-   ",
     "       -/++++//++///+//////////////               .::::---:::-.+`   ",
     "       `////////////////////////////:.            --::-----...-/    ",
     "        -///://////////////////////::::-..      :-:-:-..-::.`.+`    ",
     "         :/://///:///::://::://::::::/:::::::-:---::-.-....``/- -   ",
     "           ::::://::://::::::::::::::----------..-:....`.../- -+oo/ ",
     "            -/:::-:::::---://:-::-::::----::---.-.......`-/.      ``",
     "           s-`::--:::------:////----:---.-:::...-.....`./:          ",
     "          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`           ",
     "         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-              ",
     "        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`                ",
     "                        .-:mNdhh:.......--::::-`                    ",
     "                           yNh/..------..`                          ",
     "                                                                    ",
  }

  dashboard.section.buttons.val = {
    dashboard.button( "e", "  > New file" , ":ene <BAR> startinsert <CR>"),
    dashboard.button( "f", "🔍 > Find file", ":Telescope find_files hidden=true<CR>"),  
    dashboard.button( "q", "🔌 > Quit NVIM", ":qa<CR>"),
  }

  alpha.setup(dashboard.opts)

  -- "Disable folding on alpha buffer"
  vim.cmd([[
    autocmd FileType alpha setlocal nofoldenable
  ]])
end

return M
