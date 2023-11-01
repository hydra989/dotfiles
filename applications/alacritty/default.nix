{ ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal.family = "JetBrains Mono";
        bold.family = "JetBrains Mono";
        italic.family = "JetBrains Mono";
        bold_italic.family = "JetBrains Mono";
        size = 10.0;
      };
      window = {
        decorations = "none";
        opacity = 0.95;
      };
      shell = {
        program = "zsh";
        #args = [ "-l" "-c" "tmux attach || tmux" ];
      };
      env = {
        term = "alacritty-direct";
      };

      # oxocarbon-alacritty, tweaked for darker background

      colors = {
        primary = {
            background = "#000000";
            foreground = "#ffffff";
        };
        search = {
            matches = {
                foreground = "CellBackground";
                background = "#ee5396";
            };
            footer_bar = {
                background = "#000000";
                foreground = "#ffffff";
            };
        };
        normal = {
            black = "#000000";
            magenta = "#ff7eb6";
            green = "#42be65";
            yellow = "#ffe97b";
            blue = "#33b1ff";
            red = "#ee5396";
            cyan = "#3ddbd9";
            white = "#dde1e6";
        };
        bright = {
            black = "#393939";
            magenta = "#ff7eb6";
            green = "#42be65";
            yellow = "#ffe97b";
            blue = "#33b1ff";
            red = "#ee5396";
            cyan = "#3ddbd9";
            white = "#ffffff";
        };
      };
    };
  };
}
