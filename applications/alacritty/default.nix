{ ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal.family = "Hack";
        bold.family = "Hack";
        italic.family = "Hack";
        bold_italic.family = "Hack";
        size = 8.0;
      };
      window = {
        decorations = "none";
        opacity = 0.9;
      };
      shell = {
        program = "zsh";
        args = [ "--login" ];
      };
    };
  };
}
