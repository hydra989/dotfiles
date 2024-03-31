{ ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal.family = "Hack";
        bold.family = "Hack";
        italic.family = "Hack";
        bold_italic.family = "Hack";
        size = 10.0;
      };
      shell = {
        program = "zsh";
        args = [ "--login" ];
      };
    };
  };
}
