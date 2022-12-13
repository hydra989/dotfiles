{ inputs, ... }:
{
    programs.alacritty = {
      enable = true;
      settings = {
        font = {
          normal.family = "terminus";
          bold.family = "terminus";
          italic.family = "terminus";
          bold_italic.family = "terminus";
          size = 10.0;
        };
        window = {
          decorations = "none";
          opacity = 0.9;
        };
        shell = {
          program = "zsh";
          args = [
            "--login"
            "-c"
            "tmux"
          ];
        };
      };
    };
}