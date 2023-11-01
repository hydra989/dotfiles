{ ... }:
{
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 24;

        modules-left = [
          "hyprland/workspaces"
        ];
        modules-center = [

        ];
        modules-right = [
          "tray"
          "battery"
          "pulseaudio"
          "clock"
          "custom/swaync"
          "custom/wlogout"
        ];


        "battery" = {
          states = {
            warning = 20;
            critical = 10;
          };
          format = "{icon} {capacity}%";
          format-icons = [
            " "
            " "
            " "
            " "
            " "
          ];
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          tooltip = false;
          spacing = 10;
          interval = 3;
          icon-size = 16;
        };

        "clock" = {
          interval = 60;
          format = "{:%H:%M}";
          spacing = 10;
        };

        "hyprland/workspaces" = {
          format = "{icon}";
          format-icons = {
            "1" = "  ₁";
            "2" = "󰆍  ₂";
            "3" = "  ₃";
            "4" = "  ₄";
            "5" = "󰎞  ₅";
            "6" = "  ₆";
            "7" = "  ₇";
            "8" = "  ₈";
          };
          persistent-workspaces = {
            "*" = 8;
          };
          icon-size = 16;
        };

        "pulseaudio" = {
          format = "{icon} {volume}%";
          format-alt = "{icon}";
          format-alt-click = "click-right";
          format-icons = {
            headphone = [ " " " " " " " " ];
            default = [ "" "" "" "" ];
          };
          tooltip = false;
          icon-size = 18;
        };

        "tray" = {
          icon-size = 16;
          spacing = 4;
          reverse-direction = true;
        };

        "custom/swaync" = {
          tooltip = true;
          format = " {icon}";
          format-icons = {
            "notification" = " ";
            "none" = " ";
            "dnd-notification" = " ";
            "dnd-none" = " ";
            "inhibited-notification" = " ";
            "inhibited-none" = " ";
            "dnd-inhibited-notification" = " ";
            "dnd-inhibited-none" = " ";
          };
          return-type = "json";
          exec-if = "which swaync-client";
          exec = "swaync-client -swb";
          on-click = "swaync-client -t -sw";
          on-click-right = "swaync-client -d -sw";
          escape = true;
        };

        "custom/wlogout" = {
          format = " ⏻   ";
          on-click = "wlogout";
        };
      };
    };

    style = ./style.css;
  };
}
