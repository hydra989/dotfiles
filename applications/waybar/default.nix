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
                    "clock"
                ];


                "clock" = {
                    interval = 60;
                    format = "{:%H:%M}";
                };
                "tray"= {
                    icon-size = 16;
                    spacing = 5;
                };
            };
        };

        style = ./style.css;
    };
}
