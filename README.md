A simple tool I made in 30 minutes because I was tired of manually downloading
these wallpapers. Compiles to a statically linked binary with Nix. Intended to
be used as a systemd timer. The Nix snippet can be found below:
``` nix
systemd.timers."wallpaper-tool" = {
  wantedBy = ["timers.target"];
  timerConfig = {
    OnCalendar = "*-*-1 00:00:00";
    AccuracySec = "1h";
    Persistent = true;
    Unit = "wallpaper-tool.service";
  };
};

systemd.services."wallpaper-tool" = {
  script = ''
    set -eu
    ${inputs.cajun-wallpaper-tool}/bin/cajun-kriegs-wallpaper cajun
    ${pkgs.feh}/bin/feh --bg-scale ${config.users.users.cajun.home}/Pictures/wallpaper.jpg
  '';

  serviceConfig = {
    Type = "oneshot";
    User = "cajun";
  };
};
```
