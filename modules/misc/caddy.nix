{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.custom.misc.caddy;

  virtualHost =
    name: host:
    ''
      http://${name} {
        log
    ''
    + optionalString (host.allow != null) (
      "  @denied not remote_ip ${concatStringsSep " " host.allow}\n"
      + "  respond @denied \"Forbidden\" 403\n"
    )
    + ''
        reverse_proxy ${host.upstream}
      }
    '';

  caddyfile = pkgs.writeText "Caddyfile" (
    ''
      {
        admin unix/{$XDG_RUNTIME_DIR}/caddy/admin.sock
        auto_https off
        persist_config off
      }
    ''
    + concatStrings (mapAttrsToList virtualHost cfg.virtualHosts)
    + ''
      :80 {
        log
        respond "Unknown host" 404
      }
    ''
  );

  caddy-route = pkgs.writers.writePython3Bin "caddy-route" {
    flakeIgnore = [ "E501" ];
  } (builtins.readFile ./caddy-route.py);
in
{
  options = {
    custom.misc.caddy = {
      enable = mkOption {
        default = false;
      };

      virtualHosts = mkOption {
        default = { };
        type = types.attrsOf (
          types.submodule {
            options = {
              upstream = mkOption {
                type = types.str;
              };
              allow = mkOption {
                default = null;
                type = types.nullOr (types.listOf types.str);
              };
            };
          }
        );
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ caddy-route ];

    systemd.user.services.caddy = {
      Unit.Description = "Caddy reverse proxy";
      Service = {
        RuntimeDirectory = "caddy";
        RuntimeDirectoryMode = "0700";
        ExecStart = "${pkgs.caddy}/bin/caddy run --adapter caddyfile --config ${caddyfile}";
        ExecStartPost = "${caddy-route}/bin/caddy-route restore";
        Restart = "always";
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
