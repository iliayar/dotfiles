{
  description = "My NixOs Configuration using home-manager with flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    code-stats-vim = {
      url = "gitlab:iliayar/codestats.nvim";
      flake = false;
    };

    wakatime-cli = {
      url = "github:wakatime/wakatime-cli";
      flake = false;
    };

    zsh-wakatime = {
      url = "github:wbingli/zsh-wakatime";
      flake = false;
    };

    secrets.url = "git+ssh://git@github.com/iliayar/dotfiles-secrets.git";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    libxft-bgra = {
      url = "github:uditkarode/libxft-bgra";
      flake = false;
    };

    org-roam-ui = {
      url = "github:org-roam/org-roam-ui";
      flake = false;
    };

    lean4-mode = {
      url = "github:leanprover-community/lean4-mode";
      flake = false;
    };

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };

    tlpui-src = {
      url = "github:d4nj1/TLPUI";
      flake = false;
    };

    rust-blocks = {
      url = "github:iliayar/rust-blocks";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uci = {
      url = "git+ssh://git@github.com/iliayar/uci.git";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    pyprland-newest = {
      url = "github:hyprland-community/pyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyprland-my = {
      url = "github:iliayar/pyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    anyrun = {
      url = "github:Kirottu/anyrun";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nwg-displays = {
      url = "github:nwg-piotr/nwg-displays";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    denv = {
      url = "github:iliayar/env.nix";
      # url = "path:/home/iliayar/Repos/env.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wezterm-newest = {
      url = "github:wez/wezterm?dir=nix";
      flake = true;

      # NOTE: This is vital. To match GL versions,
      # because wezterm links to its own libGL, but
      inputs.nixpkgs.follows = "nixpkgs";
    };

    obsidian-nvim = {
      url = "github:epwalsh/obsidian.nvim";
      flake = false;
    };

    remote-nvim = {
      url = "github:amitds1997/remote-nvim.nvim";
      # url = "path:/home/iliayar/Repos/remote-nvim.nvim";
      flake = false;
    };
  };

  outputs = { self, home-manager, nixpkgs, code-stats-vim, secrets
    , emacs-overlay, libxft-bgra, org-roam-ui, picom-jonaburg, wakatime-cli
    , zsh-wakatime, tlpui-src, rust-blocks, nur, flake-utils, uci, anyrun
    , nwg-displays, rust-overlay, denv, wezterm-newest, pyprland-newest
    , obsidian-nvim, lean4-mode, pyprland-my, remote-nvim, ... }@inputs:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [ x86_64-linux x86_64-darwin ]) (system:
      let
        overlays = [ emacs-overlay.overlay rust-overlay.overlays.default ]
          ++ (import ./modules/overlays (inputs // { inherit system; }));

        nixpkgs-config = {
          inherit system overlays;
          config.allowUnfree = true;
          config.permittedInsecurePackages = [ "electron-25.9.0" ];
        };

        pkgs = import nixpkgs nixpkgs-config;

        mylib = import ./modules/lib.nix { inherit pkgs; };

        themes = import ./modules/themes { inherit mylib; };

        specialArgs = {
          inherit home-manager code-stats-vim libxft-bgra org-roam-ui
            picom-jonaburg wakatime-cli zsh-wakatime mylib tlpui-src system
            anyrun wezterm-newest pyprland-newest obsidian-nvim lean4-mode
            pyprland-my remote-nvim;

          secrets = import secrets;
          themes = themes;
        };

        makeHomeProfileImpl = profile:
          home-manager.lib.homeManagerConfiguration rec {
            inherit pkgs;
            extraSpecialArgs = specialArgs // { inherit pkgs system; };
            modules = [
              denv.homeManagerModules.default
              anyrun.homeManagerModules.default
              ./modules
              profile
            ];
          };

        makeHomeProfile = name: makeHomeProfileImpl ./profiles/${name}.nix;

        makeProfiles =
          pkgs.lib.foldl (acc: name: acc // { ${name} = makeHomeProfile name; })
          { };

        homeConfigurations =
          (makeProfiles [ "heavy" "ubuntu-virt" "work" "work-linux" ]) // {
            wsl = home-manager.lib.homeManagerConfiguration rec {
              inherit pkgs;
              extraSpecialArgs = specialArgs // { inherit pkgs system; };
              modules = [ ./hosts/wsl/home.nix ];
            };
          };

        dellLaptop = let
          modules = [
            nur.nixosModules.nur
            ./hosts/dellLaptop/configuration.nix
            ./cachix.nix
            {
              nixpkgs = nixpkgs-config;
              nix = {
                gc = {
                  automatic = true;
                  options = "--delete-older-than 3d";
                };
              };
            }
          ];
        in nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

        lenovoLaptop = let
          modules = [
            ./hosts/lenovoLaptop/configuration.nix
            ./cachix.nix
            {
              nixpkgs = nixpkgs-config;
              nix = {
                gc = {
                  automatic = true;
                  options = "--delete-older-than 3d";
                };
              };
            }
          ];
        in nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

        homeSrv = let
          modules = [
            ./hosts/homeSrv/configuration.nix
            ./cachix.nix
            {
              nixpkgs = nixpkgs-config;
              nix = {
                gc = {
                  automatic = true;
                  options = "--delete-older-than 3d";
                };
              };
            }
          ];
        in nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
      in {
        packages = {
          nixosConfigurations.NixLaptop = dellLaptop;
          nixosConfigurations.NixLenovo = lenovoLaptop;
          nixosConfigurations.NixServer = homeSrv;
          inherit homeConfigurations;
        };

        makeHomeConfiguration = makeHomeProfileImpl;
      });
}
