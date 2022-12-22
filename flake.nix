{ 
  description = "My NixOs Configuration using home-manager with flakes";


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = github:nix-community/NUR;

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    code-stats-vim = {
      url = "gitlab:code-stats/code-stats-vim";
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

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    libxft-bgra = {
      url = "github:uditkarode/libxft-bgra";
      flake = false;
    };

    xmonad-newest = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-contrib-newest = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    org-roam-ui = {
      url = "github:org-roam/org-roam-ui";
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
      url = github:iliayar/rust-blocks;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self
            , home-manager
            , nixpkgs
            , code-stats-vim
            , secrets
            , emacs-overlay
            , libxft-bgra
            , xmonad-contrib-newest
            , xmonad-newest
            , org-roam-ui
            , picom-jonaburg
            , wakatime-cli
            , zsh-wakatime
            , tlpui-src
            , rust-blocks
            , nur
            , ...
            }
    @inputs: 
      let
        system = "x86_64-linux";

        overlays = [
          emacs-overlay.overlay
        ] ++ (import ./modules/overlays (inputs // { inherit system; }));

        nixpkgs-config = {
          inherit system overlays;
          config.allowUnfree = true;
        };

        pkgs = import nixpkgs nixpkgs-config;

        mylib = import ./modules/lib.nix { inherit pkgs; };

        wallpapers = import ./modules/themes/wallpapers.nix { inherit pkgs mylib; };

        themes = import ./modules/themes { inherit mylib; };

        specialArgs = {
          inherit
            home-manager
            code-stats-vim
            libxft-bgra
            xmonad-contrib-newest
            xmonad-newest
            org-roam-ui
            picom-jonaburg
            wakatime-cli
            zsh-wakatime
            wallpapers
            mylib
            tlpui-src
            system
          ;

          secrets = import secrets;
          themes = themes;
        };

        homeConfigurations = {
          heavy = home-manager.lib.homeManagerConfiguration rec {
            inherit pkgs;
            extraSpecialArgs = specialArgs // {
              inherit pkgs system;
            };
            modules = [
              ./profiles/heavy.nix
            ];
          };

          wsl = home-manager.lib.homeManagerConfiguration rec {
            inherit pkgs;
            extraSpecialArgs = specialArgs // {
              inherit pkgs system;
            };
            modules = [
              ./hosts/wsl/home.nix
            ];
          };
        };

        dellLaptop = 
          let
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
          in
            nixpkgs.lib.nixosSystem {
              inherit
                system
                modules
                specialArgs;
            };

        lenovoLaptop = 
          let
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
          in
            nixpkgs.lib.nixosSystem {
              inherit
                system
                modules
                specialArgs;
            };
      in {
        nixosConfigurations.NixLaptop = dellLaptop;
        nixosConfigurations.NixLenovo = lenovoLaptop;
        inherit homeConfigurations;
      };
}
