{ 
  description = "My NixOs Configuration using home-manager with flakes";


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    code-stats-vim = {
      url = "gitlab:code-stats/code-stats-vim";
      flake = false;
    };

    secrets.url = "path:./modules/secrets";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    libxft-bgra = {
      url = "github:uditkarode/libxft-bgra";
      flake = false;
    };

    xmonad-newest = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    my-xmonad-contrib = {
      url = "github:iliayar/xmonad-contrib/feature/fallback-fonts";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    systec-can = {
      url = "https://www.systec-electronic.com/fileadmin/Redakteur/Unternehmen/Support/Downloadbereich/Treiber/systec_can-V1.0.3.tar.bz2";
      flake = false;
    };

    org-roam-ui = {
      url = "github:org-roam/org-roam-ui";
      flake = false;
    };

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };
  };

  outputs = { self
            , home-manager
            , nixpkgs
            , code-stats-vim
            , secrets
            , emacs-overlay
            , libxft-bgra
            , my-xmonad-contrib
            , xmonad-newest
            , org-roam-ui
            , picom-jonaburg
            , ...
            }
    @inputs: 
      let
        system = "x86_64-linux";

        overlays = [
          emacs-overlay.overlay
        ] ++ (import ./modules/overlays inputs);

        nixpkgs-config = {
          inherit system overlays;
          config.allowUnfree = true;
        };

        pkgs = import nixpkgs nixpkgs-config;

        specialArgs = {
          inherit
            home-manager
            code-stats-vim
            libxft-bgra
            my-xmonad-contrib
            xmonad-newest
            org-roam-ui
            picom-jonaburg
          ;

          secrets = import secrets;
          themes = import ./modules/themes { lib = pkgs.lib; };
        };

        homeConfigurations = {
          iliayar = home-manager.lib.homeManagerConfiguration rec {
            inherit system;
            extraSpecialArgs = specialArgs // {
              inherit pkgs system;
              wallpapers = (pkgs.callPackage (import ./modules/themes/wallpapers.nix) { });
            };
            homeDirectory = "/home/iliayar";
            username = "iliayar";
            configuration = { pkgs, config, ... }: {
              imports = [
                ./hosts/dellLaptop/home.nix
              ];
              nixpkgs = nixpkgs-config;
            };
          };
        };

        dellLaptop = 
          let
            modules = [
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
      in {
        nixosConfigurations.NixLaptop = dellLaptop;
        inherit homeConfigurations;
      };
}
