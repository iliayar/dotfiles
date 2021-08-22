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

    xmonad-newest.url = "github:xmonad/xmonad";

    my-xmonad-contrib = {
      url = "github:iliayar/xmonad-contrib/feature/fallback-fonts";
    };

    systec-can = {
      url = "https://www.systec-electronic.com/fileadmin/Redakteur/Unternehmen/Support/Downloadbereich/Treiber/systec_can-V1.0.3.tar.bz2";
      flake = false;
    };
  };

  outputs = { self, home-manager, nixpkgs, code-stats-vim, secrets, emacs-overlay, libxft-bgra, my-xmonad-contrib, xmonad-newest, ... }@inputs: 
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
          xmonad-newest;

        # my-xmonad-contrib = pkgs.haskell.lib.overrideCabal (my-xmonad-contrib.defaultPackage."x86_64-linux") (drv: {
        #   configureFlags = [ "-fuse_xft" ];
        # });

        secrets = import secrets;
        themes = import ./modules/themes;
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
              {
                nixpkgs = nixpkgs-config;
              }
            ];
          };
        };
      };

      dellLaptop = 
        let
          modules = [
            ./hosts/dellLaptop/configuration.nix
            {
              nixpkgs = nixpkgs-config;
              nix = {
                binaryCaches = [
                  "https://cache.nixos.org"
                  "https://nix-community.cachix.org"
                  "https://nixpkgs.cachix.org"
                ];
                binaryCachePublicKeys = [
                  "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                  "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                  "nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE="
                ];
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
