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
  };

  outputs = { self, home-manager, nixpkgs, code-stats-vim, secrets, ... }: 
  let
    specialArgs = {
      inherit home-manager
      code-stats-vim;

      secrets = import secrets;
      themes = import ./modules/themes;
    };

    homeConfigurations = {
      iliayar = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        extraSpecialArgs = specialArgs;
        pkgs = import nixpkgs {
          inherit system;
        };
        homeDirectory = "/home/iliayar";
        username = "iliayar";
        configuration = { pkgs, config, ... }: {
          imports = [
            ./hosts/dellLaptop/home.nix
          ];

          nixpkgs.config = {
            allowUnfree = true;
          };
        };
      };
    };

    dellLaptop = 
      let
        system = "x86_64-linux";

        modules = [
          ./hosts/dellLaptop/configuration.nix
          {
            nixpkgs.config = {
              allowUnfree = true;
            };
          }
        ];
      in
        nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
  in {
    nixosConfigurations.nixos = dellLaptop;
    inherit homeConfigurations;
  };
}
