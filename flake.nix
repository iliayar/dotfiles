{ 
  description = "My NixOs Configuration using home-manager with flakes";


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # theme = {
    #   background = "#1c1e1f";
    #   foreground = "#f8f8f2";
    #   color1     = "#e3276b";
    #   color2     = "#a6e22e";
    #   color3     = "#f4bf75";
    # };
  };

  outputs = { self, home-manager, nixpkgs, ... }: 
  let
    specialArgs = {
      inherit home-manager;
    };

    dellLaptop = 
      let
        system = "x86_64-linux";

        modules = [
          ./hosts/dellLaptop/configuration.nix
          home-manager.nixosModules.home-manager
          {
            nixpkgs.config = {
              allowUnfree = true;
            };
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.iliayar = ./hosts/dellLaptop/home.nix;
              extraSpecialArgs = specialArgs;
            };
          }
        ];
      in
        nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
  in {
    nixosConfigurations.nixos = dellLaptop;
  };
}
