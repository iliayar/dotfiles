{ self, config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [];
  
  nix.settings.experimental-features = "nix-command flakes";
  
  system.configurationRevision = self.rev or self.dirtyRev or null;
  
  system.stateVersion = 6;
  
  nixpkgs.hostPlatform = "aarch64-darwin";
}
