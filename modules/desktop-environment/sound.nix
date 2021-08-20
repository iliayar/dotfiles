{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ 
    pulseaudio
    pulsemixer
  ];
}
