{
  appimageTools,
  fetchurl,
  makeWrapper,
}: let
  pname = "msty";
  version = "1.7";

  src = fetchurl {
    url = "https://assets.msty.app/prod/latest/linux/amd64/Msty_x86_64_amd64.AppImage";
    sha256 = "sha256-4NjS9/ZlzFWyVHA054DmpHeTl35PgkPiHwgRjHeB4is=";
  };
  appimageContents = appimageTools.extractType2 {inherit pname version src;};
in
  appimageTools.wrapType2 {
    inherit pname version src;

    nativeBuildInputs = [makeWrapper];

    extraInstallCommands = ''
      install -m 444 -D ${appimageContents}/msty.desktop -t $out/share/applications
      substituteInPlace $out/share/applications/msty.desktop \
              --replace 'Exec=AppRun' 'Exec=${pname}'
      install -m 444 -D ${appimageContents}/msty.png \
              $out/share/icons/hicolor/256x256/apps/msty.png
      wrapProgram $out/bin/${pname} \
              --set XDG_CURRENT_DESKTOP GNOME
    '';
  }
