{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,

  alsa-lib,
  at-spi2-atk,
  cairo,
  cups,
  dbus,
  expat,
  glib,
  gtk3,
  libdrm,
  libgbm,
  libGL,
  libx11,
  libxcb,
  libxcomposite,
  libxdamage,
  libxext,
  libxfixes,
  libxkbcommon,
  libxrandr,
  nspr,
  nss,
  pango,
  systemd,
  vulkan-loader,
}:

let
  version = "0.15.3";

  sources = {
    x86_64-linux = {
      target = "linux-x64";
      hash = "sha256-JE6ZW1VO2yn2bPB/f4MKdoha5qb6Iz3sU5gL+wpQlUE=";
    };
    aarch64-linux = {
      target = "linux-arm64";
      hash = "sha256-afJqdr3MYeR4JzOqG5/LbgZ3SsnWsN8+n7sKpKuw9rw=";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
      or (throw "terminal-browser: unsupported system ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "terminal-browser";
  inherit version;

  # prebuilt release: an Electron runtime plus bundled JS and a native renderer module
  src = fetchurl {
    url = "https://terminal-browser.sh/install/dl/stable/v${version}/terminal-browser-${source.target}.tar.gz";
    inherit (source) hash;
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    stdenv.cc.cc
    alsa-lib
    at-spi2-atk
    cairo
    cups
    dbus
    expat
    glib
    gtk3
    libdrm
    libgbm
    libxkbcommon
    nspr
    nss
    pango
    systemd
    libx11
    libxcb
    libxcomposite
    libxdamage
    libxext
    libxfixes
    libxrandr
  ];

  # Chromium dlopens these at runtime, so autoPatchelf cannot see them
  runtimeDependencies = [
    libGL
    systemd
    vulkan-loader
  ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/terminal-browser $out/bin
    cp -r . $out/share/terminal-browser

    # setuid is impossible in the store; Chromium falls back to user namespaces
    rm $out/share/terminal-browser/electron/chrome-sandbox

    # bin/terminal-browser resolves its own symlink to find the bundle root
    ln -s $out/share/terminal-browser/bin/terminal-browser $out/bin/terminal-browser

    runHook postInstall
  '';

  meta = {
    description = "A real browser inside your terminal";
    homepage = "https://github.com/zenbu-labs/terminal-browser";
    license = lib.licenses.mit;
    mainProgram = "terminal-browser";
    platforms = builtins.attrNames sources;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
