{ pkgs, ... }:
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "betterbird";
  version = "153.2.0esr-bb8";

  src = pkgs.fetchurl {
    url = "https://www.betterbird.eu/downloads/LinuxArchive/betterbird-${finalAttrs.version}-latest-build7.en-US.linux-x86_64.tar.xz";
    hash = "sha256-EjNYjnRhi4wwNhxQSbjHmohiqolvAUS10Y5Oa8BPM/0=";
  };

  nativeBuildInputs = with pkgs; [
    autoPatchelfHook
    patchelfUnstable
    wrapGAppsHook3
  ];

  buildInputs = with pkgs; [
    alsa-lib
  ];

  # Thunderbird uses "relrhack" to manually process relocations from a fixed offset
  patchelfFlags = [ "--no-clobber-old-sections" ];

  strictDeps = true;

  postPatch = ''
    # Don't download updates from Mozilla directly
    echo 'pref("app.update.auto", "false");' >> defaults/pref/channel-prefs.js
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$prefix/usr/lib/betterbird-bin-${finalAttrs.version}"
    cp -r * "$prefix/usr/lib/betterbird-bin-${finalAttrs.version}"

    mkdir -p "$out/bin"
    ln -s "$prefix/usr/lib/betterbird-bin-${finalAttrs.version}/betterbird" "$out/bin/"

    # wrapThunderbird expects "$out/lib" instead of "$out/usr/lib"
    ln -s "$out/usr/lib" "$out/lib"

    runHook postInstall
  '';

  meta = {
    changelog = "https://www.betterbird.net/en-US/betterbird/${finalAttrs.version}/releasenotes/";
    description = "Betterbird is a fine-tuned version of Mozilla Thunderbird, Thunderbird on steroids, if you will.";
    homepage = "https://www.betterbird.eu";
    mainProgram = "betterbird";
    sourceProvenance = with pkgs.lib.sourceTypes; [ binaryNativeCode ];
    license = pkgs.lib.licenses.mpl20;
    maintainers = with pkgs.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
})
