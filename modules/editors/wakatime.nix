{ config, pkgs, secrets, ... }:

{
  home.file.".wakatime.cfg".text = ''
    [settings]
    debug = false
    hidefilenames = false
    ignore =
        COMMIT_EDITMSG$
        PULLREQ_EDITMSG$
        MERGE_MSG$
        TAG_EDITMSG$
    api_key=${secrets.wakatime-api-key}
  '';
}
