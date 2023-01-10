{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.email = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf (cfg.enable && cfg.email.enable) {
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.mu.enable = true;

    accounts.email = {
      maildirBasePath = "Mail";
      accounts = {
        personal = {
          address = "iliayar3@gmail.com";
          imap.host = "imap.gmail.com";
          gpg = {
            key = "0x3FE87CB13CB3AC4E";
            signByDefault = true;
          };
          mu.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            patterns = [ 
              "*" 
              "![Gmail]*" 
              "[Gmail]/Sent Mail" 
              "[Gmail]/Important" 
              "[Gmail]/Drafts" 
              "[Gmail]/Trash" 
            ];
          };
          msmtp.enable = true;
          primary = true;
          realName = "Ilya Yaroshevskiy";
          passwordCommand = "pass google.com/iliayar3@gmail.com";
          smtp.host = "smtp.gmail.com";
          userName = "iliayar3@gmail.com";
          signature = {
            text = ''
          '';
            showSignature = "append";
          };
        };
        cock = {
          address = "iliayar@cock.li";
          imap.host = "mail.cock.li";
          gpg = {
            key = "0x3FE87CB13CB3AC4E";
            signByDefault = true;
          };
          mu.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            patterns = [ 
              "*" 
            ];
          };
          msmtp.enable = true;
          realName = "Ilya Yaroshevskiy";
          passwordCommand = "pass cock.li/iliayar";
          smtp.host = "mail.cock.li";
          userName = "iliayar@cock.li";
          signature = {
            text = ''
          '';
            showSignature = "append";
          };
        };
        university = {
          address = "iliayar@niuitmo.ru";
          imap.host = "outlook.office365.com";
          gpg = {
            key = "0x3FE87CB13CB3AC4E";
            signByDefault = true;
          };
          mu.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            patterns = [ 
              "*" 
            ];
          };
          msmtp.enable = true;
          realName = "Ilya Yaroshevskiy";
          passwordCommand = "pass outlook/iliayar@niuitmo.ru";
          smtp.host = "smtp-mail.outlook.com";
          userName = "iliayar@niuitmo.ru";
          signature = {
            text = ''
          '';
            showSignature = "append";
          };
        };
      };
    };
  };
}
