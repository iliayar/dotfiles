{ config, pkgs, ... }:

{
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
    };
  };

}
