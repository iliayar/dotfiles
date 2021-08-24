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
        passwordCommand = "pass cock.li/iliayar@cock.li";
        smtp.host = "mail.cock.li";
        userName = "iliayar@cock.li";
        signature = {
          text = ''
          '';
          showSignature = "append";
        };
      };
    };
  };

}
