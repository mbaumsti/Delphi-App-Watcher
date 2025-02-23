(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_consts.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 23/02/2025
  Version  : 1.1
  License  : MIT

  Description :
  -------------
  This unit defines constants for the application

  Features :
  -----------
  - Minimal messages need for the application when ini file are not found

  Change Log :
  ------------
  - [22/02/2025] : Initial creation
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language


  *******************************************************************************)

unit AppWatcher_consts;

interface

const
    AppWPassword: string = 'appW';
    MsgNoInstanceActiveEn: string = '❌ [ERROR] This AppWatcher instance is ot active.';
    MsgNoInstanceActiveFr: string = '❌ [ERREUR] Cette instance de AppWatcher n''est pas active.';
    MsgIniFileNotFoundEn: string = 'Configuration file "%s" not found.';
    MsgIniFileNotFoundFr: string = 'Fichier de configuration "%s"  introuvable.';
    AppWatcherIniFileNAme: string = 'AppWatcher.ini';
    LangFrIniFileName: string = 'AppWatcher_lang_fr.ini';
    LangEnIniFileName: string = 'AppWatcher_lang_en.ini';

implementation

end.
