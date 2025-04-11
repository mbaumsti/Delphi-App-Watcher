(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_consts.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 2.0.0
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
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.
  - [07/03/2025] : v2.0 Adding  AppWatcherJsonFileName for new deployment features

  *******************************************************************************)

Unit AppWatcher_consts;

Interface


Const
    AppWPassword: String = 'appW';
    MsgNoInstanceActiveEn: String = '❌ [ERROR] This AppWatcher instance is not active.';
    MsgNoInstanceActiveFr: String = '❌ [ERREUR] Cette instance de AppWatcher n''est pas active.';
    MsgIniFileNotFoundEn: String = 'Configuration file "%s" not found.';
    MsgIniFileNotFoundFr: String = 'Fichier de configuration "%s"  introuvable.';
    AppWatcherIniFileNAme: String = 'AppWatcher.ini';
    LangFrIniFileName: String = 'AppWatcher_lang_fr.ini';
    LangEnIniFileName: String = 'AppWatcher_lang_en.ini';
    AppWatcherJsonFileName: String = 'AppWatcher.json';
    cPipeName : string = 'NamedPipesAppWatcher';

Implementation

End.

