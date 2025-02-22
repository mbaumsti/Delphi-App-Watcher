(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_Lang.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 20/02/2025
  Version  : 1.0
  License  : MIT

  Description :
  -------------
  This unit manages **multi-language support** for the AppWatcher project.
  It loads language-specific messages from **INI files** and provides
  a function to retrieve localized strings dynamically.

  Features :
  -----------
  - Loads language files from `AppWatcher_lang_fr.ini` and `AppWatcher_lang_en.ini`
  - Provides **dynamic access** to localized messages
  - Supports **on-the-fly language switching**
  - Automatically replaces `\n` with real line breaks
  - Uses `TMemIniFile` for efficient reading

  Change Log :
  ------------
  - [06/02/2025] : Initial creation
  - [15/02/2025] : Improved error handling when loading language files
  - [17/02/2025] : Added automatic removal of surrounding quotes in messages
  - [21/02/2025] : Added FindConfigPath
  - [22/02/2025] : use of AppWatcher_consts
  - [22/02/2025] : Deleted singleton AppLangManager. All applications must be modified to use a local instance.

  Notes :
  -------
  This project is **open-source**. Contributions and improvements are welcome!
  *******************************************************************************)

unit AppWatcher_Lang;

interface

uses
    System.SysUtils, System.IniFiles, System.Classes, Vcl.Forms, Windows, ActiveX, ComObj, ShlObj,
    AppWatcher_consts;

type
    TAppWatcherLang = (langFr, langEn);

    TAppLangManager = class
    private
        FIniFIleLang: TMemIniFile;
        FLanguage:    TAppWatcherLang;
        procedure SetLanguage(Value: TAppWatcherLang);
    public
        constructor Create(ALanguage: TAppWatcherLang);
        destructor Destroy; override;
        Function LoadLanguage(ALanguage: TAppWatcherLang): boolean;
        function GetMessage(const Section, Key: string): string;
        property Language: TAppWatcherLang read FLanguage write SetLanguage;
    end;

function FindConfigPath(const IniFileName: string): string;

implementation

uses
    System.IOUtils;

{TAppLangManager}

constructor TAppLangManager.Create(ALanguage: TAppWatcherLang);
begin
    //Ne pas charger la langue ici, éviter les erreurs en mode conception
    FLanguage := ALanguage;
end;

destructor TAppLangManager.Destroy;
begin
    FreeAndNil(FIniFIleLang);
    inherited;
end;

procedure TAppLangManager.SetLanguage(Value: TAppWatcherLang);
begin
    if FLanguage <> Value then
    begin
        LoadLanguage(Value); //Charge la nouvelle langue
    end;
end;

function FindConfigPath(const IniFileName: string): string;
var
    CurrentPath, ConfigPath, FullPath: string;
begin
    Result := '';
    CurrentPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

    while True do
    begin
        //🔹 Vérifier directement dans le répertoire courant
        FullPath := CurrentPath + IniFileName;
        if FileExists(FullPath) then
        begin
            Result := FullPath;
            Exit;
        end;

        //🔹 Vérifier dans le sous-dossier Config
        ConfigPath := CurrentPath + 'Config\' + IniFileName;
        if FileExists(ConfigPath) then
        begin
            Result := ConfigPath;
            Exit;
        end;

        //🔹 Vérifier si on est déjà à la racine du disque
        if (CurrentPath = ExtractFileDrive(CurrentPath) + PathDelim) or (CurrentPath = PathDelim) then
            Break; //On arrête si on est arrivé à la racine

        //🔹 Remonter d'un cran
        CurrentPath := ExpandFileName(CurrentPath + '..\');
        CurrentPath := IncludeTrailingPathDelimiter(CurrentPath);
    end;

    //📌 Dernier recours : dossier %APPDATA%
    FullPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + 'AppWatcher\Config\' + IniFileName;
    if FileExists(FullPath) then
        Result := FullPath;
end;

Function TAppLangManager.LoadLanguage(ALanguage: TAppWatcherLang): boolean;
var
    LangFileName: string;
begin
    FLanguage := ALanguage;
    Result := false;

    //🔹 Sélection du bon fichier selon la langue
    case FLanguage of
        langFr:
            LangFileName := LangFrIniFileName;
        langEn:
            LangFileName := LangEnIniFileName;
    end;

    //🔹 Trouver dynamiquement le chemin du fichier
    LangFileName := FindConfigPath(LangFileName);

    if (LangFileName <> '') then
    begin
        //🔹 Libération de l'ancien fichier INI
        FreeAndNil(FIniFIleLang);
        //🔹 Chargement du fichier
        FIniFIleLang := TMemIniFile.Create(LangFileName, TEncoding.UTF8);
        Result := True;
    end;
end;

function TAppLangManager.GetMessage(const Section, Key: string): string;
begin
    if Assigned(FIniFIleLang) then
        Result := FIniFIleLang.ReadString(Section, Key, '[' + Section + '].' + Key)
    else
        Result := '[' + Section + '].' + Key;

    //🔹 Supprime les guillemets s'il y en a
    Result := AnsiDequotedStr(Result, '"');

    //🔹 Remplace \n par des sauts de ligne réels
    Result := StringReplace(Result, '\n', sLineBreak, [rfReplaceAll]);
end;

end.
