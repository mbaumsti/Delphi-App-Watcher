(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_Lang.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 1.2
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
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved FindConfigPath to support .lnk shortcuts when searching for configuration files.

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



function ResolveShortcut(const LnkFile: string): string;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  WidePath: array[0..MAX_PATH] of WideChar;
  FindData: TWin32FindData;
begin
  Result := '';

  CoInitialize(nil);
  try
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
    PersistFile := ShellLink as IPersistFile;

    if Succeeded(PersistFile.Load(PWideChar(WideString(LnkFile)), STGM_READ)) then
    begin
      if Succeeded(ShellLink.GetPath(WidePath, MAX_PATH, FindData, 0)) then
        Result := WidePath;
    end;
  finally
    CoUninitialize;
  end;
end;

function FindConfigPath(const IniFileName: string): string;
var
  CurrentPath, FullPath, FileName, ResolvedPath: string;
  Files: TArray<string>;
begin
  Result := '';
  CurrentPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  while True do
  begin
    // 🔹 Chercher directement le fichier .ini
    FullPath := CurrentPath + IniFileName;
    if FileExists(FullPath, True) then
    begin
      Result := FullPath;
      Exit;
    end;

    // 🔹 Vérifier les raccourcis dans le dossier
    Files := TDirectory.GetFiles(CurrentPath, '*.lnk', TSearchOption.soTopDirectoryOnly);
    for FileName in Files do
    begin
      ResolvedPath := ResolveShortcut(FileName);
      if FileExists(ResolvedPath, True) and SameText(ExtractFileName(ResolvedPath), IniFileName) then
      begin
        Result := ResolvedPath;
        Exit;
      end;
    end;

    // 🔹 Vérifier si on est déjà à la racine du disque
    if (CurrentPath = ExtractFileDrive(CurrentPath) + PathDelim) or (CurrentPath = PathDelim) then
      Break;

    // 🔹 Remonter d'un cran
    CurrentPath := ExpandFileName(CurrentPath + '..\');
    CurrentPath := IncludeTrailingPathDelimiter(CurrentPath);
  end;

  // 📌 Dernier recours : dossier %APPDATA%
  FullPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + 'AppWatcher\Config\' + IniFileName;
  if FileExists(FullPath, True) then
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
