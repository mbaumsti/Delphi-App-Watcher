(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_Lang.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 3.1
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
  - [25/02/2025] : v1.2.1 - Fixed missing `Config\` folder check in `FindConfigPath`
  - [23/04/2025] : v3.1 Introduction of the `TAWStringArray` type for Multi-version compatibility:
                            - In Delphi 12+: alias to `TArray<string>`
                            - In Delphi ≤ 11: alias to `TStringDynArray`

  Notes :
  -------
  This project is **open-source**. Contributions and improvements are welcome!
  *******************************************************************************)

Unit AppWatcher_Lang;

Interface

Uses
    System.SysUtils, System.IniFiles, System.Classes, Vcl.Forms, Windows, ActiveX, ComObj, ShlObj,
    AppWatcher_consts;

Type

    {$IF CompilerVersion >= 36} // Delphi 12 Athens = 36.x
        TAWStringArray = TArray<String>;
    {$ELSE}
        TAWStringArray = TStringDynArray;
    {$ENDIF}

    TAppWatcherLang = (langFr, langEn);

    TAppLangManager = Class
    private
        FIniFIleLang: TMemIniFile;
        FLanguage: TAppWatcherLang;
        Procedure SetLanguage(Value: TAppWatcherLang);
    public
        Constructor Create(ALanguage: TAppWatcherLang);
        Destructor Destroy; override;
        Function LoadLanguage(ALanguage: TAppWatcherLang): boolean;
        Function GetMessage(Const Section, Key: String): String;
        Property Language: TAppWatcherLang Read FLanguage Write SetLanguage;
    End;

Function FindConfigPath(Const IniFileName: String): String;

Implementation

Uses
    System.IOUtils;

{TAppLangManager}

Constructor TAppLangManager.Create(ALanguage: TAppWatcherLang);
Begin
    //Ne pas charger la langue ici, éviter les erreurs en mode conception
    FLanguage := ALanguage;
End;

Destructor TAppLangManager.Destroy;
Begin
    FreeAndNil(FIniFIleLang);
    Inherited;
End;

Procedure TAppLangManager.SetLanguage(Value: TAppWatcherLang);
Begin
    If FLanguage <> Value Then Begin
        LoadLanguage(Value); //Charge la nouvelle langue
    End;
End;

Function ResolveShortcut(Const LnkFile: String): String;
Var
    ShellLink: IShellLink;
    PersistFile: IPersistFile;
    WidePath: Array[0..MAX_PATH] Of WideChar;
    FindData: TWin32FindData;
Begin
    Result := '';

    CoInitialize(Nil);
    Try
        ShellLink := CreateComObject(CLSID_ShellLink) As IShellLink;
        PersistFile := ShellLink As IPersistFile;

        If Succeeded(PersistFile.Load(PWideChar(WideString(LnkFile)), STGM_READ)) Then Begin
            If Succeeded(ShellLink.GetPath(WidePath, MAX_PATH, FindData, 0)) Then
                Result := WidePath;
        End;
    Finally
        CoUninitialize;
    End;
End;

Function FindConfigPath(Const IniFileName: String): String;
Var
    CurrentPath, FullPath, ConfigPath, FileName, ResolvedPath: String;
    Files: TAWStringArray;
Begin
    Result := '';
    CurrentPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

    While True Do Begin
        // 🔹 Chercher directement le fichier .ini
        FullPath := CurrentPath + IniFileName;
        If FileExists(FullPath, True) Then Begin
            Result := FullPath;
            Exit;
        End;

        // 🔹 Vérifier dans le sous-dossier Config
        ConfigPath := CurrentPath + 'Config\' + IniFileName;
        If FileExists(ConfigPath, True) Then Begin
            Result := ConfigPath;
            Exit;
        End;

        // 🔹 Vérifier les raccourcis dans le dossier courant
        Files := TDirectory.GetFiles(CurrentPath, '*.lnk', TSearchOption.soTopDirectoryOnly);
        For FileName In Files Do Begin
            ResolvedPath := ResolveShortcut(FileName);
            If FileExists(ResolvedPath, True) And SameText(ExtractFileName(ResolvedPath), IniFileName) Then Begin
                Result := ResolvedPath;
                Exit;
            End;
        End;

        // 🔹 Vérifier les raccourcis dans le dossier Config
        If DirectoryExists(CurrentPath + 'Config\') Then Begin
            Files := TDirectory.GetFiles(CurrentPath + 'Config\', '*.lnk', TSearchOption.soTopDirectoryOnly);
            For FileName In Files Do Begin
                ResolvedPath := ResolveShortcut(FileName);
                If FileExists(ResolvedPath, True) And SameText(ExtractFileName(ResolvedPath), IniFileName) Then Begin
                    Result := ResolvedPath;
                    Exit;
                End;
            End;
        End;

        // 🔹 Vérifier si on est déjà à la racine du disque
        If (CurrentPath = ExtractFileDrive(CurrentPath) + PathDelim) Or (CurrentPath = PathDelim) Then
            Break;

        // 🔹 Remonter d'un cran
        CurrentPath := ExpandFileName(CurrentPath + '..\');
        CurrentPath := IncludeTrailingPathDelimiter(CurrentPath);
    End;

    // 📌 Dernier recours : dossier %APPDATA%
    FullPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + 'AppWatcher\Config\' + IniFileName;
    If FileExists(FullPath, True) Then
        Result := FullPath;
End;

Function TAppLangManager.LoadLanguage(ALanguage: TAppWatcherLang): boolean;
Var
    LangFileName: String;
Begin
    FLanguage := ALanguage;
    Result := false;

    //🔹 Sélection du bon fichier selon la langue
    Case FLanguage Of
        langFr:
            LangFileName := LangFrIniFileName;
        langEn:
            LangFileName := LangEnIniFileName;
    End;

    //🔹 Trouver dynamiquement le chemin du fichier
    LangFileName := FindConfigPath(LangFileName);

    If (LangFileName <> '') Then Begin
        //🔹 Libération de l'ancien fichier INI
        FreeAndNil(FIniFIleLang);
        //🔹 Chargement du fichier
        FIniFIleLang := TMemIniFile.Create(LangFileName, TEncoding.UTF8);
        Result := True;
    End;
End;

Function TAppLangManager.GetMessage(Const Section, Key: String): String;
Begin
    If Assigned(FIniFIleLang) Then
        Result := FIniFIleLang.ReadString(Section, Key, '[' + Section + '].' + Key)
    Else
        Result := '[' + Section + '].' + Key;

    //🔹 Supprime les guillemets s'il y en a
    Result := AnsiDequotedStr(Result, '"');

    //🔹 Remplace \n par des sauts de ligne réels
    Result := StringReplace(Result, '\n', sLineBreak, [rfReplaceAll]);
End;

End.

