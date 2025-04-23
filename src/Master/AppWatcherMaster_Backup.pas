(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_Backup.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 22/04/2025
  Version : 3.1
  License : MIT

  Description :
  -------------
  This unit allows you to save executable files with a timestamp

  Features :
  -----------
  - Backup with timeStamp

  Change Log :
  ------------
  - [22/04/2025] : Initial creation

  Note :
  -------
  This project is open-source. Contributions are welcome!

*******************************************************************************)

Unit AppWatcherMaster_Backup;

Interface

Uses
    system.SysUtils, System.types, System.Classes, System.IOUtils, System.DateUtils, system.IniFiles;

Type
    TBackupSettings = Record
        MaxVersions: Integer;
        Folder: String; // '' = même dossier
        DoRotate: Boolean;
        /// <summary>
          /// Retourne le dossier de sauvegarde à utiliser.
          /// Si Folder est vide, on renvoie le dossier du fichier de référence.
          /// </summary>
        Function GetFolder(Const ReferenceFile: String): String;
    End;

Procedure BackupWithTimestamp(Const ADestFile: String; Const Settings: TBackupSettings);
Function LoadBackupSettings: TBackupSettings;

Implementation

Uses
    AppWatcher_Lang, AppWatcher_consts;

Function LoadBackupSettings: TBackupSettings;
Var
    ini: TIniFile;
    FIniFilePathName: String;
Begin
    FIniFilePathName := FindConfigPath(AppWatcherIniFileName);
    Result.MaxVersions := 0;
    Result.Folder := '';
    If FIniFilePathName <> '' Then Begin
        ini := TIniFile.Create(FIniFilePathName);
        Try
            Result.MaxVersions := ini.ReadInteger('Backup', 'MaxVersions', 0);
            Result.DoRotate:=Result.MaxVersions>0;
            Result.Folder := ini.ReadString('Backup', 'Folder', '');
        Finally
            ini.Free;
        End;
    End;
End;

Function TBackupSettings.GetFolder(Const ReferenceFile: String): String;
Begin
    If Folder <> '' Then
        Result := Folder
    Else
        Result := ExtractFilePath(ReferenceFile);
End;

Function CompareFileCreation(List: TStringList; Index1, Index2: Integer): Integer;
Begin
    Result := CompareDateTime(
        TFile.GetCreationTime(List[Index1]),
        TFile.GetCreationTime(List[Index2]));
End;

Procedure BackupWithTimestamp(Const ADestFile: String; Const Settings: TBackupSettings);
Var
    Stamp, BackupDir, BackupFile: String;
    Files:  TAWStringArray;
    SL: TStringList;
    i: Integer;
Begin
    If Not FileExists(ADestFile) Or (Settings.MaxVersions = 0) Then
        Exit; // rien à sauvegarder

    // ▸ Dossier de sauvegarde
    BackupDir := Settings.GetFolder(ADestFile);

    ForceDirectories(BackupDir);

    // Copie de l’exe actuel avec horodatage
    Stamp := FormatDateTime('yyyymmdd_hhnnss', Now);
    BackupFile := TPath.Combine(
        BackupDir,
        TPath.GetFileNameWithoutExtension(ADestFile) +
        '_' + Stamp +
        TPath.GetExtension(ADestFile));
    TFile.Copy(ADestFile, BackupFile, False); // NE PAS écraser une sauvegarde existante

    // Rotation
    If (Settings.MaxVersions > 0) And (Settings.DoRotate) Then Begin
        Files := TDirectory.GetFiles(
            BackupDir,
            TPath.GetFileNameWithoutExtension(ADestFile) + '_*' +
            TPath.GetExtension(ADestFile),
            TSearchOption.soTopDirectoryOnly);

        SL := TStringList.Create;
        Try
            For i := 0 To High(Files) Do
                SL.Add(Files[i]);

            // trier par date de création croissante (plus vieux d'abord)
            SL.CustomSort(@CompareFileCreation);

            While SL.Count > Settings.MaxVersions Do Begin
                TFile.Delete(SL[0]); // supprime le plus ancien
                SL.Delete(0);
            End;
        Finally
            SL.Free;
        End;
    End;
End;



//==========================================================================================================

End.

