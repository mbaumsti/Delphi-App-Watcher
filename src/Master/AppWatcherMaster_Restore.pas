
(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_Restore.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 22/04/2025
  Version : 3.1
  License : MIT

  Description :
  -------------
  This unit allows you to restore a previously saved executable file

  Features :
  -----------
  - Recovering a previous version

  Change Log :
  ------------
  - [22/04/2025] : Initial creation

  Note :
  -------
  This project is open-source. Contributions are welcome!
  *******************************************************************************)

Unit AppWatcherMaster_Restore;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
    Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls, System.IOUtils, System.Types,
    AppWatcher_consts, AppWatcher_Lang, Vcl.Controls,
    system.DateUtils, dialogs, AppWatcherMaster_Backup;

Type
    TDlgRestore = Class(TForm)
        VersionsList: TListView;
        BtnRestore: TButton;
        BtnClose: TButton;
        Procedure FormShow(Sender: TObject);
        Procedure BtnRestoreClick(Sender: TObject);
    private
        FMainExe: String;
        FSettings: TBackupSettings;
        FLangMgr: TAppLangManager;
        Procedure ReloadList;
    public
        Function Execute(Const ADestFile: String;
            Const ASettings: TBackupSettings;
            ALangMgr: TAppLangManager): Boolean;
    End;

Implementation

{$R *.dfm}

Procedure TDlgRestore.FormShow(Sender: TObject);
Begin
    ReloadList;
End;

Function CompareBackupItems(Item1, Item2: TListItem; ParamSort: Integer): Integer; stdcall;
Begin
    Result := CompareDateTime(
        StrToDateTimeDef(Item2.SubItems[0], 0), // ⇩ tri décroissant
        StrToDateTimeDef(Item1.SubItems[0], 0));
End;

Procedure TDlgRestore.ReloadList;
Var
    Pattern, Dir: String;
    SR: TSearchRec;
    It: TListItem;
Begin
    VersionsList.Items.Clear;

    Dir := FSettings.GetFolder(FMainExe);
    Pattern := TPath.GetFileNameWithoutExtension(FMainExe) + '_*' +
    TPath.GetExtension(FMainExe);

    If FindFirst(TPath.Combine(Dir, Pattern), faAnyFile, SR) = 0 Then Try
            Repeat
                It := VersionsList.Items.Add;
                It.Caption := SR.Name;
                It.SubItems.Add(DateTimeToStr(FileDateToDateTime(SR.Time))); // col.0
                It.SubItems.Add(FormatFloat('#,##0', SR.Size) + ' o');
            Until FindNext(SR) <> 0;
        Finally
            FindClose(SR);
        End;
    VersionsList.CustomSort(@CompareBackupItems, 0);
End;

Procedure TDlgRestore.BtnRestoreClick(Sender: TObject);
Var
    BackupPath: String;
Begin
    If Not Assigned(VersionsList.Selected) Then Exit;

    BackupPath := TPath.Combine(FSettings.GetFolder(FMainExe), VersionsList.Selected.Caption);

    If MessageDlg(Format(FLangMgr.GetMessage('MASTER_RESTORE', 'MSG_REPLACE_CONFIRM'),
        [FMainExe, BackupPath]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes Then Begin


        BackupWithTimestamp(FMainExe, FSettings);
        Try
            TFile.Copy(BackupPath, FMainExe, True); // restauration
        Except
            On E: Exception Do Begin
                ShowMessage(Format(FLangMgr.GetMessage('MASTER_RESTORE', 'MSG_COPYERROR'), [FMainExe, E.Message]));

            End;
        End;
        ModalResult := mrOk;
    End;
End;

Function TDlgRestore.Execute(Const ADestFile: String; Const ASettings: TBackupSettings; ALangMgr: TAppLangManager): Boolean;
Begin
    FMainExe := ADestFile;
    FSettings := ASettings;
    FSettings.DoRotate := False; // On applique pas de rotation sur la récupération
    FLangMgr := ALangMgr;

    VersionsList.Columns[0].Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'COL_NAME');
    VersionsList.Columns[1].Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'COL_DATE');
    VersionsList.Columns[2].Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'COL_SIZE');

    Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'TITLE');
    BtnRestore.Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'BTN_RESTORE');
    BtnClose.Caption := FLangMgr.GetMessage('MASTER_RESTORE', 'BTN_CLOSE');
    Result := ShowModal = mrOk;
End;

End.

