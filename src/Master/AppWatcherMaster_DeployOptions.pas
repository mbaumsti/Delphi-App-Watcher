(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_DeployOptions.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 23/04/2025
  Version : 3.1
  License : MIT

  Description :
  -------------
  This unit allows you to modify backup rotation options at deployment time

  Features :
  -----------
  - Change the number of backups.
  - Enable or disable backup rotations.

  Change Log :
  ------------
  - [23/04/2025] : Initial creation

  Note :
  -------
  This project is open-source. Contributions are welcome!

*******************************************************************************)

Unit AppWatcherMaster_DeployOptions;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, AppWatcher_Lang, AppWatcherMaster_Backup;

Type

    TDlgDeployOptions = Class(TForm)
        ChkRotate: TCheckBox;
        SpinMax: TSpinEdit;
        LblMax: TLabel;
        BtnOK: TButton;
        BtnCancel: TButton;
        Procedure ChkRotateClick(Sender: TObject);
    private
        FLangMgr: TAppLangManager;
    public
        Function Execute(LangMgr: TAppLangManager; var Opt: TBackupSettings): Boolean;
    End;


Implementation

{$R *.dfm}

Procedure TDlgDeployOptions.ChkRotateClick(Sender: TObject);
Begin
    LblMax.Enabled := ChkRotate.Checked;
    SpinMax.Enabled := ChkRotate.Checked;
End;

Function TDlgDeployOptions.Execute(LangMgr: TAppLangManager; var Opt: TBackupSettings): Boolean;
Begin
    FLangMgr := LangMgr;
    Caption := FLangMgr.GetMessage('MASTER_DEPLOY_OPT', 'TITLE');
    ChkRotate.Caption := FLangMgr.GetMessage('MASTER_DEPLOY_OPT', 'CHK_ROTATE');
    LblMax.Caption := FLangMgr.GetMessage('MASTER_DEPLOY_OPT', 'LBL_MAX');
    BtnOK.Caption := FLangMgr.GetMessage('MASTER_DEPLOY_OPT', 'BTN_OK');
    BtnCancel.Caption := FLangMgr.GetMessage('MASTER_DEPLOY_OPT', 'BTN_CANCEL');

    ChkRotate.Checked := Opt.DoRotate;
    SpinMax.Value := Opt.MaxVersions;
    ChkRotateClick(Nil);

    Result := ShowModal = mrOk;
    If Result Then Begin
        Opt.DoRotate := ChkRotate.Checked;
        Opt.MaxVersions := SpinMax.Value;
    End;
End;

End.

