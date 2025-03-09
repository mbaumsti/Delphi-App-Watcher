(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_AddFiles.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 07/03/2025
  Version : 2.0.0
  License : MIT

  Description :
  -------------
 This unit allows you to add or modify entries in the list of applications to be deployed.

  Features :
  -----------
- Select source file and destination directory

  Change Log :
  ------------
  - [07/03/2025] : Initial creation

  Note :
  -------
  This project is open-source. Contributions are welcome!
  *******************************************************************************)

Unit AppWatcherMaster_AddFile;

Interface

Uses
    Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.FileCtrl, dialogs,
    Winapi.CommDlg, Winapi.ShellAPI,AppWatcher_Lang;

Type
    TDlgAddFile = Class(TForm)
        OKBtn: TButton;
        CancelBtn: TButton;
        lblSource: TLabel;
        lblDest: TLabel;
        EditSource: TEdit;
        EditDest: TEdit;
        BtnBrowseSource: TSpeedButton;
        BtnBrowseDest: TSpeedButton;
        Procedure BtnBrowseSourceClick(Sender: TObject);
        Procedure BtnBrowseDestClick(Sender: TObject);
    private
                FLanguageManager: TAppLangManager;
    public
        Function Execute(Var Source, Dest: String;var LangManager : TAppLangManager): Boolean;
    End;

Var
    DlgAddFile: TDlgAddFile;

Implementation



{$R *.dfm}

procedure TDlgAddFile.BtnBrowseSourceClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Title := FLanguageManager.GetMessage('MASTER_DEPLOY', 'SELECT_SOURCE'); //'Sélectionnez le fichier source';
    OpenDlg.Filter := FLanguageManager.GetMessage('MASTER_DEPLOY', 'SOURCE_FILTER'); //'Exécutables (*.exe)|*.exe|Tous les fichiers (*.*)|*.*';

    if EditSource.Text <>'' then
    OpenDlg.InitialDir:=ExtractFilePath( EditSource.Text );

     if OpenDlg.Execute(self.Handle) then
      EditSource.Text := OpenDlg.FileName;

  finally
    OpenDlg.Free;
  end;
end;


Procedure TDlgAddFile.BtnBrowseDestClick(Sender: TObject);
Var
    Dir: String;
Begin
    Dir := EditDest.Text;
    If SelectDirectory(FLanguageManager.GetMessage('MASTER_DEPLOY', 'SELECT_DEST'), '', Dir) Then
        EditDest.Text := Dir;
End;

Function TDlgAddFile.Execute(Var Source, Dest: String;var LangManager : TAppLangManager): Boolean;
Begin
    FLanguageManager:=LangManager;

    EditSource.Text := Source;
    EditDest.Text := Dest;

    Result := ShowModal = mrOk;

    If Result Then Begin
        Source := EditSource.Text;
        Dest := EditDest.Text;
    End;
End;

End.

