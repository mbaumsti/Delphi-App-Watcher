Program AppWatcherMaster;

uses
  Vcl.Forms,
  AppWatcher_ioHandler in '..\Common\AppWatcher_ioHandler.pas',
  AppWatcher_Lang in '..\Common\AppWatcher_Lang.pas',
  AppWatcher_consts in '..\Common\AppWatcher_consts.pas',
  AppWatcherMaster_main in 'AppWatcherMaster_main.pas' {FormAppWatcherMaster},
  AppWatcherMaster_Deploy in 'AppWatcherMaster_Deploy.pas' {FormDeployManager},
  Vcl.Themes,
  Vcl.Styles,
  AppWatcherMaster_AddFile in 'AppWatcherMaster_AddFile.pas' {DlgAddFile},
  AppWatcherMaster_Restore in 'AppWatcherMaster_Restore.pas' {DlgRestore},
  AppWatcherMaster_Backup in 'AppWatcherMaster_Backup.pas',
  AppWatcherMaster_DeployOptions in 'AppWatcherMaster_DeployOptions.pas' {DlgDeployOptions};

{$R *.res}

Begin
    Application.Initialize;
    Application.MainFormOnTaskbar := true;
    Application.ModalPopupMode := pmExplicit;
    TStyleManager.TrySetStyle('Wedgewood Light');
    Application.CreateForm(TFormAppWatcherMaster, FormAppWatcherMaster);
    Application.Run;
End.

