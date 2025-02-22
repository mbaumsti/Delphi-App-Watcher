program AppWatcherMaster;

uses
  Vcl.Forms,
  AppWatcherMaster_main in 'AppWatcherMaster_main.pas' {FormAppWatcherMaster},
  AppWatcher_ioHandler in '..\Common\AppWatcher_ioHandler.pas',
  AppWatcher_Lang in '..\Common\AppWatcher_Lang.pas',
  AppWatcher_consts in '..\Common\AppWatcher_consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormAppWatcherMaster, FormAppWatcherMaster);
  Application.Run;
end.
