program AppWatcherAgent;

uses
  Vcl.Forms,
  AppWatcherAgent_main in 'AppWatcherAgent_main.pas' {FormAppWatcher},
  AppWatcherAgent_Stop in 'AppWatcherAgent_Stop.pas' {FormStopNotification},
  AppWatcher_ioHandler in '..\Common\AppWatcher_ioHandler.pas',
  AppWatcher_Lang in '..\Common\AppWatcher_Lang.pas',
  AppWatcher_consts in '..\Common\AppWatcher_consts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormAppWatcher, FormAppWatcher);
  Application.Run;
end.
