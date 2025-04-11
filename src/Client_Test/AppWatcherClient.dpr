program AppWatcherClient;

uses
  Vcl.Forms,
  AppWatcherClient_main in 'AppWatcherClient_main.pas' {FormAppWatcherClient},
  AppWatcherClient_second in 'AppWatcherClient_second.pas' {FormAppWatcherClient2},
  AppWatcher_ioHandler in '..\Common\AppWatcher_ioHandler.pas',
  AppWatcher_Lang in '..\Common\AppWatcher_Lang.pas',
  AppWatcherClient_Component in '..\Client_Component\AppWatcherClient_Component.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormAppWatcherClient, FormAppWatcherClient);
  Application.CreateForm(TFormAppWatcherClientSecond, FormAppWatcherClientSecond);
  Application.Run;
end.
