(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_main.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 20/02/2025
  Version  : 1.0
  License  : MIT

  Description :
  -------------
  This is a **test client** for the AppWatcher project.
  It connects to the **AppWatcher Master** using the `TAppWatcherClient` component
  and listens for commands such as **STOP, WHO, and START**.

  Features :
  -----------
  - Displays received commands in a **memo log**
  - Responds to **STOP** requests
  - Provides application parameters when requested
  - Useful for debugging communication with the Master

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [15/02/2025] : Added support for `OnStopRequested` event
  - [19/02/2025] : Improved logging of received commands
  - [22/02/2025] : Adding StopRequested
  - [22/02/2025] : Replaced singleton `AppLangManager` with a local instance, allowing multiple instances of `TAppWatcherClient` to have different languages.

  Notes :
  -------
  This unit is a **test tool** and should not be used in production.
  Contributions and improvements are welcome!
  *******************************************************************************)

unit AppWatcherClient_main;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
    IdTCPConnection, IdTCPClient, Vcl.ExtCtrls, Vcl.StdCtrls,
    AppWatcherClient_Component, AppWatcher_ioHandler,
    AppWatcherClient_second;

type

    TFormAppWatcherClient = class(TForm)
        Memo1: TMemo;
        Button1: TButton;
    AppWatcherClient1: TAppWatcherClient;

        procedure AppWatcherClient1CommandReceived(Sender: TObject; const Command:
            TAppWatcherCommand);
        procedure AppWatcherClient1GetAppParams(Sender: TObject; var Params: string);
        procedure AppWatcherClient1StopRequested(Sender: TObject;
            var CanStop: Boolean);
        procedure Button1Click(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    private
        {Déclarations privées}

    public
        {Déclarations publiques}
    end;

var
    FormAppWatcherClient: TFormAppWatcherClient;

implementation

{$R *.dfm}


procedure TFormAppWatcherClient.AppWatcherClient1CommandReceived(Sender:
    TObject; const Command: TAppWatcherCommand);
begin
    Memo1.lines.add('Received :' + TAppWatcherStrCommand[ord(Command)]);
end;

procedure TFormAppWatcherClient.AppWatcherClient1GetAppParams(Sender: TObject;
    var Params: string);
begin
    Params := 'TEST';
end;

procedure TFormAppWatcherClient.AppWatcherClient1StopRequested(
    Sender: TObject; var CanStop: Boolean);
begin
    CanStop := true;
end;

procedure TFormAppWatcherClient.Button1Click(Sender: TObject);
begin
    FormAppWatcherClient2.showModal;
end;

procedure TFormAppWatcherClient.FormCloseQuery(Sender: TObject; var CanClose:
    Boolean);
begin
    if  AppWatcherClient1.CloseRequested then
    CanClose := true;

end;

end.
