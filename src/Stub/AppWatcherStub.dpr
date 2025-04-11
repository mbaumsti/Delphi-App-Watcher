(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherStub.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 09/04/2025
  Version  : 3.0.0
  License  : MIT

  Description :
  -------------
  Minimal utility program used to restart the AppWatcher agent.
  Intended for scenarios such as agent replacement or updates, this stub connects
  to the master server, waits for a `RESTART_AGENT` command, and relaunches the
  target executable with optional parameters.

  Features :
  -----------
  - TCP connection to master server
  - Passive wait for restart message
  - Automatic relaunch of executable with parameters
  - Uses shared message handler for structured communication

  Change Log :
  ------------
  - [09/04/2025] : Initial creation

  Notes :
  -------
  This stub is designed to be lightweight and temporary. It can be used as a
  transitional tool during updates without disrupting the monitoring cycle.
  This project is **open-source**. Contributions and improvements are welcome!
*******************************************************************************)



program AppWatcherStub;

{$APPTYPE GUI}


uses
    SysUtils,
    IdTCPClient,
    IdGlobal,
    Windows,
    ShellAPI,
    AppWatcher_ioHandler in '..\Common\AppWatcher_ioHandler.pas';

procedure RestartAgent(const ExePath, Params: string);
begin
    ShellExecute(0, 'open', PChar(ExePath), PChar(Params), nil, SW_SHOWNORMAL);
end;

procedure RunStub(const MasterHost: string; MasterPort: Integer; const ExePath, Params: string);
var
    Client: TIdTCPClient;

    MsgReceived: TAppWatcherMessage;
begin
    Client := TIdTCPClient.Create(nil);
    try
        Client.Host := MasterHost;
        Client.Port := MasterPort;

        //Writeln('Connecting to master at ', MasterHost, ':', MasterPort);
        Client.ConnectTimeout := 3000;
        Client.ReadTimeout := 0;
        Client.Connect;

        //Writeln('Connected. Waiting for command...');

        while True do
        begin
            //Command := Client.IOHandler.ReadLn;
            if ReadMessage(Client.IOHandler, MsgReceived) then begin

                if MsgReceived.Command = cmdRESTART_AGENT then
                begin
                    //Writeln('Restart command received. Launching: ', ExePath, ' ', Params);
                    RestartAgent(ExePath, Params);
                    Break;
                end;
            end;
            sleep(100);
        end;

    finally
            Client.Free;
    end;

end;

procedure ShowUsage;
begin
    //Writeln('Usage: AppWatcherStub <MasterIP> <Port> <ExePath> <Params>');
end;

begin
    try
        if ParamCount < 4 then
        begin
            ShowUsage;
            Exit;
        end;

        RunStub(ParamStr(1), StrToInt(ParamStr(2)), ParamStr(3), ParamStr(4));

    except
        //on E: Exception do
        //Writeln(E.ClassName, ': ', E.Message);
    end;

end.
