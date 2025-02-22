(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_Component.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 20/02/2025
  Version  : 1.0
  License  : MIT

  Description :
  -------------
  This unit defines a **client component** for communicating with the AppWatcher Master.
  It allows applications to send and receive **STOP, WHO, START, and CANCEL** commands.

  Features :
  -----------
  - **TCP connection management** with automatic reconnection
  - **Receives commands** and executes corresponding actions
  - Events for handling commands:
  - `OnCommandReceived` → Triggered when a command is received
  - `OnStopRequested` → Triggered before stopping an application
  - `OnGetAppParams` → Retrieves application parameters

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [19/02/2025] : Improved error handling for network failures
  - [22/02/2025] :
  Property Port and Interval moved in inifile
  TAppWatcherClient.Loaded modified for loading inifile
  New property CloseRequested to test in your onclosing query of your applications
  use of AppWatcher_consts
  - [22/02/2025] : Replaced the singleton AppLangManager with a local instance to allow multiple instances.

  *******************************************************************************)

unit AppWatcherClient_Component;

interface

uses
    System.Classes, System.SysUtils, IdTCPClient, IdTCPConnection, IdComponent, IdGlobal,
    IdBaseComponent, Vcl.ExtCtrls, Vcl.StdCtrls, AppWatcher_ioHandler,
    Vcl.Forms, AppWatcher_Lang, Vcl.Controls, dialogs, System.IniFiles, Winapi.Windows, Winapi.Messages,
    AppWatcher_consts;

type
    TOnCommandReceived = procedure(Sender: TObject; const Command: TAppWatcherCommand) of object;
    TOnStopRequested = procedure(Sender: TObject; var CanStop: Boolean) of object;
    TOnGetAppParams = procedure(Sender: TObject; var Params: string) of object;

    TAppWatcherClient = class(TComponent)
    private
        FIdTCPClient:          TIdTCPClient;
        FTimer:                TTimer;
        FMemo:                 TMemo;
        FLang:                 TAppWatcherLang;
        FLastCommand:          string;
        FOnCommandReceived:    TOnCommandReceived;
        FOnStopRequested:      TOnStopRequested;
        FOnGetAppParams:       TOnGetAppParams;
        FPort:                 Integer;
        FInterval:             Integer;
        FLastReconnectAttempt: TDateTime;
        FSignalErrorConnect:   Boolean;
        FRequestingClose:      Boolean;
        FIniFileNotFOund:      Boolean;
        FIsOnMainForm:         Boolean;
        FLanguageManager:      TAppLangManager;
        procedure TimerEvent(Sender: TObject);
        procedure SetMemo(Value: TMemo);
        procedure SetPort(Value: Integer);
        procedure SetInterval(Value: Integer);
        procedure CheckForCommands;
        procedure SetLang(Value: TAppWatcherLang);
        procedure WriteMsgFormat(const Section, Msg: string; const Args: array of const);
        procedure WriteMsg(const Msg: string);
        Function InstanceActive: Boolean;

    protected
        FIsFirstLoad: Boolean; //Permet de savoir si c'est le premier chargement
        procedure Loaded; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Connect;
        procedure Disconnect;
        property LastCommand: String read FLastCommand;
    published
        property Memo: TMemo read FMemo write SetMemo;
        //22/02/2025 Property Port and Interval moved in inifile
        //property Port:              Integer read FPort write SetPort default 2520;
        //property Interval:          Integer read FInterval write SetInterval default 3000;
        property OnCommandReceived: TOnCommandReceived read FOnCommandReceived write FOnCommandReceived;
        property OnStopRequested:   TOnStopRequested read FOnStopRequested write FOnStopRequested;
        property OnGetAppParams:    TOnGetAppParams read FOnGetAppParams write FOnGetAppParams;
        property Lang:              TAppWatcherLang read FLang write SetLang default langFr;
        property CloseRequested:    Boolean read FRequestingClose;
    end;

procedure Register;

implementation


procedure Register;
begin
    RegisterComponents('AppWatcher', [TAppWatcherClient]);
end;

{TAppWatcherClient}

const
    CLientConnectRetryDelay: Integer = 5000; //5 sec

function FindParentForm(AComponent: TComponent): TForm;
var
    Current: TComponent;
begin
    Result := nil;
    Current := AComponent;

    //🔄 Remonte dans la hiérarchie des "Owner" (gestion mémoire)
    while Assigned(Current) do
    begin
        if Current is TForm then
            Exit(TForm(Current)); //✅ Trouvé !

        Current := Current.Owner;
    end;

    //🔄 Remonte aussi la hiérarchie des "Parent" (visuel)
    if AComponent is TControl then
    begin
        Current := TControl(AComponent).Parent;
        while Assigned(Current) do
        begin
            if Current is TForm then
                Exit(TForm(Current)); //✅ Trouvé via Parent !

            Current := TControl(Current).Parent;
        end;
    end;
end;

constructor TAppWatcherClient.Create(AOwner: TComponent);
var
    IniFilePathName: string;
    Ini:             TMemIniFile;
    ParentForm:      TForm;
begin

    inherited Create(AOwner);
    FIsFirstLoad := true;
    FRequestingClose := False; //Par défaut, pas de demande de fermeture
    FSignalErrorConnect := true;
    FPort := 2520; //default port
    FInterval := 3000; //Default inMessages checking interval

    FIdTCPClient := TIdTCPClient.Create(Self);
    FIdTCPClient.Host := '127.0.0.1';
    FIdTCPClient.Port := FPort;
    FIdTCPClient.ReadTimeout := 1000;

    //✅ Créer un gestionnaire de langue propre à chaque composant
    FLang := langFr; //Définit la langue par défaut
    FLanguageManager := TAppLangManager.Create(FLang);

    FTimer := TTimer.Create(Self);
    FTimer.Interval := FInterval;
    FTimer.OnTimer := TimerEvent;
    FTimer.Enabled := False;

    if not(csDesigning in ComponentState) then
        FTimer.Enabled := true; //✅ Actif seulement en mode exécution

end;

destructor TAppWatcherClient.Destroy;
begin
    Disconnect;
    FreeAndNil(FTimer);
    FreeAndNil(FIdTCPClient);
    FreeAndNil(FLanguageManager);
    inherited Destroy;
end;

procedure TAppWatcherClient.Loaded;
var
    IniFilePathName: string;
    Ini:             TMemIniFile;
    ParentForm:      TForm;
    Msg:             string;
begin
    inherited Loaded;

    if Assigned(FMemo) then
        FMemo.Lines.clear;

    ParentForm := FindParentForm(Self.GetParentComponent);
    FIsOnMainForm := (ParentForm = Application.MainForm);

    //Charger la langue
    SetLang(FLang);

    //Charger le fichier INI
    IniFilePathName := FindConfigPath(AppWatcherIniFileName);

    if IniFilePathName = '' then
    begin
        //Le fichier INI est introuvable
        FIniFileNotFOund := true;
        if FLang = LangEn then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileName])
        else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileName]);

        WriteMsg(Msg);
        if FIsOnMainForm and not(csDesigning in ComponentState) then
            MessageDlg(Msg, mtError, [mbok], 0)
    end
    else
    begin
        //Charger les valeurs du fichier INI
        Ini := TMemIniFile.Create(IniFilePathName);
        try
            FPort := Ini.ReadInteger('ClientConfig', 'Port', 2520);
            FInterval := Ini.ReadInteger('ClientConfig', 'Interval', 3000);
        finally
                Ini.Free;
        end;
    end;

    FIsFirstLoad := False;
end;

procedure TAppWatcherClient.SetLang(Value: TAppWatcherLang);
var
    Msg: string;
begin
    if (FLang <> Value) or FIsFirstLoad then
    begin
        FLang := Value;
        FIsFirstLoad := False;

        // ✅ Charger la langue localement
        if not FLanguageManager.LoadLanguage(FLang) then
        begin
            if FLang = langEn then
                Msg := format(MsgIniFileNotFoundEn, [LangEnIniFileName])
            else
                Msg := format(MsgIniFileNotFoundFr, [LangFrIniFileName]);

            WriteMsg(Msg);

            if FIsOnMainForm and not (csDesigning in ComponentState) then
                MessageDlg(Msg, mtError, [mbOK], 0);

            FIniFileNotFound := True;
        end;
    end;
end;




Function TAppWatcherClient.InstanceActive: Boolean;
Begin
    Result := true;

    if not FIsOnMainForm or FRequestingClose or FIniFileNotFOund then begin
        Result := False;
        if FIniFileNotFOund then begin
            //Message si le fichier est introuvable
            if FLang = LangEn then
                WriteMsg(MsgNoInstanceActiveEn)
            else
                WriteMsg(MsgNoInstanceActiveFr);

        end
        Else
            WriteMsgFormat('CLIENT', 'NOT_ACTIVE', []);
    end;
ENd;

procedure TAppWatcherClient.Connect;
var
    ParentForm: TForm;
begin
    //ParentForm := FindParentForm(Self.GetParentComponent);
    //FIsOnMainForm := (ParentForm = Application.MainForm);
    if not InstanceActive then
        Exit;

    if (Now - FLastReconnectAttempt) * 86400000 < CLientConnectRetryDelay then
        Exit; //⏳ Attendre avant de réessayer

    FLastReconnectAttempt := Now;

    if not FIdTCPClient.Connected then
    begin
        try
            FIdTCPClient.Port := FPort;
            FIdTCPClient.Connect;
            FSignalErrorConnect := False;

            if Assigned(FMemo) then
                FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'CONNECTED'));
        except
            on E: Exception do
            begin
                if FSignalErrorConnect then begin
                    if Assigned(FMemo) then
                        FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'FAILED_CONNECT'));
                    FSignalErrorConnect := False;
                end;
            end;
        end;
    end;
end;

procedure TAppWatcherClient.TimerEvent(Sender: TObject);
var
    ParentForm: TForm;
begin
    //ParentForm := FindParentForm(Self.GetParentComponent);
    //FIsOnMainForm := (ParentForm = Application.MainForm);
    if not InstanceActive then begin
        FTimer.Enabled := False;
        Exit;
    end;

    if not FIdTCPClient.Connected then
        Connect;

    //Exécuter CheckForCommands dans un thread
    TThread.CreateAnonymousThread(
        procedure
        begin
            CheckForCommands;
        end).Start;
end;

procedure TAppWatcherClient.Disconnect;
begin
    if FIdTCPClient.Connected then
        FIdTCPClient.Disconnect;
end;

procedure TAppWatcherClient.SetMemo(Value: TMemo);
begin
    FMemo := Value;
end;

procedure TAppWatcherClient.SetPort(Value: Integer);
begin

    if Value <> FPort then
    begin
        FPort := Value;
        if FIdTCPClient.Connected then
        begin
            Disconnect;
            Connect;
        end;
    end;
end;

procedure TAppWatcherClient.SetInterval(Value: Integer);
begin
    if Value <> FInterval then
    begin
        FInterval := Value;
        FTimer.Interval := FInterval;
    end;
end;

procedure TAppWatcherClient.WriteMsg(const Msg: string);
begin
    TThread.Queue(nil,
        procedure
        begin
            if Assigned(FMemo) then
                FMemo.Lines.Add(Msg);
        end);
end;

procedure TAppWatcherClient.WriteMsgFormat(const Section, Msg: string; const Args: array of const);
var
    FormattedMsg: string;
begin
    // ✅ Nouvelle version utilisant l'instance locale
    FormattedMsg := Format(FLanguageManager.GetMessage(Section, Msg), Args);
    WriteMsg(FormattedMsg);
end;


procedure TAppWatcherClient.CheckForCommands;
var
    LocalMsg, AnswerMsg, ReceivedMsg: TAppWatcherMessage;
    CanStop:                          Boolean;
    AppPath, Params:                  string;
    idx:                              Integer;
    MessageReceived:                  Boolean;
begin
    if not FIdTCPClient.Connected then
        Exit;

    try
        MessageReceived := False;

        //🔁 Lire tous les messages disponibles
        while FIdTCPClient.IOHandler.InputBuffer.Size > 0 do
        begin
            MessageReceived := true;

            //🔹 Lire les données réseau
            if ReadMessage(FIdTCPClient.IOHandler, ReceivedMsg) then
            begin
                LocalMsg := ReceivedMsg;
                AppPath := ParamStr(0);

                WriteMsgFormat('CLIENT', 'MESSAGE_RECEIVED', [LocalMsg.CmdName]);

                case LocalMsg.Command of
                    //🔵 Commande WHO (identification)
                    cmdWHO:
                        begin
                            AnswerMsg.Init(Application.Handle, cmdWHO_REPLY, AppPath, '', 0);
                            AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                            WriteMsgFormat('CLIENT', 'WHO_RECEIVED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                        end;

                    //🛑 Commande STOP (demande d'arrêt)
                    cmdSTOP:
                        begin
                            //Pas de réponse si la demande ne nous concerne pas
                            if SameText(ExtractFileName(AppPath), LocalMsg.AppName) and (LocalMsg.Handle = Application.Handle) then
                            begin
                                Params := '';
                                if Assigned(FOnGetAppParams) then
                                    TThread.Synchronize(nil,
                                        procedure
                                        begin
                                            FOnGetAppParams(Self, Params);
                                        end);

                                //Initialisation de la réponse
                                AnswerMsg.Init(Application.Handle, cmdACK, ParamStr(0), Params, 0);
                                WriteMsgFormat('CLIENT', 'STOP_RECEIVED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);

                                //🔍 Demander l'arrêt à l'application
                                CanStop := true;
                                if Assigned(FOnStopRequested) then
                                begin
                                    TThread.Synchronize(nil,
                                        procedure
                                        begin
                                            FOnStopRequested(Self, CanStop);
                                        end);
                                end;

                                //📌 Réponse ACK ou NACK
                                if CanStop then begin
                                    AnswerMsg.Command := cmdACK;
                                    WriteMsgFormat('CLIENT', 'STOP_ACCEPTED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                end else begin
                                    AnswerMsg.Command := cmdNACK;
                                    WriteMsgFormat('CLIENT', 'STOP_REFUSED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                end;

                                AnswerMsg.SendMessage(FIdTCPClient.IOHandler);

                                //🔥 Fermer l'application si arrêt accepté
                                if CanStop then
                                    TThread.Queue(nil,
                                        procedure
                                        begin
                                            FRequestingClose := true; //✅ Indique que le composant demande la fermeture

                                            if Assigned(Application.MainForm) then begin
                                                Application.MainForm.OnCloseQuery := nil;

                                                Application.MainForm.close;
                                            end
                                            else
                                                Application.terminate; //Sécurité si la MainForm n'existe pas
                                        end);

                            end;
                        end;

                    cmdSTOP_REQUEST:
                        begin
                            //Préparation à une demande de STOP
                            //L'application doit renvoyer son Handle
                            WriteMsgFormat('CLIENT', 'STOP_REQUEST', [IntToStr(Application.Handle)]);
                            //Construire la réponse enrichie avec le Handle

                            AnswerMsg.Init(Application.Handle, cmdREPLY_STOP_REQUEST, ParamStr(0), '', LocalMsg.Duration);
                            //Envoyer la réponse STOP à l’AGENT
                            AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                        end;

                else
                    //❓ Commande inconnue
                    WriteMsgFormat('CLIENT', 'UNKNOW_COMMAND', [LocalMsg.CmdName, IntToStr(Application.Handle)]);
                end;

                //🔔 Déclencher l'événement OnCommandReceived
                if Assigned(FOnCommandReceived) then
                begin
                    TThread.Synchronize(nil,
                        procedure
                        begin
                            FOnCommandReceived(Self, LocalMsg.Command);
                        end);
                end;
            end;
        end;

        //✅ Ajout d'une pause uniquement si aucun message n'a été lu
        if not MessageReceived then
            Sleep(10);
    except
        on E: Exception do
            WriteMsgFormat('CLIENT', 'ERROR_RECEIVING', [E.Message]);

    end;
end;

end.
