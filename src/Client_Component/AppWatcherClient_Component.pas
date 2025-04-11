(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_Component.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 09/04/2025
  Version  : 3.0.0
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
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.
  - [25/02/2025] : v1.3.2 **Fixed bug where all applications were stopping instead of only the requested one**
  - [06/03/2025] : v2.0 - Use of New  TAppWatcherMessage with Packed Record
                          !!! This change makes the version incompatible with v1 !!!
                        - Minor modification : improvement of FindParentForm
                        - Correction : FLastCommand was never filled
  - [09/04/2025] : v3.0 - Adapted to use Named Pipes instead of TCPIP for communication with the local agent.
                        - Removed Port and Interval properties

  *******************************************************************************)

unit AppWatcherClient_Component;

interface

uses
    System.Classes, System.SysUtils, IdTCPClient, IdTCPConnection, IdComponent,
    IdGlobal, IdBaseComponent, Vcl.ExtCtrls, Vcl.StdCtrls, AppWatcher_ioHandler,
    Vcl.Forms, AppWatcher_Lang, Vcl.Controls, dialogs, System.IniFiles,
    Winapi.Windows, Winapi.Messages, AppWatcher_consts, PipesCommon, PipeClient;

type
    TOnCommandReceived = procedure(Sender: TObject; const Command: TAppWatcherCommand) of object;

    TOnStopRequested = procedure(Sender: TObject; var CanStop: Boolean) of object;

    TOnGetAppParams = procedure(Sender: TObject; var Params: string) of object;

    TAppWatcherClient = class(TComponent)
    private
        //FIdTCPClient: TIdTCPClient;
        FNamedPipeClient: TPipeClient;
        FTimer: TTimer;
        FMemo: TMemo;
        FLang: TAppWatcherLang;
        FLastCommand: string;
        FOnCommandReceived: TOnCommandReceived;
        FOnStopRequested: TOnStopRequested;
        FOnGetAppParams: TOnGetAppParams;
        FPort: Integer;
        FInterval: Integer;
        FLastReconnectAttempt: TDateTime;
        FSendNoConnectMessage: Boolean;
        FRequestingClose: Boolean;
        FIniFileNotFOund: Boolean;
        FIsOnMainForm: Boolean;
        FProcessingScheduled: Boolean;
        FLanguageManager: TAppLangManager;
        FMsgQueue: TAppWatcherMessageQueue;

        procedure TimerEvent(Sender: TObject);
        procedure SetMemo(Value: TMemo);
        //        Procedure SetPort(Value: Integer);
        procedure SetInterval(Value: Integer);
        procedure CheckForCommands;
        procedure SetLang(Value: TAppWatcherLang);
        procedure WriteMsgFormat(const Section, Msg: string; const Args: array of const);
        procedure WriteMsg(const Msg: string);
        function InstanceActive: Boolean;
        procedure NamedPipeClientPipeMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
        procedure NamedPipeClientPipeDisconnect(Sender: TObject; Pipe: HPIPE);
    protected
        FIsFirstLoad: Boolean; //Permet de savoir si c'est le premier chargement
        procedure Loaded; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Connect;
        procedure Disconnect;
        property LastCommand: string read FLastCommand;
    published
        property Memo: TMemo read FMemo write SetMemo;
        //22/02/2025 Property Port and Interval moved in inifile
        //property Port:              Integer read FPort write SetPort default 2520;
        //property Interval:          Integer read FInterval write SetInterval default 3000;
        property OnCommandReceived: TOnCommandReceived read FOnCommandReceived write FOnCommandReceived;
        property OnStopRequested: TOnStopRequested read FOnStopRequested write FOnStopRequested;
        property OnGetAppParams: TOnGetAppParams read FOnGetAppParams write FOnGetAppParams;
        property Lang: TAppWatcherLang read FLang write SetLang default langFr;
        property CloseRequested: Boolean read FRequestingClose;
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
        Exit(TForm(GetParentForm(TControl(AComponent))));
    end;
end;

constructor TAppWatcherClient.Create(AOwner: TComponent);
var
    ParentForm: TForm;
begin

    inherited Create(AOwner);

    FIsFirstLoad := true;

    FRequestingClose := False; //Par défaut, pas de demande de fermeture
    FSendNoConnectMessage := true;
    FProcessingScheduled := false;
    FPort := 2520; //default port
    FInterval := 3000; //Default inMessages checking interval


    FMsgQueue := TAppWatcherMessageQueue.Create;

    FNamedPipeClient := TPipeClient.Create;
    with FNamedPipeClient do
    begin
        PipeName := cPipeName;
        OnPipeDisconnect := NamedPipeClientPipeDisconnect;
        OnPipeMessage := NamedPipeClientPipeMessage;
    end;

    FLang := langFr; // Définit la langue par défaut
    FLanguageManager := TAppLangManager.Create(FLang);

    FTimer := TTimer.Create(Self);
    FTimer.Interval := FInterval;
    FTimer.OnTimer := TimerEvent;
    FTimer.Enabled := False;


    if not (csDesigning in ComponentState) then
    begin
        // Actif seulement en mode exécution
        FTimer.Enabled := true;

    end;
end;

destructor TAppWatcherClient.Destroy;
begin


//    Disconnect;
    FreeAndNil(FTimer);

    FreeAndNil(FNamedPipeClient);
    FreeAndNil(FMsgQueue);
    FreeAndNil(FLanguageManager);

    inherited Destroy;
end;



procedure TAppWatcherClient.Loaded;
var
    IniFilePathName: string;
    Ini: TMemIniFile;
    ParentForm: TForm;
    Msg: string;
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
        if FIsOnMainForm and not (csDesigning in ComponentState) then
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

        //✅ Charger la langue localement
        if not FLanguageManager.LoadLanguage(FLang) then
        begin
            if FLang = LangEn then
                Msg := format(MsgIniFileNotFoundEn, [LangEnIniFileName])
            else
                Msg := format(MsgIniFileNotFoundFr, [LangFrIniFileName]);

            WriteMsg(Msg);

            if FIsOnMainForm and not (csDesigning in ComponentState) then
                MessageDlg(Msg, mtError, [mbok], 0);

            FIniFileNotFOund := true;
        end;
    end;
end;

function TAppWatcherClient.InstanceActive: Boolean;
begin
    Result := true;

    if not FIsOnMainForm or FRequestingClose or FIniFileNotFOund then
    begin
        Result := False;
        if FIniFileNotFOund then
        begin
            //Message si le fichier est introuvable
            if FLang = LangEn then
                WriteMsg(MsgNoInstanceActiveEn)
            else
                WriteMsg(MsgNoInstanceActiveFr);
        end
        else
            WriteMsgFormat('CLIENT', 'NOT_ACTIVE', []);
    end;
end;

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

    try
        if not FNamedPipeClient.Connected then
        begin
            //    If Not FIdTCPClient.Connected Then Begin
            try
                //            FIdTCPClient.Port := FPort;
                //            FIdTCPClient.Connect;
                FNamedPipeClient.Connect(100);

                if FNamedPipeClient.Connected then
                begin
                    // Connexion réussie on a pas besoin de réessayer
                    FTimer.Enabled := False;

                    if Assigned(FMemo) then
                        FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'CONNECTED'));
                end
                else if FSendNoConnectMessage and Assigned(FMemo) then
                    FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'FAILED_CONNECT'));
            except
                on E: Exception do
                begin
                    if FSendNoConnectMessage and Assigned(FMemo) then
                        FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'FAILED_CONNECT'));
                end;
            end;
        end;
    finally
        FSendNoConnectMessage := False;
    end;
end;

procedure TAppWatcherClient.TimerEvent(Sender: TObject);
var
    ParentForm: TForm;
begin

    if not InstanceActive then
    begin
        FTimer.Enabled := False;
        Exit;
    end;

    // Tentative de connexion
    if not FNamedPipeClient.Connected then
        Connect;
end;

procedure TAppWatcherClient.Disconnect;
begin

    if FNamedPipeClient.Connected then
        FNamedPipeClient.Disconnect(true);
end;

procedure TAppWatcherClient.SetMemo(Value: TMemo);
begin
    FMemo := Value;
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
    //✅ Nouvelle version utilisant l'instance locale
    FormattedMsg := format(FLanguageManager.GetMessage(Section, Msg), Args);
    WriteMsg(FormattedMsg);
end;

procedure TAppWatcherClient.NamedPipeClientPipeMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
var
    ReceivedMsg: TAppWatcherMessage;
begin


    if Stream.Size <> SizeOf(TAppWatcherMessage) then
        WriteMsgFormat('CLIENT', 'ERROR_RECEIVING', [])
    else
    begin
        Stream.Position := 0;
        Stream.ReadBuffer(ReceivedMsg, SizeOf(TAppWatcherMessage));
        FMsgQueue.Enqueue(ReceivedMsg);
    end;

    // Planifier le traitement si ce n'est déjà fait
    if not FProcessingScheduled then
    begin
        FProcessingScheduled := True;
        TThread.Queue(nil,
            procedure
            begin
                CheckForCommands;
            end);
    end;
end;

procedure TAppWatcherClient.NamedPipeClientPipeDisconnect(Sender: TObject; Pipe: HPIPE);
begin


    // Le client est déconnecté du serveur
    FSendNoConnectMessage := true;

    // On remet le timer en route pour faire des tentatives de reconnexion
    fTimer.enabled := true;
end;

procedure TAppWatcherClient.CheckForCommands;
var
    LocalMsg, AnswerMsg, ReceivedMsg: TAppWatcherMessage;
    CanStop: Boolean;
    AppPath, Params: string;
    idx: Integer;
    MessageReceived: Boolean;
begin


    if not FNamedPipeClient.Connected then
        Exit;

    try
        // Traiter tous les messages disponibles
        repeat

            MessageReceived := False;

            //🔁 Lire tous les messages disponibles
            while FMsgQueue.Dequeue(ReceivedMsg) do
            begin

                MessageReceived := true;

                try
                    LocalMsg := ReceivedMsg;
                    AppPath := ParamStr(0);
                    FLastCommand := LocalMsg.CmdName;

                    WriteMsgFormat('CLIENT', 'MESSAGE_RECEIVED', [LocalMsg.CmdName]);

                    case LocalMsg.Command of
                        //🔵 Commande WHO (identification)
                        cmdWHO:
                            begin
                                //                        AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                                WriteMsgFormat('CLIENT', 'WHO_RECEIVED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);

                                AnswerMsg.Init(Application.Handle, cmdWHO_REPLY, AppPath, '', 0,LocalMsg.Silent);
                                if not FNamedPipeClient.Write(AnswerMsg, SizeOf(AnswerMsg)) then
                                    WriteMsgFormat('CLIENT', 'ERROR_SENDING', ['WHO']);
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
                                    AnswerMsg.Init(Application.Handle, cmdACK, ParamStr(0), Params, 0,LocalMsg.Silent);
                                    WriteMsgFormat('CLIENT', 'STOP_RECEIVED', [AnswerMsg.AppPath + ' params=' + Params, IntToStr(AnswerMsg.Handle)]);

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
                                    if CanStop then
                                    begin
                                        AnswerMsg.Command := cmdACK;
                                        WriteMsgFormat('CLIENT', 'STOP_ACCEPTED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                    end
                                    else
                                    begin
                                        AnswerMsg.Command := cmdNACK;
                                        WriteMsgFormat('CLIENT', 'STOP_REFUSED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                    end;

                                    //AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                                    if not FNamedPipeClient.Write(AnswerMsg, SizeOf(AnswerMsg)) then
                                        WriteMsgFormat('CLIENT', 'ERROR_SENDING', ['STOP']);

                                    //🔥 Fermer l'application si arrêt accepté
                                    if CanStop then
                                        TThread.Queue(nil,
                                            procedure
                                            begin
                                                FRequestingClose := true; //✅ Indique que le composant demande la fermeture

                                                if Assigned(Application.MainForm) then
                                                begin
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
                                //Pas de réponse si la demande ne nous concerne pas
                                if SameText(ExtractFileName(AppPath), LocalMsg.AppName) then
                                begin

                                    //L'application doit renvoyer son Handle
                                    WriteMsgFormat('CLIENT', 'STOP_REQUEST', [IntToStr(Application.Handle)]);
                                    //Construire la réponse enrichie avec le Handle

                                    AnswerMsg.Init(Application.Handle, cmdREPLY_STOP_REQUEST, ParamStr(0), '', LocalMsg.Duration,LocalMsg.Silent);
                                    //Envoyer la réponse STOP à l’AGENT
                                    if not FNamedPipeClient.Write(AnswerMsg, SizeOf(AnswerMsg)) then
                                        WriteMsgFormat('CLIENT', 'ERROR_SENDING', ['STOP_REQUEST']);
                                    //AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                                end;
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
                except
                    on E: Exception do
                        WriteMsgFormat('CLIENT', 'ERROR_PROCESSING', [E.Message]);
                end;
            end;

            // Si de nouveaux messages sont arrivés pendant le traitement, la file sera non vide,
            // et la boucle redémarrera.
        until not MessageReceived;

        // Ajout d'une pause
        Sleep(10);
    finally
        // Le traitement est terminé, on libère le flag
        FProcessingScheduled := False;

        // Au cas où de nouveaux messages auraient été enfilés juste après,
        // on vérifie de nouveau et on replanifie si nécessaire.
        if FMsgQueue.Count > 0 then
        begin
            FProcessingScheduled := True;
            TThread.Queue(nil,
                procedure
                begin
                    CheckForCommands;
                end);
        end;
    end;
end;

end.

