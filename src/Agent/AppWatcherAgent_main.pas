(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherAgent_main.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 1.2
  License  : MIT

  Description :
  -------------
  This unit manages the **Agent** component of the AppWatcher project.
  The Agent communicates with the **Master** over TCP and handles
  stopping and restarting local applications.

  Features :
  -----------
  - TCP communication with the **Master** (`IdTCPClient`)
  - Handles **STOP / WHO / START** commands
  - Displays stop notifications and restart messages
  - Automatic reconnection if the connection to the Master is lost
  - Thread-safe message queue using `TCriticalSection`

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [10/02/2025] : Added stop notification management
  - [19/02/2025] : Improved thread safety
  - [22/02/2025] : Improved INI file handling in FormCreate :  Ensured `FindConfigPath` checks file existence before loading.
  - [22/02/2025] : Replaced the singleton AppLangManager with a local instance to allow multiple instances.
  - [23/02/2025] : Fixed missing client section loading from INI file (ClientConfig port and interval are now loaded in Agent)
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.


  Notes :
  -------
  This project is **open-source**. Contributions and improvements are welcome!
  *******************************************************************************)

unit AppWatcherAgent_main;

interface

uses
    Winapi.Windows, Winapi.Messages, Winapi.ShellAPI, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Tlhelp32, System.Generics.Collections,
    Vcl.ExtCtrls, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
    IdTCPServer, System.IniFiles, IdTCPConnection, IdTCPClient, IdException, AppWatcherAgent_Stop,
    AppWatcher_ioHandler, AppWatcher_Lang, System.SyncObjs,
    AppWatcher_consts;

type

    TDialogState = (dsInit, dsStopRequested, dsStopped, dsRestarted, dsCancel);

    TStopDialog = class
        CountDown: Integer;
        Handle: HWND;
        AppName: string; //Nom
        AppPath: string; //Chemin complet
        Params: string; //Arguments
        FormStop: TFormStopNotification;
        State: TDialogState;
        constructor Create(LanguageManager: TAppLangManager);
        destructor Destroy; override;
    end;

    TStoppedDialogs = class
    private
        FList: TList<TStopDialog>;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        function TryGetStopDialog(Handle: HWND; AppName: string; var StopDialog: TStopDialog; var idx: Integer): Boolean;
        function AddOrReplace(AppWatcherMsg: TAppWatcherMessage; LanguageManager: TAppLangManager): TStopDialog;
        function AppList: string;
        function Count: Integer;
        procedure Delete(idx: Integer);
        Procedure Restart(idx: Integer);
        property Items: TList<TStopDialog> read FList;
    end;

    TFormAppWatcher = class(TForm)
        Memo1: TMemo;
        TrayIcon1: TTrayIcon;
        PopupMenu1: TPopupMenu;
        Voir1: TMenuItem;
        IdTCPServerCLIENT: TIdTCPServer;
        IdTCPClientMASTER: TIdTCPClient;
        Timer1: TTimer;
        Quitter1: TMenuItem;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure IdTCPServerCLIENTExecute(AContext: TIdContext);
        procedure Quitter1Click(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure Voir1Click(Sender: TObject);
    private
        {Déclarations privées}
        FServerIP:             string;
        FServerPort:           Integer;
        FStopTimeOut:          Integer;
        FCLientPort:           Integer;
        FClientInterval:       Integer;
        FStopDialogs:          TStoppedDialogs;
        FLastReconnectAttempt: TDateTime;
        FLastConfigLoad:       TDateTime;
        FMessageQueue:         TQueue<TAppWatcherMessage>; //File d’attente des messages
        FQueueLock:            TCriticalSection; //🔒 Protection d'accès à la file
        FSignalErrorConnect:   Boolean;
        FRequestingClose:      Boolean;
        FLang:                 TAppWatcherLang;
        FLanguageManager:      TAppLangManager;
        Function GetPassword: Boolean;
        procedure ProcessQueuedMessages; //🔄 Fonction de traitement dans Timer1
        procedure WM_SYSCOMMAND(var Msg: TMessage); message WM_SYSCOMMAND;
        procedure LoadConfig;

        procedure HandleMasterMessage(var Msg: TAppWatcherMessage);
        procedure HandleMASTERWhoRequest;
        procedure HandleMASTERStopRequest(var Msg: TAppWatcherMessage);
        procedure HandleMASTERStartRequest;
        procedure HandleMASTERCancelRequest;
        procedure HandleMASTERStopAgent;

        procedure HandleCLIENTWhoReply(Msg: TAppWatcherMessage);
        procedure HandleCLIENTStopRequestReply(Msg: TAppWatcherMessage);
        procedure HandleCLIENTAckReply(Msg: TAppWatcherMessage);
        procedure HandleCLIENTNackReply(Msg: TAppWatcherMessage);

        procedure SendStopCommandToCLIENT(var StopDialog: TStopDialog);
    public
        {Déclarations publiques}
    end;

var
    FormAppWatcher: TFormAppWatcher;

const
    RECONNECT_INTERVAL = 10000; //10 secondes
    LOADCONFIG_INTERVAL = 5000; //5 secondes

implementation

{$R *.dfm}


var
    DialogLock: TCriticalSection;

    (*============================== GESTION  DIALOGUE DE STOP ====================================================================*)

constructor TStopDialog.Create(LanguageManager: TAppLangManager);
Begin
    inherited Create;
    CountDown := 30;
    Handle := 0;
    AppName := ''; //Nom
    AppPath := ''; //Chemin complet
    Params := ''; //Arguments
    State := dsInit;
    //Création du dialogue
    FormStop := TFormStopNotification.Create(FormAppWatcher);
    FormStop.FLanguageManager := LanguageManager;
End;

destructor TStopDialog.Destroy;
Begin
    FormStop.Free;
    inherited;
End;

(*============================== GESTION DE LA LISTE DES DIALOGUES DE STOP ====================================================================*)

constructor TStoppedDialogs.Create;
begin
    inherited Create;
    FList := TList<TStopDialog>.Create;
end;

destructor TStoppedDialogs.Destroy;
begin
    Clear;

    FList.Free;
    inherited;
end;

procedure TStoppedDialogs.Clear;
var
    i: Integer;
begin
    for i := FList.Count - 1 downto 0 do
    begin
        if Assigned(FList[i]) then
        begin
            FList[i].Free; //✅ Libération explicite de la mémoire
            FList[i] := nil; //🔧 Éviter tout accès invalide
        end;
    end;
    FList.Clear; //Maintenant on peut vider la liste en toute sécurité
end;

function TStoppedDialogs.TryGetStopDialog(Handle: HWND; AppName: string; var StopDialog: TStopDialog; var idx: Integer): Boolean;
var
    i: Integer;
begin
    Result := False;
    idx := -1;
    for i := 0 to FList.Count - 1 do
    begin
        if (FList[i].Handle = Handle) and (FList[i].AppName = AppName) then
        begin
            idx := i;
            StopDialog := FList[i];
            Exit(True);
        end;
    end;
end;

function TStoppedDialogs.AddOrReplace(AppWatcherMsg: TAppWatcherMessage; LanguageManager: TAppLangManager): TStopDialog;
var
    idx: Integer;
begin

    //Recherche d'un élément existant
    if TryGetStopDialog(AppWatcherMsg.Handle, AppWatcherMsg.AppName, Result, idx) then
    begin
        //Mise à jour de l'entrée existante
        Result.AppPath := AppWatcherMsg.AppPath;
        Result.Params := AppWatcherMsg.Params;
        Exit;
    end;

    //Ajout d'une nouvelle entrée
    Result := TStopDialog.Create(LanguageManager);

    Result.Handle := AppWatcherMsg.Handle;
    Result.AppName := AppWatcherMsg.AppName;
    Result.AppPath := AppWatcherMsg.AppPath;
    Result.Params := AppWatcherMsg.Params;
    Result.CountDown := AppWatcherMsg.Duration;
    FList.Add(Result);

end;

Function TStoppedDialogs.AppList: string;
var
    i: Integer;
begin
    Result := '';
    for i := 0 to FList.Count - 1 do
        if FList[i].State = dsStopped then
            if i = 0 then
                Result := FList[i].AppName
            else
                Result := Result + #13#10 + FList[i].AppName;

End;

function TStoppedDialogs.Count: Integer;
begin
    Result := FList.Count;
end;

procedure TStoppedDialogs.Delete(idx: Integer);
begin
    if (idx >= 0) and (idx < FList.Count) then begin
        FList[idx].Free;
        FList.Delete(idx);
    End;
end;

Procedure TStoppedDialogs.Restart(idx: Integer);
var
    CurDialog: TStopDialog;
begin
    if (idx >= 0) and (idx < FList.Count) then
        FList[idx].State := dsRestarted;
end;

(*============================== DEMARRAGE ET INITIALISATIONS ====================================================================*)

procedure TFormAppWatcher.LoadConfig;
var
    Ini:                  TMemIniFile;
    IniFilePathName, Msg: string;
begin
    //🔹 Trouver le fichier de configuration
    IniFilePathName := FindConfigPath(AppWatcherIniFileNAme);

    if IniFilePathName = '' then
    begin
        //Le fichier INI est introuvable
        if FLang = LangEn then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileNAme])
        else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileNAme]);

        MessageDlg(Msg, mtError, [mbok], 0);
        FRequestingClose := True;
        Exit;
    end;

    //🔹 Charger le fichier INI
    Ini := TMemIniFile.Create(IniFilePathName);
    try
        //🔹 Charger l'IP du serveur et le port
        FServerIP := Ini.ReadString('MasterConfig', 'ServerIP', '127.0.0.1');
        FServerPort := Ini.ReadInteger('MasterConfig', 'Port', 2510);
        FStopTimeOut := Ini.ReadInteger('MasterConfig', 'StopTimeOut', 30);
        IdTCPClientMASTER.Port := FServerPort;
        IdTCPClientMASTER.Host := FServerIP;
        IdTCPClientMASTER.ReadTimeout := 10000; //Timeout de lecture pour éviter blocage

        FCLientPort := Ini.ReadInteger('ClientConfig', 'Port', 2520);
        FClientInterval := Ini.ReadInteger('ClientConfig', 'Interval', 3000);
        IdTCPServerCLIENT.DefaultPort := FCLientPort;

    finally
            Ini.Free;
    end;
end;

procedure TFormAppWatcher.WM_SYSCOMMAND(var Msg: TMessage);
begin
    if (Msg.WParam = SC_MINIMIZE) then
    begin
        Hide; //Cache la fenêtre
        TrayIcon1.Visible := True; //Affiche l'icône dans la zone de notification
        Exit;
    end;
    inherited;
end;

procedure TFormAppWatcher.FormCreate(Sender: TObject);
var
    LangValue:            string;
     Msg: string;
    LanguageLoaded:       Boolean;
begin
    //🔹 Trouver le fichier de configuration
    FLastConfigLoad := Now;
    FRequestingClose := False;

    //🔹 Vérification des paramètres en ligne de commande (--lang fr ou --lang en)
    if FindCmdLineSwitch('lang', LangValue, True, [clstValueNextParam, clstValueAppended]) then
    begin
        LangValue := LowerCase(LangValue);
        if (LangValue <> 'fr') and (LangValue <> 'en') then
            FLang := LangFr; //🔴 Sécurisation : éviter une langue inconnue
    end
    else
        FLang := LangFr;

    LoadConfig; //Charge le fichier INI

    FLanguageManager := TAppLangManager.Create(FLang);

    //🔹 Chargement de la langue
    //🔴 Si le fichier est introuvable, afficher un message et quitter
    if not FLanguageManager.LoadLanguage(FLang) then
    begin
        //Le fichier INI est introuvable
        if FLang = LangEn then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileNAme])
        else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileNAme]);
        MessageDlg(Msg, mtError, [mbok], 0);

        FreeAndNil(FLanguageManager);
        FRequestingClose := True;

    end;

    if FRequestingClose then
        application.terminate;

    Self.caption := FLanguageManager.GetMessage('AGENT', 'TITLE');

    FLastReconnectAttempt := Now;
    FSignalErrorConnect := True;

    FStopDialogs := TStoppedDialogs.Create;

    FMessageQueue := TQueue<TAppWatcherMessage>.Create;
    FQueueLock := TCriticalSection.Create;

    Timer1.Interval := 1000; //Vérifie chaque seconde
    Timer1.enabled := True;

    application.ShowMainForm := False;
    Hide;
    TrayIcon1.Visible := True;
end;

Function TFormAppWatcher.GetPassword: Boolean;
var
    Password:      string;
    CloseQryF:     TInputCloseQueryFunc;
    PasswordArray: array of string;
begin
    Result := False;
    Password := '';
    Password := InputBox(FLanguageManager.GetMessage('AGENT', 'PROTECT'), #0 + FLanguageManager.GetMessage('AGENT', 'PASSWORD_REQUEST'), '');

    if Password = AppWPassword then
    begin
        Result := True;
    end
    else
        ShowMessage(FLanguageManager.GetMessage('AGENT', 'INVALID_PASSWORD'));
end;

procedure TFormAppWatcher.FormClose(Sender: TObject; var Action: TCloseAction);
var
    Password: string;
begin
    if (application.Terminated) or (GetSystemMetrics(SM_SHUTTINGDOWN) = 1) or (FRequestingClose) then
    begin
        Timer1.enabled := False; //✅ Empêcher toute exécution pendant la fermeture

        //Windows ferme la session → on ferme normalement
        TrayIcon1.Visible := False;
        FStopDialogs.Free;
        FreeAndNil(FLanguageManager);
        FQueueLock.Enter;
        try
            FMessageQueue.Clear;
            FreeAndNil(FMessageQueue);
        finally
            FQueueLock.Leave;
            FreeAndNil(FQueueLock);
        end;
        Action := caFree;

    end
    else
    begin
        //Vérifier si l'utilisateur est autorisé à fermer
        if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) then
        begin
            if not GetPassword then
                Action := caNone; //Empêche la fermeture complète
        end
        else
        begin
            //L'utilisateur normal → on cache simplement la fenêtre
            Hide;
            TrayIcon1.Visible := True;
            Action := caNone; //Empêche la fermeture complète
        end;
    end;
end;

(*===================== GESTION DES ACTIONS DU ME?NU  =============================================================================*)

procedure TFormAppWatcher.Quitter1Click(Sender: TObject);
begin

    if GetPassword then
    begin
        FRequestingClose := True;
        close;
    end;
end;

procedure TFormAppWatcher.Voir1Click(Sender: TObject);
begin
    //Memo1.Clear;
    Show;
    application.BringToFront;
end;

(*===================== BOUCLE DE TRAITEMENT  =============================================================================*)

procedure TFormAppWatcher.Timer1Timer(Sender: TObject);

var
    i:                      Integer;
    StopDialog:             TStopDialog;
    MsgReceived:            TAppWatcherMessage;
    ReconnectionAuthorized: Boolean;
begin
    try
        Timer1.enabled := False;
        if FRequestingClose then
            close;

        ProcessQueuedMessages; //🔄 Traitement des messages en attente

        //On ne charge pas le fichier config périodiquement
        if (Now - FLastConfigLoad) * 86400000 >= LOADCONFIG_INTERVAL then
        Begin
            LoadConfig;
            FLastConfigLoad := Now;
        End;

        //📡 🔄 Vérifier et maintenir la connexion au MASTER
        if not IdTCPClientMASTER.Connected then
        begin
            if (Now - FLastReconnectAttempt) * 86400000 < RECONNECT_INTERVAL then
                Exit; //⏳ On attend avant de réessayer

            FLastReconnectAttempt := Now;
            try
                IdTCPClientMASTER.Connect;
                Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CONNECTED'));
            except
                on E: Exception do
                begin
                    if FSignalErrorConnect then begin
                        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'ERROR_CONNECTING'), [E.Message]));
                        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'RECONNECTING'));
                        FSignalErrorConnect := False;

                    End;
                    Exit;
                end;
            end;
            FSignalErrorConnect := True;
        end;

        //📩 🔎 Lire les messages du MASTER
        while ReadMessage(IdTCPClientMASTER.IOHandler, MsgReceived) do
        begin
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'COMMAND_RECEIVED'), [MsgReceived.CmdName]));
            HandleMasterMessage(MsgReceived);
        end;

        //⏳ 🔥 Vérifier les comptes à rebours des arrêts demandés
        for i := 0 to FStopDialogs.Count - 1 do
        begin
            StopDialog := FStopDialogs.FList[i];
            if StopDialog.State = dsInit then
            begin
                Dec(StopDialog.CountDown);

                if Assigned(StopDialog.FormStop) then
                    StopDialog.FormStop.StepCountdown;

                if StopDialog.CountDown <= 0 then
                begin
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_EXEC'), [StopDialog.AppName]));

                    //✅ Marquer STOP comme déjà envoyé
                    StopDialog.State := dsStopRequested;
                    SendStopCommandToCLIENT(StopDialog);
                end;
            end;
        end;

    finally
            Timer1.enabled := True;
    end;
end;

procedure TFormAppWatcher.SendStopCommandToCLIENT(var StopDialog: TStopDialog);
var
    Context:     TIdContext;
    ContextList: TList;
    Msg:         TAppWatcherMessage;
begin
    //Prépare le message de stop
    Msg.Init(StopDialog.Handle, cmdSTOP, StopDialog.AppPath, StopDialog.Params, 0);

    //On l'envoi à tout le monde mais le client qui possède ce Handle répondra  et les autres pourront ignorer le message
    ContextList := IdTCPServerCLIENT.Contexts.LockList;
    try
        for Context in ContextList do
        begin
            try
                Msg.SendMessage(Context.Connection.IOHandler);
                Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_SENT'), [Context.Binding.PeerIP]));
            except
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_SENT_ERROR'), [Context.Binding.PeerIP]));
            end;
        end;
    finally
            IdTCPServerCLIENT.Contexts.UnlockList;
    end;
end;

(*=============================== TRAITEMENT DES MESSAGES DU MASTER ===================================================================*)

procedure TFormAppWatcher.HandleMasterMessage(var Msg: TAppWatcherMessage);
begin
    case Msg.Command of
        cmdWHO:
            HandleMASTERWhoRequest;
        cmdSTOP:
            HandleMASTERStopRequest(Msg);
        cmdSTART:
            HandleMASTERStartRequest;
        cmdCANCEL:
            HandleMASTERCancelRequest;
        cmdSTOP_AGENT:
            HandleMASTERStopAgent;
    end;
end;

procedure TFormAppWatcher.HandleMASTERStopAgent;
var
    Msg: TAppWatcherMessage;
begin
    Msg.Init(0, cmdACK, ParamStr(0), '', 0);
    if IdTCPClientMASTER.Connected then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);
    //On empêche de refuser la fermeture
    FRequestingClose := True;
    OnCloseQuery := nil;
    close;
End;

procedure TFormAppWatcher.HandleMASTERWhoRequest;
//Demande de WHO du MASTER
var
    Context:       TIdContext;
    ContextList:   TList;
    ConnectedApps: TStringList;
    AnswerMsg:     TAppWatcherMessage;
begin
    ConnectedApps := TStringList.Create;
    try
        ContextList := IdTCPServerCLIENT.Contexts.LockList;
        try
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_COUNT'), [IntToStr(ContextList.Count)]));
            for Context in ContextList do
            begin
                try
                    //Demander le nom de l'application connectée
                    AnswerMsg.Init(0, cmdWHO, '', '', 0);
                    AnswerMsg.SendMessage(Context.Connection.IOHandler);
                    ConnectedApps.Add(Context.Binding.PeerIP);
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'WHO_SEND'), [Context.Binding.PeerIP]));

                except
                    //Ignorer les erreurs de lecture (client déconnecté)
                end;
            end;
        finally
                IdTCPServerCLIENT.Contexts.UnlockList;
        end;

        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_LIST_SENT'), [ConnectedApps.CommaText]));
    finally
            ConnectedApps.Free;
    end;
end;

procedure TFormAppWatcher.HandleMASTERStopRequest(var Msg: TAppWatcherMessage);
//Demande de STOP
var
    Context:        TIdContext;
    ContextList:    TList;
    StopRequestMsg: TAppWatcherMessage;
begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_RECEIVED'), [Msg.AppName]));

    StopRequestMsg.Init(0, cmdSTOP_REQUEST, Msg.AppPath, Msg.Params, Msg.Duration);

    ContextList := IdTCPServerCLIENT.Contexts.LockList;
    try
        for Context in ContextList do
        begin
            try
                StopRequestMsg.SendMessage(Context.Connection.IOHandler);
                Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_SEND'));
            except
                    Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_FAILED'));
            end;
        end;
    finally
            IdTCPServerCLIENT.Contexts.UnlockList;
    end;
end;

procedure TFormAppWatcher.HandleMASTERStartRequest;
//Demande de START
var
    StopForm:                    TFormStopNotification;
    AppName, AppPath, AppParams: string;
    i, SeparatorPos:             Integer;
begin
    Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'START_RECEIVED'));
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'RESTART_STOP_LIST'), [FStopDialogs.AppList]));
    if FormAppWatcher.FStopDialogs.AppList = '' then
    begin

        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'START_NOAPP'));
    end else begin

        for i := FStopDialogs.Count - 1 downto 0 do
        begin

            if FStopDialogs.FList[i].State <> dsStopped then
                Continue;

            //Extraction de AppPath et AppParams
            AppPath := FStopDialogs.FList[i].AppPath;
            AppParams := FStopDialogs.FList[i].Params;

            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'TRY_RESTART'), [AppPath, AppParams]));

            if FileExists(AppPath,true) then
            begin
                if ShellExecute(0, 'open', PChar(AppPath), PChar(AppParams), nil, SW_SHOWNORMAL) > 32 then
                begin

                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'APP_RESTARTED'), [AppPath]));
                    StopForm := FormAppWatcher.FStopDialogs.FList[i].FormStop;

                    if Assigned(StopForm) then begin
                        StopForm.Show;
                        StopForm.NotifyRestart;
                    End;

                    FStopDialogs.FList[i].State := dsRestarted;
                end
                else
                begin
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'RESTART_ERROR'), [AppPath]));
                end;
            end
            else
            begin
                Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'APP_NOT_FOUND'), [AppPath]));
            end;
        end;
    end;

end;

procedure TFormAppWatcher.HandleMASTERCancelRequest;
//demande de CANCEL
var
    StopDialog:       TStopDialog;
    StopForm:         TFormStopNotification;
    AppPath, AppName: string;
    i, SeparatorPos:  Integer;
    CancelCount:      Integer;
Begin
    Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CANCEL_RECEIVED'));
    CancelCount := 0;
    //On annule tout les dialogues de stop
    for i := 0 to FStopDialogs.Count - 1 do
    begin
        StopDialog := FStopDialogs.FList[i];
        StopForm := StopDialog.FormStop;
        AppName := StopDialog.AppName;
        if ((StopDialog.State = dsInit) or (StopDialog.State = dsStopRequested)) then
        begin
            if Assigned(StopForm) then
                StopForm.CancelStop;
            StopDialog.State := dsCancel;
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CANCEL_EXEC'), [AppName]));

            inc(CancelCount);
        end;

    end;

    //📝 Affichage d’un message si rien a  annuler
    if CancelCount = 0 then
        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CANCEL_EMPTY'));

End;

(*===================== TRAITEMENT DES MESSAGES EN PROVENANCE DES CLIENTS =============================================================================*)

procedure TFormAppWatcher.IdTCPServerCLIENTExecute(AContext: TIdContext);
//Traitement des messages clients - Ajout à la file d'attente pour traitement ultérieur par le timer
var
    MsgReceived: TAppWatcherMessage;
begin
    if not ReadMessage(AContext.Connection.IOHandler, MsgReceived) then
    begin
        sleep(300);
        Exit;
    end;

    FQueueLock.Enter; //🔒 Verrouillage de la file
    try
            FMessageQueue.Enqueue(MsgReceived); //📩 Ajout en file d’attente
    finally
            FQueueLock.Leave; //🔓 Libération
    end;
end;

procedure TFormAppWatcher.ProcessQueuedMessages;
var
    Msg: TAppWatcherMessage;
begin
    FQueueLock.Enter; //🔒 Sécurisation de l’accès à la file
    try
        while FMessageQueue.Count > 0 do
        begin
            Msg := FMessageQueue.Dequeue; //📤 Extraire le premier message

            case Msg.Command of
                cmdWHO_REPLY:
                    HandleCLIENTWhoReply(Msg);
                cmdREPLY_STOP_REQUEST:
                    HandleCLIENTStopRequestReply(Msg);
                cmdACK:
                    HandleCLIENTAckReply(Msg);
                cmdNACK:
                    HandleCLIENTNackReply(Msg);
            end;
        end;
    finally
            FQueueLock.Leave; //🔓 Libération
    end;
end;

procedure TFormAppWatcher.HandleCLIENTWhoReply(Msg: TAppWatcherMessage);
//La réponse à un WHO est envoyée au MASTER tel quel
begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_CONNECTED'), [Msg.AppName]));
    //Envoyer la réponse immédiatement au Master
    if IdTCPClientMASTER.Connected then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);

end;

procedure TFormAppWatcher.HandleCLIENTStopRequestReply(Msg: TAppWatcherMessage);
//La réponse au STOP_REQUEST doit être enregistrée pour démarrer le décompte et arrêter le client plus tard
var
    StopDialog: TStopDialog;
    idx:        Integer;
begin

    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_REPLY'), [Msg.AppName, IntToStr(Msg.Handle)]));

    //Ajoute le dialogue avec le HANDLE du CLIENT
    StopDialog := FStopDialogs.AddOrReplace(Msg, FLanguageManager);

    //Si le dialogue existe déjà on le remet à l'état init ?
    if (StopDialog.State = dsInit) or (StopDialog.State = dsCancel) or (StopDialog.State = dsRestarted) then begin
        StopDialog.State := dsInit;
        StopDialog.CountDown := Msg.Duration; //Same as Timer1 TimerInterval = 1000 (1 sec.)
        StopDialog.FormStop.Show;
        StopDialog.FormStop.StartCountdown(Msg.Duration, Msg.AppName);
        //🛑 Ajoute un log pour voir la valeur de Duration
        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_COUNTDOWN'), [StopDialog.AppName, StopDialog.CountDown]));
    end;

end;

procedure TFormAppWatcher.HandleCLIENTAckReply(Msg: TAppWatcherMessage);
//réponse au STOP
//Si le message commence par "ACK ", Le client à accepté le STOP et s'est arrêté
//Dans ce cas on passe le dialogue à dsStopped
var
    StopDialog: TStopDialog;
    idx:        Integer;
begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_STOPPED'), [Msg.AppName, Msg.AppPath]));
    if FStopDialogs.TryGetStopDialog(Msg.Handle, Msg.AppName, StopDialog, idx) then begin
        if Assigned(StopDialog.FormStop) then
            StopDialog.FormStop.NotifyStop;

        StopDialog.State := dsStopped;
    end;

    if IdTCPClientMASTER.Connected then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);
end;

procedure TFormAppWatcher.HandleCLIENTNackReply(Msg: TAppWatcherMessage);
//réponse au STOP
//Si le message commence par "NACK ", c'est une réponse négative à un STOP
//Dans ce cas il faut annuler la demande de stop en passant le dialogue à dsCancel
var
    StopDialog: TStopDialog;
    idx:        Integer;
begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_STOP_REFUSED'), [Msg.AppName + ' - ' + IntToStr(Msg.Handle)]));
    if FStopDialogs.TryGetStopDialog(Msg.Handle, Msg.AppName, StopDialog, idx) then begin
        if Assigned(StopDialog.FormStop) then
            StopDialog.FormStop.CancelStop;

        StopDialog.State := dsCancel;
    end;

    if IdTCPClientMASTER.Connected then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);

end;

end.
