(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherAgent_main.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 2.0.0
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
  - [25/02/2025] : v1.3.2 - Fixed issue where all applications stopped instead of only the requested one
                          - Ensured restarted applications are launched in their original working directory
  - [06/03/2025] : v2.0 - Use of New  TAppWatcherMessage with Packed Record
                         !!! This change makes the version incompatible with v1 !!!
                        - Fixed HandleCLIENTAckReply which did  forget to transmit the Parameters : StopDialog.Params:=Msg.Params;
                        - Load the .ini file only if it has been modified.

  Notes :
  -------
  This project is **open-source**. Contributions and improvements are welcome!
  *******************************************************************************)

Unit AppWatcherAgent_main;

Interface

Uses
    Winapi.Windows, Winapi.Messages, Winapi.ShellAPI, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Tlhelp32, System.Generics.Collections,
    Vcl.ExtCtrls, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
    IdTCPServer, System.IniFiles, IdTCPConnection, IdTCPClient, IdException, AppWatcherAgent_Stop,
    AppWatcher_ioHandler, AppWatcher_Lang, System.SyncObjs,System.IOUtils,
    AppWatcher_consts;

Type

    TDialogState = (dsInit, dsStopRequested, dsStopped, dsRestarted, dsCancel);

    TStopDialog = Class
        CountDown: Integer;
        Handle: HWND;
        AppName: String; //Nom
        AppPath: String; //Chemin complet
        Params: String; //Arguments
        FormStop: TFormStopNotification;
        State: TDialogState;
        Constructor Create(LanguageManager: TAppLangManager);
        Destructor Destroy; override;
    End;

    TStoppedDialogs = Class
    private
        FList: TList<TStopDialog>;
    public
        Constructor Create;
        Destructor Destroy; override;
        Procedure Clear;
        Function TryGetStopDialog(Handle: HWND; AppName: String; Var StopDialog: TStopDialog; Var idx: Integer): Boolean;
        Function AddOrReplace(AppWatcherMsg: TAppWatcherMessage; LanguageManager: TAppLangManager): TStopDialog;
        Function AppList: String;
        Function Count: Integer;
        Procedure Delete(idx: Integer);
        Procedure Restart(idx: Integer);
        Property Items: TList<TStopDialog>Read FList;
    End;

    TFormAppWatcher = Class(TForm)
        Memo1: TMemo;
        TrayIcon1: TTrayIcon;
        PopupMenu1: TPopupMenu;
        Voir1: TMenuItem;
        IdTCPServerCLIENT: TIdTCPServer;
        IdTCPClientMASTER: TIdTCPClient;
        Timer1: TTimer;
        Quitter1: TMenuItem;
        Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
        Procedure FormCreate(Sender: TObject);
        Procedure IdTCPServerCLIENTExecute(AContext: TIdContext);
        Procedure Quitter1Click(Sender: TObject);
        Procedure Timer1Timer(Sender: TObject);
        Procedure Voir1Click(Sender: TObject);
    private
        {Déclarations privées}
        FServerIP: String;
        FServerPort: Integer;
        FStopTimeOut: Integer;
        FCLientPort: Integer;
        FClientInterval: Integer;
        FStopDialogs: TStoppedDialogs;
        FLastReconnectAttempt: TDateTime;
        FLastConfigLoad: TDateTime;
        FMessageQueue: TQueue<TAppWatcherMessage>; //File d’attente des messages
        FQueueLock: TCriticalSection; //🔒 Protection d'accès à la file
        FSignalErrorConnect: Boolean;
        FRequestingClose: Boolean;
        FLang: TAppWatcherLang;
        FLanguageManager: TAppLangManager;
        FLastIniModified: TDateTime;
        Function GetPassword: Boolean;
        Procedure ProcessQueuedMessages; //🔄 Fonction de traitement dans Timer1
        Procedure WM_SYSCOMMAND(Var Msg: TMessage); message WM_SYSCOMMAND;
        Procedure LoadConfig;

        Procedure HandleMasterMessage(Var Msg: TAppWatcherMessage);
        Procedure HandleMASTERWhoRequest;
        Procedure HandleMASTERStopRequest(Var Msg: TAppWatcherMessage);
        Procedure HandleMASTERStartRequest;
        Procedure HandleMASTERCancelRequest;
        Procedure HandleMASTERStopAgent;

        Procedure HandleCLIENTWhoReply(Msg: TAppWatcherMessage);
        Procedure HandleCLIENTStopRequestReply(Msg: TAppWatcherMessage);
        Procedure HandleCLIENTAckReply(Msg: TAppWatcherMessage);
        Procedure HandleCLIENTNackReply(Msg: TAppWatcherMessage);

        Procedure SendStopCommandToCLIENT(Var StopDialog: TStopDialog);
    public
        {Déclarations publiques}
    End;

Var
    FormAppWatcher: TFormAppWatcher;

Const
    RECONNECT_INTERVAL = 10000; //10 secondes
    LOADCONFIG_INTERVAL = 5000; //5 secondes

Implementation

{$R *.dfm}

Var
    DialogLock: TCriticalSection;

    (*============================== GESTION  DIALOGUE DE STOP ====================================================================*)

Constructor TStopDialog.Create(LanguageManager: TAppLangManager);
Begin
    Inherited Create;
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

Destructor TStopDialog.Destroy;
Begin
    FormStop.Free;
    Inherited;
End;

(*============================== GESTION DE LA LISTE DES DIALOGUES DE STOP ====================================================================*)

Constructor TStoppedDialogs.Create;
Begin
    Inherited Create;
    FList := TList<TStopDialog>.Create;
End;

Destructor TStoppedDialogs.Destroy;
Begin
    Clear;

    FList.Free;
    Inherited;
End;

Procedure TStoppedDialogs.Clear;
Var
    i: Integer;
Begin
    For i := FList.Count - 1 Downto 0 Do Begin
        If Assigned(FList[i]) Then Begin
            FList[i].Free; //✅ Libération explicite de la mémoire
            FList[i] := Nil; //🔧 Éviter tout accès invalide
        End;
    End;
    FList.Clear; //Maintenant on peut vider la liste en toute sécurité
End;

Function TStoppedDialogs.TryGetStopDialog(Handle: HWND; AppName: String; Var StopDialog: TStopDialog; Var idx: Integer): Boolean;
Var
    i: Integer;
Begin
    Result := False;
    idx := -1;
    For i := 0 To FList.Count - 1 Do Begin
        If (FList[i].Handle = Handle) And (FList[i].AppName = AppName) Then Begin
            idx := i;
            StopDialog := FList[i];
            Exit(True);
        End;
    End;
End;

Function TStoppedDialogs.AddOrReplace(AppWatcherMsg: TAppWatcherMessage; LanguageManager: TAppLangManager): TStopDialog;
Var
    idx: Integer;
Begin

    //Recherche d'un élément existant
    If TryGetStopDialog(AppWatcherMsg.Handle, AppWatcherMsg.AppName, Result, idx) Then Begin
        //Mise à jour de l'entrée existante
        Result.AppPath := AppWatcherMsg.AppPath;
        Result.Params := AppWatcherMsg.Params;
        Exit;
    End;

    //Ajout d'une nouvelle entrée
    Result := TStopDialog.Create(LanguageManager);

    Result.Handle := AppWatcherMsg.Handle;
    Result.AppName := AppWatcherMsg.AppName;
    Result.AppPath := AppWatcherMsg.AppPath;
    Result.Params := AppWatcherMsg.Params;
    Result.CountDown := AppWatcherMsg.Duration;
    FList.Add(Result);

End;

Function TStoppedDialogs.AppList: String;
Var
    i: Integer;
Begin
    Result := '';
    For i := 0 To FList.Count - 1 Do
        If FList[i].State = dsStopped Then
            If i = 0 Then
                Result := FList[i].AppName
            Else
                Result := Result + #13#10 + FList[i].AppName;

End;

Function TStoppedDialogs.Count: Integer;
Begin
    Result := FList.Count;
End;

Procedure TStoppedDialogs.Delete(idx: Integer);
Begin
    If (idx >= 0) And (idx < FList.Count) Then Begin
        FList[idx].Free;
        FList.Delete(idx);
    End;
End;

Procedure TStoppedDialogs.Restart(idx: Integer);
Var
    CurDialog: TStopDialog;
Begin
    If (idx >= 0) And (idx < FList.Count) Then
        FList[idx].State := dsRestarted;
End;

(*============================== DEMARRAGE ET INITIALISATIONS ====================================================================*)

Procedure TFormAppWatcher.LoadConfig;
Var
    Ini: TMemIniFile;
    IniFilePathName, Msg: String;
Begin
    //🔹 Trouver le fichier de configuration
    IniFilePathName := FindConfigPath(AppWatcherIniFileNAme);

    If IniFilePathName = '' Then Begin
        //Le fichier INI est introuvable
        If FLang = LangEn Then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileNAme])
        Else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileNAme]);

        MessageDlg(Msg, mtError, [mbok], 0);
        FRequestingClose := True;
        Exit;
    End;

    If FLastIniModified <> TFile.GetLastWriteTime(IniFilePathName) Then Begin
        FLastIniModified := TFile.GetLastWriteTime(IniFilePathName);
        //🔹 Charger le fichier INI
        Ini := TMemIniFile.Create(IniFilePathName);
        Try
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

        Finally
            Ini.Free;
        End;
    End;
End;

Procedure TFormAppWatcher.WM_SYSCOMMAND(Var Msg: TMessage);
Begin
    If (Msg.WParam = SC_MINIMIZE) Then Begin
        Hide; //Cache la fenêtre
        TrayIcon1.Visible := True; //Affiche l'icône dans la zone de notification
        Exit;
    End;
    Inherited;
End;

Procedure TFormAppWatcher.FormCreate(Sender: TObject);
Var
    LangValue: String;
    Msg: String;
    LanguageLoaded: Boolean;
Begin
    //🔹 Trouver le fichier de configuration
    FLastConfigLoad := Now;
    FRequestingClose := False;
    FLastIniModified := EncodeDate(1900, 1, 1);

    //🔹 Vérification des paramètres en ligne de commande (--lang fr ou --lang en)
    If FindCmdLineSwitch('lang', LangValue, True, [clstValueNextParam, clstValueAppended]) Then Begin
        LangValue := LowerCase(LangValue);
        If (LangValue <> 'fr') And (LangValue <> 'en') Then
            FLang := LangFr; //🔴 Sécurisation : éviter une langue inconnue
    End Else
        FLang := LangFr;

    LoadConfig; //Charge le fichier INI

    FLanguageManager := TAppLangManager.Create(FLang);

    //🔹 Chargement de la langue
    //🔴 Si le fichier est introuvable, afficher un message et quitter
    If Not FLanguageManager.LoadLanguage(FLang) Then Begin
        //Le fichier INI est introuvable
        If FLang = LangEn Then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileNAme])
        Else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileNAme]);
        MessageDlg(Msg, mtError, [mbok], 0);

        FreeAndNil(FLanguageManager);
        FRequestingClose := True;

    End;

    If FRequestingClose Then
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
End;

Function TFormAppWatcher.GetPassword: Boolean;
Var
    Password: String;
    CloseQryF: TInputCloseQueryFunc;
    PasswordArray: Array Of String;
Begin
    Result := False;
    Password := '';
    Password := InputBox(FLanguageManager.GetMessage('AGENT', 'PROTECT'), #0 + FLanguageManager.GetMessage('AGENT', 'PASSWORD_REQUEST'), '');

    If Password = AppWPassword Then Begin
        Result := True;
    End Else
        ShowMessage(FLanguageManager.GetMessage('AGENT', 'INVALID_PASSWORD'));
End;

Procedure TFormAppWatcher.FormClose(Sender: TObject; Var Action: TCloseAction);
Var
    Password: String;
Begin
    If (application.Terminated) Or (GetSystemMetrics(SM_SHUTTINGDOWN) = 1) Or (FRequestingClose) Then Begin
        Timer1.enabled := False; //✅ Empêcher toute exécution pendant la fermeture

        //Windows ferme la session → on ferme normalement
        TrayIcon1.Visible := False;
        FStopDialogs.Free;
        FreeAndNil(FLanguageManager);
        FQueueLock.Enter;
        Try
            FMessageQueue.Clear;
            FreeAndNil(FMessageQueue);
        Finally
            FQueueLock.Leave;
            FreeAndNil(FQueueLock);
        End;
        Action := caFree;

    End Else Begin
        //Vérifier si l'utilisateur est autorisé à fermer
        If (GetAsyncKeyState(VK_CONTROL) < 0) And (GetAsyncKeyState(VK_SHIFT) < 0) Then Begin
            If Not GetPassword Then
                Action := caNone; //Empêche la fermeture complète
        End Else Begin
            //L'utilisateur normal → on cache simplement la fenêtre
            Hide;
            TrayIcon1.Visible := True;
            Action := caNone; //Empêche la fermeture complète
        End;
    End;
End;

(*===================== GESTION DES ACTIONS DU ME?NU  =============================================================================*)

Procedure TFormAppWatcher.Quitter1Click(Sender: TObject);
Begin

    If GetPassword Then Begin
        FRequestingClose := True;
        close;
    End;
End;

Procedure TFormAppWatcher.Voir1Click(Sender: TObject);
Begin
    //Memo1.Clear;
    Show;
    application.BringToFront;
End;

(*===================== BOUCLE DE TRAITEMENT  =============================================================================*)

Procedure TFormAppWatcher.Timer1Timer(Sender: TObject);

Var
    i: Integer;
    StopDialog: TStopDialog;
    MsgReceived: TAppWatcherMessage;
    ReconnectionAuthorized: Boolean;
Begin
    Try
        Timer1.enabled := False;
        If FRequestingClose Then
            close;

        ProcessQueuedMessages; //🔄 Traitement des messages en attente

        //On ne charge pas le fichier config périodiquement
        If (Now - FLastConfigLoad) * 86400000 >= LOADCONFIG_INTERVAL Then Begin
            LoadConfig;
            FLastConfigLoad := Now;
        End;

        //📡 🔄 Vérifier et maintenir la connexion au MASTER
        If Not IdTCPClientMASTER.Connected Then Begin
            If (Now - FLastReconnectAttempt) * 86400000 < RECONNECT_INTERVAL Then
                Exit; //⏳ On attend avant de réessayer

            FLastReconnectAttempt := Now;
            Try
                IdTCPClientMASTER.Connect;
                Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CONNECTED'));
            Except
                On E: Exception Do Begin
                    If FSignalErrorConnect Then Begin
                        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'ERROR_CONNECTING'), [E.Message]));
                        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'RECONNECTING'));
                        FSignalErrorConnect := False;

                    End;
                    Exit;
                End;
            End;
            FSignalErrorConnect := True;
        End;

        //📩 🔎 Lire les messages du MASTER
        While ReadMessage(IdTCPClientMASTER.IOHandler, MsgReceived) Do Begin
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'COMMAND_RECEIVED'), [MsgReceived.CmdName]));
            HandleMasterMessage(MsgReceived);
        End;

        //⏳ 🔥 Vérifier les comptes à rebours des arrêts demandés
        For i := 0 To FStopDialogs.Count - 1 Do Begin
            StopDialog := FStopDialogs.FList[i];
            If StopDialog.State = dsInit Then Begin
                Dec(StopDialog.CountDown);

                If Assigned(StopDialog.FormStop) Then
                    StopDialog.FormStop.StepCountdown;

                If StopDialog.CountDown <= 0 Then Begin
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_EXEC'), [StopDialog.AppName]));

                    //✅ Marquer STOP comme déjà envoyé
                    StopDialog.State := dsStopRequested;
                    SendStopCommandToCLIENT(StopDialog);
                End;
            End;
        End;

    Finally
        Timer1.enabled := True;
    End;
End;

Procedure TFormAppWatcher.SendStopCommandToCLIENT(Var StopDialog: TStopDialog);
Var
    Context: TIdContext;
    ContextList: TList;
    Msg: TAppWatcherMessage;
Begin
    //Prépare le message de stop
    Msg.Init(StopDialog.Handle, cmdSTOP, StopDialog.AppPath, StopDialog.Params, 0);

    //On l'envoi à tout le monde mais le client qui possède ce Handle répondra  et les autres pourront ignorer le message
    ContextList := IdTCPServerCLIENT.Contexts.LockList;
    Try
        For Context In ContextList Do Begin
            Try
                Msg.SendMessage(Context.Connection.IOHandler);
                Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_SENT'), [Context.Binding.PeerIP]));
            Except
                Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_SENT_ERROR'), [Context.Binding.PeerIP]));
            End;
        End;
    Finally
        IdTCPServerCLIENT.Contexts.UnlockList;
    End;
End;

(*=============================== TRAITEMENT DES MESSAGES DU MASTER ===================================================================*)

Procedure TFormAppWatcher.HandleMasterMessage(Var Msg: TAppWatcherMessage);
Begin
    Case Msg.Command Of
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
    End;
End;

Procedure TFormAppWatcher.HandleMASTERStopAgent;
Var
    Msg: TAppWatcherMessage;
Begin
    Msg.Init(0, cmdACK, ParamStr(0), '', 0);
    If IdTCPClientMASTER.Connected Then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);
    //On empêche de refuser la fermeture
    FRequestingClose := True;
    OnCloseQuery := Nil;
    close;
End;

Procedure TFormAppWatcher.HandleMASTERWhoRequest;
//Demande de WHO du MASTER
Var
    Context: TIdContext;
    ContextList: TList;
    ConnectedApps: TStringList;
    AnswerMsg: TAppWatcherMessage;
Begin
    ConnectedApps := TStringList.Create;
    Try
        ContextList := IdTCPServerCLIENT.Contexts.LockList;
        Try
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_COUNT'), [IntToStr(ContextList.Count)]));
            For Context In ContextList Do Begin
                Try
                    //Demander le nom de l'application connectée
                    AnswerMsg.Init(0, cmdWHO, '', '', 0);
                    AnswerMsg.SendMessage(Context.Connection.IOHandler);
                    ConnectedApps.Add(Context.Binding.PeerIP);
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'WHO_SEND'), [Context.Binding.PeerIP]));

                Except
                    //Ignorer les erreurs de lecture (client déconnecté)
                End;
            End;
        Finally
            IdTCPServerCLIENT.Contexts.UnlockList;
        End;

        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_LIST_SENT'), [ConnectedApps.CommaText]));
    Finally
        ConnectedApps.Free;
    End;
End;

Procedure TFormAppWatcher.HandleMASTERStopRequest(Var Msg: TAppWatcherMessage);
//Demande de STOP
Var
    Context: TIdContext;
    ContextList: TList;
    StopRequestMsg: TAppWatcherMessage;
Begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_RECEIVED'), [Msg.AppName]));

    StopRequestMsg.Init(0, cmdSTOP_REQUEST, Msg.AppPath, Msg.Params, Msg.Duration);

    ContextList := IdTCPServerCLIENT.Contexts.LockList;
    Try
        For Context In ContextList Do Begin
            Try
                StopRequestMsg.SendMessage(Context.Connection.IOHandler);
                Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_SEND'));
            Except
                Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'STOP_REQUEST_FAILED'));
            End;
        End;
    Finally
        IdTCPServerCLIENT.Contexts.UnlockList;
    End;
End;

Procedure TFormAppWatcher.HandleMASTERStartRequest;
//Demande de START
Var
    StopForm: TFormStopNotification;
    AppName, AppPath, AppParams: String;
    i, SeparatorPos: Integer;
Begin
    Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'START_RECEIVED'));
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'RESTART_STOP_LIST'), [FStopDialogs.AppList]));
    If FormAppWatcher.FStopDialogs.AppList = '' Then Begin

        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'START_NOAPP'));
    End Else Begin

        For i := FStopDialogs.Count - 1 Downto 0 Do Begin

            If FStopDialogs.FList[i].State <> dsStopped Then
                Continue;

            //Extraction de AppPath et AppParams
            AppPath := FStopDialogs.FList[i].AppPath;
            AppParams := FStopDialogs.FList[i].Params;

            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'TRY_RESTART'), [AppPath, AppParams]));

            If FileExists(AppPath, True) Then Begin
                If ShellExecute(0, 'open', PChar(AppPath), PChar(AppParams), PChar(ExtractFilePath(AppPath)), SW_SHOWNORMAL) > 32 Then Begin
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'APP_RESTARTED'), [AppPath]));
                    StopForm := FormAppWatcher.FStopDialogs.FList[i].FormStop;

                    If Assigned(StopForm) Then Begin
                        StopForm.Show;
                        StopForm.NotifyRestart;
                    End;

                    FStopDialogs.FList[i].State := dsRestarted;
                End Else Begin
                    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'RESTART_ERROR'), [AppPath]));
                End;
            End Else Begin
                Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'APP_NOT_FOUND'), [AppPath]));
            End;

        End;
    End;

End;

Procedure TFormAppWatcher.HandleMASTERCancelRequest;
//demande de CANCEL
Var
    StopDialog: TStopDialog;
    StopForm: TFormStopNotification;
    AppPath, AppName: String;
    i, SeparatorPos: Integer;
    CancelCount: Integer;
Begin
    Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CANCEL_RECEIVED'));
    CancelCount := 0;
    //On annule tout les dialogues de stop
    For i := 0 To FStopDialogs.Count - 1 Do Begin
        StopDialog := FStopDialogs.FList[i];
        StopForm := StopDialog.FormStop;
        AppName := StopDialog.AppName;
        If ((StopDialog.State = dsInit) Or (StopDialog.State = dsStopRequested)) Then Begin
            If Assigned(StopForm) Then
                StopForm.CancelStop;
            StopDialog.State := dsCancel;
            Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CANCEL_EXEC'), [AppName]));

            inc(CancelCount);
        End;

    End;

    //📝 Affichage d’un message si rien a  annuler
    If CancelCount = 0 Then
        Memo1.Lines.Add(FLanguageManager.GetMessage('AGENT', 'CANCEL_EMPTY'));

End;

(*===================== TRAITEMENT DES MESSAGES EN PROVENANCE DES CLIENTS =============================================================================*)

Procedure TFormAppWatcher.IdTCPServerCLIENTExecute(AContext: TIdContext);
//Traitement des messages clients - Ajout à la file d'attente pour traitement ultérieur par le timer
Var
    MsgReceived: TAppWatcherMessage;
Begin
    If Not ReadMessage(AContext.Connection.IOHandler, MsgReceived) Then Begin
        sleep(300);
        Exit;
    End;

    FQueueLock.Enter; //🔒 Verrouillage de la file
    Try
        FMessageQueue.Enqueue(MsgReceived); //📩 Ajout en file d’attente
    Finally
        FQueueLock.Leave; //🔓 Libération
    End;
End;

Procedure TFormAppWatcher.ProcessQueuedMessages;
Var
    Msg: TAppWatcherMessage;
Begin
    FQueueLock.Enter; //🔒 Sécurisation de l’accès à la file
    Try
        While FMessageQueue.Count > 0 Do Begin
            Msg := FMessageQueue.Dequeue; //📤 Extraire le premier message

            Case Msg.Command Of
                cmdWHO_REPLY:
                    HandleCLIENTWhoReply(Msg);
                cmdREPLY_STOP_REQUEST:
                    HandleCLIENTStopRequestReply(Msg);
                cmdACK:
                    HandleCLIENTAckReply(Msg);
                cmdNACK:
                    HandleCLIENTNackReply(Msg);
            End;
        End;
    Finally
        FQueueLock.Leave; //🔓 Libération
    End;
End;

Procedure TFormAppWatcher.HandleCLIENTWhoReply(Msg: TAppWatcherMessage);
//La réponse à un WHO est envoyée au MASTER tel quel
Begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_CONNECTED'), [Msg.AppName]));
    //Envoyer la réponse immédiatement au Master
    If IdTCPClientMASTER.Connected Then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);

End;

Procedure TFormAppWatcher.HandleCLIENTStopRequestReply(Msg: TAppWatcherMessage);
//La réponse au STOP_REQUEST doit être enregistrée pour démarrer le décompte et arrêter le client plus tard
Var
    StopDialog: TStopDialog;
    idx: Integer;
Begin

    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_REPLY'), [Msg.AppName, IntToStr(Msg.Handle)]));

    //Ajoute le dialogue avec le HANDLE du CLIENT
    StopDialog := FStopDialogs.AddOrReplace(Msg, FLanguageManager);

    //Si le dialogue existe déjà on le remet à l'état init ?
    If (StopDialog.State = dsInit) Or (StopDialog.State = dsCancel) Or (StopDialog.State = dsRestarted) Then Begin
        StopDialog.State := dsInit;
        StopDialog.CountDown := Msg.Duration; //Same as Timer1 TimerInterval = 1000 (1 sec.)
        StopDialog.FormStop.Show;
        StopDialog.FormStop.StartCountdown(Msg.Duration, Msg.AppName);
        //🛑 Ajoute un log pour voir la valeur de Duration
        Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'STOP_COUNTDOWN'), [StopDialog.AppName, StopDialog.CountDown]));
    End;

End;

Procedure TFormAppWatcher.HandleCLIENTAckReply(Msg: TAppWatcherMessage);
//réponse au STOP
//Si le message commence par "ACK ", Le client à accepté le STOP et s'est arrêté
//Dans ce cas on passe le dialogue à dsStopped
Var
    StopDialog: TStopDialog;
    idx: Integer;
Begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_STOPPED'), [Msg.AppName, Msg.AppPath]));
    If FStopDialogs.TryGetStopDialog(Msg.Handle, Msg.AppName, StopDialog, idx) Then Begin
        StopDialog.Params := Msg.Params;

        If Assigned(StopDialog.FormStop) Then
            StopDialog.FormStop.NotifyStop;

        StopDialog.State := dsStopped;
    End;

    If IdTCPClientMASTER.Connected Then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);
End;

Procedure TFormAppWatcher.HandleCLIENTNackReply(Msg: TAppWatcherMessage);
//réponse au STOP
//Si le message commence par "NACK ", c'est une réponse négative à un STOP
//Dans ce cas il faut annuler la demande de stop en passant le dialogue à dsCancel
Var
    StopDialog: TStopDialog;
    idx: Integer;
Begin
    Memo1.Lines.Add(format(FLanguageManager.GetMessage('AGENT', 'CLIENT_STOP_REFUSED'), [Msg.AppName + ' - ' + IntToStr(Msg.Handle)]));
    If FStopDialogs.TryGetStopDialog(Msg.Handle, Msg.AppName, StopDialog, idx) Then Begin
        If Assigned(StopDialog.FormStop) Then
            StopDialog.FormStop.CancelStop;

        StopDialog.State := dsCancel;
    End;

    If IdTCPClientMASTER.Connected Then
        Msg.SendMessage(IdTCPClientMASTER.IOHandler);

End;

End.

