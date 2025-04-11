(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_Component.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 2.0.0
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

  *******************************************************************************)

Unit AppWatcherClient_Component;

Interface

Uses
    System.Classes, System.SysUtils, IdTCPClient, IdTCPConnection, IdComponent, IdGlobal,
    IdBaseComponent, Vcl.ExtCtrls, Vcl.StdCtrls, AppWatcher_ioHandler,
    Vcl.Forms, AppWatcher_Lang, Vcl.Controls, dialogs, System.IniFiles, Winapi.Windows, Winapi.Messages,
    AppWatcher_consts;

Type
    TOnCommandReceived = Procedure(Sender: TObject; Const Command: TAppWatcherCommand) Of Object;
    TOnStopRequested = Procedure(Sender: TObject; Var CanStop: Boolean) Of Object;
    TOnGetAppParams = Procedure(Sender: TObject; Var Params: String) Of Object;

    TAppWatcherClient = Class(TComponent)
    private
        FIdTCPClient: TIdTCPClient;
        FTimer: TTimer;
        FMemo: TMemo;
        FLang: TAppWatcherLang;
        FLastCommand: String;
        FOnCommandReceived: TOnCommandReceived;
        FOnStopRequested: TOnStopRequested;
        FOnGetAppParams: TOnGetAppParams;
        FPort: Integer;
        FInterval: Integer;
        FLastReconnectAttempt: TDateTime;
        FSignalErrorConnect: Boolean;
        FRequestingClose: Boolean;
        FIniFileNotFOund: Boolean;
        FIsOnMainForm: Boolean;
        FLanguageManager: TAppLangManager;
        Procedure TimerEvent(Sender: TObject);
        Procedure SetMemo(Value: TMemo);
        Procedure SetPort(Value: Integer);
        Procedure SetInterval(Value: Integer);
        Procedure CheckForCommands;
        Procedure SetLang(Value: TAppWatcherLang);
        Procedure WriteMsgFormat(Const Section, Msg: String; Const Args: Array Of Const);
        Procedure WriteMsg(Const Msg: String);
        Function InstanceActive: Boolean;

    protected
        FIsFirstLoad: Boolean; //Permet de savoir si c'est le premier chargement
        Procedure Loaded; override;
    public
        Constructor Create(AOwner: TComponent); override;
        Destructor Destroy; override;
        Procedure Connect;
        Procedure Disconnect;
        Property LastCommand: String Read FLastCommand;
    published
        Property Memo: TMemo Read FMemo Write SetMemo;
        //22/02/2025 Property Port and Interval moved in inifile
        //property Port:              Integer read FPort write SetPort default 2520;
        //property Interval:          Integer read FInterval write SetInterval default 3000;
        Property OnCommandReceived: TOnCommandReceived Read FOnCommandReceived Write FOnCommandReceived;
        Property OnStopRequested: TOnStopRequested Read FOnStopRequested Write FOnStopRequested;
        Property OnGetAppParams: TOnGetAppParams Read FOnGetAppParams Write FOnGetAppParams;
        Property Lang: TAppWatcherLang Read FLang Write SetLang default langFr;
        Property CloseRequested: Boolean Read FRequestingClose;
    End;

Procedure Register;

Implementation

Procedure Register;
Begin
    RegisterComponents('AppWatcher', [TAppWatcherClient]);
End;

{TAppWatcherClient}

Const
    CLientConnectRetryDelay: Integer = 5000; //5 sec

Function FindParentForm(AComponent: TComponent): TForm;
Var
    Current: TComponent;
Begin
    Result := Nil;
    Current := AComponent;

    //🔄 Remonte dans la hiérarchie des "Owner" (gestion mémoire)
    While Assigned(Current) Do Begin
        If Current Is TForm Then
            Exit(TForm(Current)); //✅ Trouvé !

        Current := Current.Owner;
    End;

    //🔄 Remonte aussi la hiérarchie des "Parent" (visuel)
    If AComponent Is TControl Then Begin
        Exit(TForm(GetParentForm(TControl(AComponent))));
    End;
End;

Constructor TAppWatcherClient.Create(AOwner: TComponent);
Var
    ParentForm: TForm;
Begin

    Inherited Create(AOwner);
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

    If Not (csDesigning In ComponentState) Then
        FTimer.Enabled := true; //✅ Actif seulement en mode exécution

End;

Destructor TAppWatcherClient.Destroy;
Begin
    Disconnect;
    FreeAndNil(FTimer);
    FreeAndNil(FIdTCPClient);
    FreeAndNil(FLanguageManager);
    Inherited Destroy;
End;

Procedure TAppWatcherClient.Loaded;
Var
    IniFilePathName: String;
    Ini: TMemIniFile;
    ParentForm: TForm;
    Msg: String;
Begin
    Inherited Loaded;

    If Assigned(FMemo) Then
        FMemo.Lines.clear;

    ParentForm := FindParentForm(Self.GetParentComponent);
    FIsOnMainForm := (ParentForm = Application.MainForm);

    //Charger la langue
    SetLang(FLang);

    //Charger le fichier INI
    IniFilePathName := FindConfigPath(AppWatcherIniFileName);

    If IniFilePathName = '' Then Begin
        //Le fichier INI est introuvable
        FIniFileNotFOund := true;
        If FLang = LangEn Then
            Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileName])
        Else
            Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileName]);

        WriteMsg(Msg);
        If FIsOnMainForm And Not (csDesigning In ComponentState) Then
            MessageDlg(Msg, mtError, [mbok], 0)
    End Else Begin
        //Charger les valeurs du fichier INI
        Ini := TMemIniFile.Create(IniFilePathName);
        Try
            FPort := Ini.ReadInteger('ClientConfig', 'Port', 2520);
            FInterval := Ini.ReadInteger('ClientConfig', 'Interval', 3000);
        Finally
            Ini.Free;
        End;
    End;

    FIsFirstLoad := False;
End;

Procedure TAppWatcherClient.SetLang(Value: TAppWatcherLang);
Var
    Msg: String;
Begin
    If (FLang <> Value) Or FIsFirstLoad Then Begin
        FLang := Value;
        FIsFirstLoad := False;

        //✅ Charger la langue localement
        If Not FLanguageManager.LoadLanguage(FLang) Then Begin
            If FLang = LangEn Then
                Msg := format(MsgIniFileNotFoundEn, [LangEnIniFileName])
            Else
                Msg := format(MsgIniFileNotFoundFr, [LangFrIniFileName]);

            WriteMsg(Msg);

            If FIsOnMainForm And Not (csDesigning In ComponentState) Then
                MessageDlg(Msg, mtError, [mbok], 0);

            FIniFileNotFOund := true;
        End;
    End;
End;

Function TAppWatcherClient.InstanceActive: Boolean;
Begin
    Result := true;

    If Not FIsOnMainForm Or FRequestingClose Or FIniFileNotFOund Then Begin
        Result := False;
        If FIniFileNotFOund Then Begin
            //Message si le fichier est introuvable
            If FLang = LangEn Then
                WriteMsg(MsgNoInstanceActiveEn)
            Else
                WriteMsg(MsgNoInstanceActiveFr);

        End Else
            WriteMsgFormat('CLIENT', 'NOT_ACTIVE', []);
    End;
End;

Procedure TAppWatcherClient.Connect;
Var
    ParentForm: TForm;
Begin
    //ParentForm := FindParentForm(Self.GetParentComponent);
    //FIsOnMainForm := (ParentForm = Application.MainForm);
    If Not InstanceActive Then
        Exit;

    If (Now - FLastReconnectAttempt) * 86400000 < CLientConnectRetryDelay Then
        Exit; //⏳ Attendre avant de réessayer

    FLastReconnectAttempt := Now;

    If Not FIdTCPClient.Connected Then Begin
        Try
            FIdTCPClient.Port := FPort;
            FIdTCPClient.Connect;
            FSignalErrorConnect := False;

            If Assigned(FMemo) Then
                FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'CONNECTED'));
        Except
            On E: Exception Do Begin
                If FSignalErrorConnect Then Begin
                    If Assigned(FMemo) Then
                        FMemo.Lines.Add(FLanguageManager.GetMessage('CLIENT', 'FAILED_CONNECT'));
                    FSignalErrorConnect := False;
                End;
            End;
        End;
    End;
End;

Procedure TAppWatcherClient.TimerEvent(Sender: TObject);
Var
    ParentForm: TForm;
Begin
    //ParentForm := FindParentForm(Self.GetParentComponent);
    //FIsOnMainForm := (ParentForm = Application.MainForm);
    If Not InstanceActive Then Begin
        FTimer.Enabled := False;
        Exit;
    End;

    If Not FIdTCPClient.Connected Then
        Connect;

    //Exécuter CheckForCommands dans un thread
    TThread.CreateAnonymousThread(
        Procedure
        Begin
            CheckForCommands;
        End).Start;
End;

Procedure TAppWatcherClient.Disconnect;
Begin
    If FIdTCPClient.Connected Then
        FIdTCPClient.Disconnect;
End;

Procedure TAppWatcherClient.SetMemo(Value: TMemo);
Begin
    FMemo := Value;
End;

Procedure TAppWatcherClient.SetPort(Value: Integer);
Begin

    If Value <> FPort Then Begin
        FPort := Value;
        If FIdTCPClient.Connected Then Begin
            Disconnect;
            Connect;
        End;
    End;
End;

Procedure TAppWatcherClient.SetInterval(Value: Integer);
Begin
    If Value <> FInterval Then Begin
        FInterval := Value;
        FTimer.Interval := FInterval;
    End;
End;

Procedure TAppWatcherClient.WriteMsg(Const Msg: String);
Begin
    TThread.Queue(Nil,
        Procedure
        Begin
            If Assigned(FMemo) Then
                FMemo.Lines.Add(Msg);
        End);
End;

Procedure TAppWatcherClient.WriteMsgFormat(Const Section, Msg: String; Const Args: Array Of Const);
Var
    FormattedMsg: String;
Begin
    //✅ Nouvelle version utilisant l'instance locale
    FormattedMsg := format(FLanguageManager.GetMessage(Section, Msg), Args);
    WriteMsg(FormattedMsg);
End;

Procedure TAppWatcherClient.CheckForCommands;
Var
    LocalMsg, AnswerMsg, ReceivedMsg: TAppWatcherMessage;
    CanStop: Boolean;
    AppPath, Params: String;
    idx: Integer;
    MessageReceived: Boolean;
Begin
    If Not FIdTCPClient.Connected Then
        Exit;

    Try
        MessageReceived := False;

        //🔁 Lire tous les messages disponibles
        While FIdTCPClient.IOHandler.InputBuffer.Size > 0 Do Begin
            MessageReceived := true;

            //🔹 Lire les données réseau
            If ReadMessage(FIdTCPClient.IOHandler, ReceivedMsg) Then Begin
                LocalMsg := ReceivedMsg;
                AppPath := ParamStr(0);
                FLastCommand := LocalMsg.CmdName;

                WriteMsgFormat('CLIENT', 'MESSAGE_RECEIVED', [LocalMsg.CmdName]);

                Case LocalMsg.Command Of
                    //🔵 Commande WHO (identification)
                    cmdWHO: Begin
                            AnswerMsg.Init(Application.Handle, cmdWHO_REPLY, AppPath, '', 0);
                            AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                            WriteMsgFormat('CLIENT', 'WHO_RECEIVED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                        End;

                    //🛑 Commande STOP (demande d'arrêt)
                    cmdSTOP: Begin
                            //Pas de réponse si la demande ne nous concerne pas
                            If SameText(ExtractFileName(AppPath), LocalMsg.AppName) And (LocalMsg.Handle = Application.Handle) Then Begin
                                Params := '';
                                If Assigned(FOnGetAppParams) Then
                                    TThread.Synchronize(Nil,
                                        Procedure
                                        Begin
                                            FOnGetAppParams(Self, Params);
                                        End);

                                //Initialisation de la réponse
                                AnswerMsg.Init(Application.Handle, cmdACK, ParamStr(0), Params, 0);
                                WriteMsgFormat('CLIENT', 'STOP_RECEIVED', [AnswerMsg.AppPath + ' params=' + Params, IntToStr(AnswerMsg.Handle)]);

                                //🔍 Demander l'arrêt à l'application
                                CanStop := true;
                                If Assigned(FOnStopRequested) Then Begin
                                    TThread.Synchronize(Nil,
                                        Procedure
                                        Begin
                                            FOnStopRequested(Self, CanStop);
                                        End);
                                End;

                                //📌 Réponse ACK ou NACK
                                If CanStop Then Begin
                                    AnswerMsg.Command := cmdACK;
                                    WriteMsgFormat('CLIENT', 'STOP_ACCEPTED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                End Else Begin
                                    AnswerMsg.Command := cmdNACK;
                                    WriteMsgFormat('CLIENT', 'STOP_REFUSED', [AnswerMsg.AppPath, IntToStr(AnswerMsg.Handle)]);
                                End;

                                AnswerMsg.SendMessage(FIdTCPClient.IOHandler);

                                //🔥 Fermer l'application si arrêt accepté
                                If CanStop Then
                                    TThread.Queue(Nil,
                                        Procedure
                                        Begin
                                            FRequestingClose := true; //✅ Indique que le composant demande la fermeture

                                            If Assigned(Application.MainForm) Then Begin
                                                Application.MainForm.OnCloseQuery := Nil;
                                                Application.MainForm.close;
                                            End Else
                                                Application.terminate; //Sécurité si la MainForm n'existe pas
                                        End);

                            End;
                        End;

                    cmdSTOP_REQUEST: Begin
                            //Préparation à une demande de STOP
                            //Pas de réponse si la demande ne nous concerne pas
                            If SameText(ExtractFileName(AppPath), LocalMsg.AppName) Then Begin

                                //L'application doit renvoyer son Handle
                                WriteMsgFormat('CLIENT', 'STOP_REQUEST', [IntToStr(Application.Handle)]);
                                //Construire la réponse enrichie avec le Handle

                                AnswerMsg.Init(Application.Handle, cmdREPLY_STOP_REQUEST, ParamStr(0), '', LocalMsg.Duration);
                                //Envoyer la réponse STOP à l’AGENT
                                AnswerMsg.SendMessage(FIdTCPClient.IOHandler);
                            End;
                        End;

                Else
                    //❓ Commande inconnue
                    WriteMsgFormat('CLIENT', 'UNKNOW_COMMAND', [LocalMsg.CmdName, IntToStr(Application.Handle)]);
                End;

                //🔔 Déclencher l'événement OnCommandReceived
                If Assigned(FOnCommandReceived) Then Begin
                    TThread.Synchronize(Nil,
                        Procedure
                        Begin
                            FOnCommandReceived(Self, LocalMsg.Command);
                        End);
                End;
            End;
        End;

        //✅ Ajout d'une pause uniquement si aucun message n'a été lu
        If Not MessageReceived Then
            Sleep(10);
    Except
        On E: Exception Do
            WriteMsgFormat('CLIENT', 'ERROR_RECEIVING', [E.Message]);

    End;
End;

End.

