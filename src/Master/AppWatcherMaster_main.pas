(*******************************************************************************
  Project : AppWatcherMaster
  Unit    : AppWatcherMaster_main.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 20/02/2025
  Version : 1.0
  License : MIT

  Description :
  -------------
  This unit manages the main server of the AppWatcherMaster application.
  It uses `IdTCPServer` for network communication with clients and a `TStringGrid`
  to display monitored applications.

  Features :
  -----------
  - TCP connection handling using `Indy`
  - Multi-thread synchronization with `TCriticalSection`
  - Dynamic application display with `TStringGrid`
  - Interactive column sorting with `OnFixedCellClick`
  - Application selection via `OnDblClick`
  - Thread-safe UI updates using `TThread.Queue`

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [18/02/2025] : Added column sorting feature
  - [20/02/2025] : Implemented thread-safe access control
  - [21/02/2025] : Improved INI file handling in FormCreate :  Ensured `FindConfigPath` checks file existence before loading.
  - [22/02/2025] : Replaced the singleton AppLangManager with a local instance to allow multiple instances.

  Note :
  -------
  This project is open-source. Contributions and improvements are welcome!
  *******************************************************************************)

unit AppWatcherMaster_main;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, System.Generics.Collections,
    IdCustomTCPServer, IdTCPServer, IdContext, Vcl.StdCtrls, Vcl.ComCtrls, System.SyncObjs,
    AppWatcher_ioHandler, Vcl.Mask, RzEdit, AppWatcher_Lang, Vcl.ExtCtrls, IdStack, System.Generics.Defaults,
    Vcl.Grids, System.IniFiles, Winapi.WinSock, IdScheduler,
    IdSchedulerOfThread, IdSchedulerOfThreadPool, IdGlobal,
    AppWatcher_consts;

type
    TAppRec = record
        ClientIp: string;
        ClientName: string;
        UserName: string;
        AppName: string;
        AppPath: String;
        AppHandle: Integer;
        procedure init(Msg: TAppWatcherMessage; Context: TIdContext);
        class operator Equal(const A, B: TAppRec): Boolean;
    end;

    TFormAppWatcherMaster = class(TForm)
        IdTCPServer1: TIdTCPServer;
        MemoLogs: TMemo;
        BtnListApps: TButton;
        BtnStopApp: TButton;
        EditAppName: TEdit;
        BtnStart: TButton;
        ListViewClients: TMemo;
        LblAppName: TLabel;
        BtnCancel: TButton;
        RzNumericEdit1: TRzNumericEdit;
        UpDown1: TUpDown;
        RdioFrench: TRadioButton;
        RdioEnglish: TRadioButton;
        LblDuration: TLabel;
        LblClientList: TLabel;
        LblAppList: TLabel;
        LblMsg: TLabel;
        PanelBottom: TPanel;
        PanelTop: TPanel;
        StringGridApp: TStringGrid;
        BtnAgentStop: TButton;
        TimerupdateClient: TTimer;
        procedure BtnAgentStopClick(Sender: TObject);
        procedure BtnCancelClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure BtnListAppsClick(Sender: TObject);

        procedure BtnStartClick(Sender: TObject);
        procedure BtnStopAppClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure IdTCPServer1Connect(AContext: TIdContext);
        procedure IdTCPServer1Disconnect(AContext: TIdContext);
        procedure IdTCPServer1Execute(AContext: TIdContext);
        procedure RdioEnglishClick(Sender: TObject);
        procedure RdioFrenchClick(Sender: TObject);
        procedure StringGridAppDblClick(Sender: TObject);
        procedure StringGridAppDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect:
            TRect; State: TGridDrawState);
        procedure StringGridAppFixedCellClick(Sender: TObject; ACol, ARow: LongInt);
        procedure StringGridAppMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
            Integer);
        procedure TimerupdateClientTimer(Sender: TObject);
    private
        {Déclarations privées}
        FSortColumn:                Integer;
        FSortAscending:             Boolean;
        FIsFirstLoad:               Boolean;
        FLang:                      TAppWatcherLang;
        FLanguageManager:           TAppLangManager;
        FAppList:                   TList<TAppRec>; //Clé = NomMachine | Valeur = Liste des Apps
        FAppListLock:               TCriticalSection; //🔒 Protection contre les accès concurrents
        FLastHintCol, FLastHintRow: Integer;
        function ContainsApp(const App: TAppRec): Boolean;
        procedure UpdateClientList;
        procedure UpdateUI;
        Function HostName(Context: TIdContext): string;
        procedure SendMessageToClients(Msg: TAppWatcherMessage);
        procedure UpdateStringGrid;
        procedure SortGrid(Column: Integer);
    public
        {Déclarations publiques}
    end;

var
    FormAppWatcherMaster: TFormAppWatcherMaster;

implementation

{$R *.dfm}


//Fonction qui renvoie l'adresse IP locale de la machine
function GetLocalIP: string;
type
    TaPInAddr = array [0 .. 10] of PInAddr; //Tableau de pointeurs vers des adresses IP
    PaPInAddr = ^TaPInAddr; //Pointeur vers le tableau d'adresses IP
var
    phe:       PHostEnt; //Structure contenant des informations sur l'hôte (ordinateur)
    pptr:      PaPInAddr; //Pointeur vers la liste d'adresses IP de l'hôte
    buffer:    array [0 .. 63] of AnsiChar; //Tampon pour stocker le nom d'hôte
    I:         Integer; //Variable d'itération
    GInitData: TWSADATA; //Structure pour initialiser l'utilisation de sockets
begin
    //Initialise le résultat à une chaîne vide
    Result := '';

    //Initialise la bibliothèque Windows Sockets

    WSAStartup($101, GInitData);
    try
        //Récupère le nom de l'hôte local (ordinateur) et le stocke dans le buffer
        GetHostName(buffer, sizeof(buffer));

        //Utilise le nom de l'hôte pour obtenir les informations associées, telles que les adresses IP
        phe := GetHostByName(buffer);

        //Si l'appel échoue (pas d'information disponible pour l'hôte), quitter la fonction
        if phe = nil then
            exit;

        //Pointe sur la liste d'adresses IP de l'hôte
        pptr := PaPInAddr(phe^.h_addr_list);

        //Boucle sur toutes les adresses IP disponibles pour l'hôte
        I := 0;
        while pptr^[I] <> nil do begin
            //Convertit l'adresse IP au format texte et la stocke dans le résultat
            Result := StrPas(inet_ntoa(pptr^[I]^));
            Inc(I); //Passe à l'adresse suivante
        end;
    Finally
        //Libère les ressources utilisées par la bibliothèque Windows Sockets
            WSACleanup;
    end;
end;

procedure TAppRec.init(Msg: TAppWatcherMessage; Context: TIdContext);
Begin
    AppName := Msg.AppName;
    AppPath := ExtractFilePath(Msg.AppPath);
    ClientIp := Context.Binding.PeerIP;
    ClientName := GStack.HostByAddress(ClientIp);
    UserName := Msg.UserName;
    AppHandle := Msg.Handle;
end;

class operator TAppRec.Equal(const A, B: TAppRec): Boolean;
begin
    Result := (A.ClientIp = B.ClientIp) and
        (A.AppHandle = B.AppHandle);
end;

Function TFormAppWatcherMaster.HostName(Context: TIdContext): string;
begin
    try
            Result := Context.Binding.PeerIP + ' = ' + GStack.HostByAddress(Context.Binding.PeerIP);
    except
            Result := Context.Binding.PeerIP;
    end;

End;

function TFormAppWatcherMaster.ContainsApp(const App: TAppRec): Boolean;
var
    ExistingApp: TAppRec;
begin
    Result := False;
    for ExistingApp in FAppList do
        if (ExistingApp = App) then
            exit(True); //Dès qu'on trouve un match, on sort immédiatement
end;

procedure TFormAppWatcherMaster.FormCreate(Sender: TObject);
var
    LangValue, ServerIP, ServerIPini: string;
    Ini:                              TIniFile;
    IniFilePathName, Msg:             string;
    DoClose:                          Boolean;
begin
    DoClose := False;
    FIsFirstLoad := True;

    try
        //🔹 Vérification des paramètres en ligne de commande (--lang fr ou --lang en)
        if FindCmdLineSwitch('lang', LangValue, True, [clstValueNextParam, clstValueAppended]) then
        begin
            LangValue := LowerCase(LangValue);
            if (LangValue <> 'fr') and (LangValue <> 'en') then
                FLang := LangFr; //🔴 Sécurisation : éviter une langue inconnue
        end
        else
            FLang := LangFr;

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
            DoClose := True;
        end;

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
            DoClose := True;
            FreeAndNil(FLanguageManager);
        end;

        if DoClose then
            Application.terminate;

        FAppList := TList<TAppRec>.Create(TDelegatedComparer<TAppRec>.Create(
            function(const Left, Right: TAppRec): Integer
            begin
                Result := CompareText(Left.ClientName, Right.ClientName);
                if Result = 0 Then
                    Result := CompareText(Left.AppName, Right.AppName);
            end));

        FAppListLock := TCriticalSection.Create; //🔒 Création du verrou

        UpdateUI;

        IdTCPServer1.Active := False; //le serveur est bien désactivé

        Ini := TIniFile.Create(IniFilePathName);
        try
            //Charger l'IP du serveur et le port
            ServerIPini := Ini.ReadString('MasterConfig', 'ServerIP', '127.0.0.1');
            ServerIP := GetLocalIP;
            //Si l'adresse dans le fichier ini ne correspond pas à l'IP alors on l'écrase
            //De cette manière les clients se connecterons au serveur en cours
            //Attention car c'est le dernier serveur lancé qui prend la main
            if ServerIP <> ServerIPini Then
                Ini.WriteString('MasterConfig', 'ServerIP', ServerIP);

        finally
                Ini.Free;
        end;

        IdTCPServer1.Active := True; //Activation du serveur après la configuration
    Finally
            FIsFirstLoad := False;
    end;
end;

procedure TFormAppWatcherMaster.FormClose(Sender: TObject; var Action: TCloseAction);
var
    ClientIp: string;
    Context:  TIdContext;

begin
    //2️⃣ Désactiver le serveur proprement
    IdTCPServer1.Active := False;
    FAppListLock.Enter; //🔒 Bloque l'accès pendant la destruction
    try
            FAppList.Free;
    finally
        FAppListLock.Leave; //🔓 Libère le verrou après destruction
        FAppListLock.Free; //🔥 Supprime l'objet de verrouillage
    end;

    MemoLogs.Lines.Add(FLanguageManager.GetMessage('MASTER', 'SERVER_STOPPED'));
    FreeAndNil(FLanguageManager);
end;

procedure TFormAppWatcherMaster.UpdateStringGrid;
var
    I: Integer;
begin
    //Désactiver les mises à jour visuelles pendant la mise à jour du contenu
    StringGridApp.BeginUpdate;
    try
        FAppListLock.Enter; //🔒 Protection contre l'accès concurrent
        try

            //Efface tout sauf l’en-tête
            //🔄 Ajuster le nombre de lignes (au moins 2 pour éviter le bug)
            if FAppList.Count = 0 then
            begin
                StringGridApp.RowCount := 2;
                StringGridApp.Rows[1].Clear; //Garde une ligne vide si la liste est vide
                exit;
            end;

            //🔢 Ajuster dynamiquement RowCount
            StringGridApp.RowCount := FAppList.Count + 1; //+1 pour l'en-tête

            //Remplit le `StringGrid` avec les données triées
            for I := 0 to FAppList.Count - 1 do
            begin
                StringGridApp.Cells[0, I + 1] := FAppList[I].ClientIp;
                StringGridApp.Cells[1, I + 1] := FAppList[I].ClientName;
                StringGridApp.Cells[2, I + 1] := FAppList[I].UserName;

                StringGridApp.Cells[3, I + 1] := FAppList[I].AppName;
                StringGridApp.Cells[4, I + 1] := FAppList[I].AppHandle.ToString;
                StringGridApp.Cells[5, I + 1] := FAppList[I].AppPath;
            end;
        finally
                FAppListLock.Leave; //🔓 Libère le verrou après mise à jour
        end;
    finally
        //Réactiver les mises à jour visuelles après la mise à jour du contenu
            StringGridApp.EndUpdate;
    end;
end;

procedure TFormAppWatcherMaster.SortGrid(Column: Integer);
begin

    if (FSortColumn = Column) and (FSortAscending = not FSortAscending) then
        exit; //🔄 Évite de trier inutilement

    //Inverser le sens si on clique sur la même colonne
    if FSortColumn = Column then
        FSortAscending := not FSortAscending
    else
        FSortAscending := True; //Par défaut, tri ascendant

    FSortColumn := Column;

    //Trier la liste FAppList
    FAppList.Sort(TComparer<TAppRec>.Construct(
        function(const Left, Right: TAppRec): Integer
        begin
            case Column of
                0: begin
                        Result := CompareText(Left.ClientIp, Right.ClientIp);
                        If Result = 0 then
                            Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 then
                            Result := Left.AppHandle - Right.AppHandle;
                    end;
                1: begin
                        Result := CompareText(Left.ClientName, Right.ClientName);
                        If Result = 0 then
                            Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 then
                            Result := Left.AppHandle - Right.AppHandle;
                    end;
                2: begin
                        Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 then
                            Result := Left.AppHandle - Right.AppHandle;
                    end;
                3:
                    Result := Left.AppHandle - Right.AppHandle;
                4: begin
                        Result := CompareText(Left.AppPath, Right.AppPath);
                        if Result = 0 then
                            Result := CompareText(Left.ClientName, Right.ClientName);
                    end;
            else
                Result := 0;
            end;

            //Inverser si tri descendant
            if not FSortAscending then
                Result := -Result;
        end
        ));

    //Rafraîchir l'affichage
    UpdateStringGrid;
end;

procedure TFormAppWatcherMaster.StringGridAppDblClick(Sender: TObject);
var
    SelectedRow: Integer;
begin
    SelectedRow := StringGridApp.Row; //Récupère la ligne sélectionnée

    //Vérifie que l'utilisateur ne clique pas sur l'en-tête (ligne 0)
    if (SelectedRow > 0) and (SelectedRow < StringGridApp.RowCount) then
    begin
        //MemoLogs.Lines.Add('Double-clic sur la ligne ' + SelectedRow.ToString);
        //MemoLogs.Lines.Add('Client IP: ' + StringGridApp.Cells[0, SelectedRow]);
        //MemoLogs.Lines.Add('App Name: ' + StringGridApp.Cells[2, SelectedRow]);
        EditAppName.Text := StringGridApp.Cells[3, SelectedRow];
    end;
end;

procedure TFormAppWatcherMaster.StringGridAppFixedCellClick(Sender: TObject;
    ACol, ARow: LongInt);
begin
    //if ARow = 0 then //Vérifie qu'on clique bien sur l'en-tête
    //FAppListLock.Enter;
    try
        begin
            MemoLogs.Lines.Add('Clique col' + ACol.ToString);
            SortGrid(ACol); //Trie en fonction de la colonne

        end;
    finally
        //FAppListLock.Leave;
    end;
end;

procedure TFormAppWatcherMaster.StringGridAppDrawCell(Sender: TObject; ACol,
    ARow: LongInt; Rect: TRect; State: TGridDrawState);
begin
    if ARow = 0 then //Si c'est la ligne d'en-tête
    begin
        StringGridApp.Canvas.Brush.Color := clGray; //Fond gris
        StringGridApp.Canvas.Font.Color := clWhite; //Texte blanc
        StringGridApp.Canvas.Font.Style := [fsBold]; //Texte en gras
        StringGridApp.Canvas.FillRect(Rect); //Remplissage du fond
        StringGridApp.Canvas.TextOut(Rect.Left + 5, Rect.Top + 5, StringGridApp.Cells[ACol, ARow]);
    end;
end;

procedure TFormAppWatcherMaster.UpdateUI;
var
    section: string;
begin

    //🌍 Mise à jour des boutons
    section := 'MASTER_UI';
    BtnListApps.Caption := FLanguageManager.GetMessage(section, 'BTN_LIST_APPS');
    BtnStopApp.Caption := FLanguageManager.GetMessage(section, 'BTN_STOP_APP');
    BtnStart.Caption := FLanguageManager.GetMessage(section, 'BTN_START');
    BtnCancel.Caption := FLanguageManager.GetMessage(section, 'BTN_CANCEL');
    BtnAgentStop.Caption := FLanguageManager.GetMessage(section, 'BTN_STOP_AGENT');

    //🏷️ Mise à jour des labels
    LblAppName.Caption := FLanguageManager.GetMessage(section, 'LBL_APP_NAME');
    LblDuration.Caption := FLanguageManager.GetMessage(section, 'LBL_DURATION');
    LblClientList.Caption := FLanguageManager.GetMessage(section, 'LBL_CLIENT_LIST');
    LblMsg.Caption := FLanguageManager.GetMessage(section, 'LBL_MSG');
    LblAppList.Caption := FLanguageManager.GetMessage(section, 'LBL_APPLIST');

    //📜 Mise à jour des groupes radio
    RdioFrench.Caption := FLanguageManager.GetMessage(section, 'RDIO_FRENCH');
    RdioEnglish.Caption := FLanguageManager.GetMessage(section, 'RDIO_ENGLISH');

    //Définition des en-têtes de la StringGRID
    StringGridApp.Cells[0, 0] := FLanguageManager.GetMessage(section, 'STRGRID_IP');
    StringGridApp.Cells[1, 0] := FLanguageManager.GetMessage(section, 'STRGRID_CLINAME');
    StringGridApp.Cells[2, 0] := FLanguageManager.GetMessage(section, 'STRGRID_USERNAME');
    StringGridApp.Cells[3, 0] := FLanguageManager.GetMessage(section, 'STRGRID_APPNAME');
    StringGridApp.Cells[4, 0] := FLanguageManager.GetMessage(section, 'STRGRID_HANDLE');
    StringGridApp.Cells[5, 0] := FLanguageManager.GetMessage(section, 'STRGRID_PATH');
end;

procedure TFormAppWatcherMaster.SendMessageToClients(Msg: TAppWatcherMessage);
var
    Context:     TIdContext;
    ContextList: TList<TIdContext>;

    RefMsg: string;
begin
    case Msg.Command of
        cmdWHO:
            RefMsg := 'LIST_SENT';
        cmdSTART:
            RefMsg := 'START_SENT';
        cmdSTOP:
            RefMsg := 'STOP_SENT';
        cmdCANCEL:
            RefMsg := 'CANCEL_SENT';
        cmdSTOP_AGENT:
            RefMsg := 'STOP_AGENT_SENT';
    else
        RefMsg := 'UNKNOWN_COMMAND';
    end;

    //Verrouiller la liste
    //ContextList := IdTCPServer1.Contexts.LockList;
    ContextList := TList<TIdContext>(IdTCPServer1.Contexts.LockList);
    try
        for Context in ContextList do
        begin
            try
                Msg.SendMessage(Context.Connection.IOHandler);
                MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', RefMsg), [HostName(Context), Msg.AppName, IntToStr(Msg.Handle)]));
            except
                    MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ERROR_CONTACTING_CLIENT'), [HostName(Context)]));
            end;
        end;
    finally
            IdTCPServer1.Contexts.UnlockList; //Déverrouiller proprement après la boucle
    end;

end;

procedure TFormAppWatcherMaster.BtnListAppsClick(Sender: TObject);
var
    Msg: TAppWatcherMessage;
begin
    //🗑️ 1. Réinitialiser la liste des applications connues

    FAppList.Clear;
    UpdateStringGrid;
    Msg.init(0, cmdWHO, '', '', 0); //Commande LIST
    SendMessageToClients(Msg);

end;

procedure TFormAppWatcherMaster.UpdateClientList;
var
    ContextList: TList<TIdContext>;
    Context:     TIdContext;
    Liste:       string;
    s:           string;
begin
    Liste := '';
    //ListViewClients.Clear;

    ContextList := TList<TIdContext>(IdTCPServer1.Contexts.LockList);
    try
        for Context in ContextList do //Boucle classique sans `in`
        begin
            Liste := Liste + HostName(Context) + #13#10;
            //ListViewClients.Lines.Add(HostName(Context));
        end;
    finally
            IdTCPServer1.Contexts.UnlockList; //Toujours libérer la liste !
    end;

    if Liste <> '' Then
        setlength(Liste, Length(Liste) - 2);
    if ListViewClients.Lines.Text <> Liste then begin
        ListViewClients.Lines.Text := Liste;
        s := FLanguageManager.GetMessage('MASTER', 'CLIENTS_LIST_UPDATED');
        MemoLogs.Lines.Add(s);
    end;
end;

procedure TFormAppWatcherMaster.BtnStartClick(Sender: TObject);
var
    Msg: TAppWatcherMessage;
begin
    Msg.init(0, cmdSTART, '', '', 0); //Commande START
    SendMessageToClients(Msg);
end;

procedure TFormAppWatcherMaster.BtnStopAppClick(Sender: TObject);
var
    Msg:     TAppWatcherMessage;
    AppName: string;
begin
    AppName := Trim(EditAppName.Text);
    if AppName = '' then
    begin
        MemoLogs.Lines.Add(FLanguageManager.GetMessage('MASTER', 'ENTER_PROGRAM_NAME'));
        exit;
    end;

    Msg.init(0, cmdSTOP, AppName, '', RzNumericEdit1.IntValue); //Commande STOP
    SendMessageToClients(Msg);
end;

procedure TFormAppWatcherMaster.BtnAgentStopClick(Sender: TObject);
var
    Msg: TAppWatcherMessage;
begin
    if MessageDlg(FLanguageManager.GetMessage('MASTER', 'STOP_AGENT_REQUEST'), mtConfirmation, [mbyes, mbNo], 0, mbNo) = mryes then
    Begin
        Msg.init(0, cmdSTOP_AGENT, '', '', 0); //Commande STOP
        SendMessageToClients(Msg);
    end;
end;

procedure TFormAppWatcherMaster.BtnCancelClick(Sender: TObject);
var
    Msg: TAppWatcherMessage;
begin
    Msg.init(0, cmdCANCEL, '', '', 0); //Commande CANCEL
    SendMessageToClients(Msg);
end;

procedure TFormAppWatcherMaster.IdTCPServer1Connect(AContext: TIdContext);
begin
    //✅ Forcer l'UI à se mettre à jour dans le thread principal
    TThread.Queue(nil,
        procedure
        begin
            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'CLIENT_CONNECTED'), [HostName(AContext)]));
        end);
end;

procedure TFormAppWatcherMaster.IdTCPServer1Disconnect(AContext: TIdContext);
Var
    Name: string;
begin
    Name := HostName(AContext);
    TThread.Queue(nil,
        procedure
        begin
            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'CLIENT_DISCONNECTED'), [Name]));
        end);
end;

procedure TFormAppWatcherMaster.TimerupdateClientTimer(Sender: TObject);
begin
    UpdateClientList; //Met à jour la liste des clients
end;

procedure TFormAppWatcherMaster.IdTCPServer1Execute(AContext: TIdContext);
//Procedure de reception des messages depuis les AGENTS
var
    ReceivedMsg: TAppWatcherMessage;

begin
    try
        if not ReadMessage(AContext.Connection.IOHandler, ReceivedMsg) then begin
            sleep(300);
            exit;
        end;

        TThread.Queue(nil,
            procedure
            var
                App: TAppRec;
                I: Integer;
            begin
                case ReceivedMsg.Command of
                    cmdWHO_REPLY:
                        begin
                            App.init(ReceivedMsg, AContext);

                            //📝 Mettre à jour la liste en mémoire
                            FAppListLock.Enter;
                            try
                                if not ContainsApp(App) then
                                    FAppList.Add(App);

                                //🔽 Créer une liste triable à partir des valeurs du dictionnaire
                                FAppList.Sort;
                            finally
                                    FAppListLock.Leave; //🔓 Libère le verrou
                            end;

                            //🔄 Rafraîchir l'affichage
                            UpdateStringGrid;

                            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'WHO_REPLY'),
                                [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));

                        end;
                    cmdACK:
                        MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ACK_REPLY'),
                            [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                    cmdNACK:
                        MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'NACK_REPLY'),
                            [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                else
                    MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'UNKNOW_REPLY'),
                        [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                end;

            end);
    except
        on E: Exception do
        begin
            TThread.Queue(nil,
                procedure
                begin
                    MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ERROR_RECEIVING_ANSWER'), [E.Message]));
                end);
        end;
    end;
end;

procedure TFormAppWatcherMaster.RdioEnglishClick(Sender: TObject);
begin
    if FIsFirstLoad then
        exit;

    FLang := LangEn;
    FLanguageManager.LoadLanguage(LangEn);
    UpdateUI
end;

procedure TFormAppWatcherMaster.RdioFrenchClick(Sender: TObject);
begin
    if FIsFirstLoad then
        exit;

    FLang := LangFr;
    FLanguageManager.LoadLanguage(LangFr);
    UpdateUI
end;

procedure TFormAppWatcherMaster.StringGridAppMouseMove(Sender: TObject; Shift:
    TShiftState; X, Y: Integer);
var
    Col, Row:           Integer;
    CellRect:           TRect;
    CellText, HintText: string;
    MousePos:           TPoint;
begin
    //Déterminer la cellule sous la souris
    StringGridApp.MouseToCell(X, Y, Col, Row);

    //Vérifier si la cellule est valide
    if (Col >= 0) and (Row >= 0) then
    begin
        StringGridApp.MouseToCell(X, Y, Col, Row);

        //N'affiche le hint que si on change de cellule
        if (Col <> FLastHintCol) or (Row <> FLastHintRow) then
        begin
            FLastHintCol := Col;
            FLastHintRow := Row;
            //Appelle la méthode précédente ou adapte le code ici

            //Récupérer le texte de la cellule
            CellText := StringGridApp.Cells[Col, Row];
            //Construire le hint personnalisé
            HintText := CellText;

            //Calculer la position de la souris pour le hint
            MousePos := StringGridApp.ClientToScreen(Point(X, Y));
            Inc(MousePos.Y, 10); //Décalage pour éviter de cacher le curseur

            //Définir et activer le hint
            Application.HideHint;
            StringGridApp.Hint := HintText;
            Application.ActivateHint(MousePos);
        end;
    end;
end;

end.
