(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_main.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 09/04/2025
  Version : 3.0.0
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
  - **Added Splitter for UI improvement**
  - **Fixed sorting issue in column headers**

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [18/02/2025] : Added column sorting feature
  - [20/02/2025] : Implemented thread-safe access control
  - [21/02/2025] : Improved INI file handling in FormCreate
  - [22/02/2025] : Replaced singleton `AppLangManager` with local instances
  - [23/02/2025] : v1.1 Added dynamic application title translation
  - [23/02/2025] : v1.2 Improved configuration file lookup with shortcut resolution
  - [24/02/2025] : v1.3 Fixed sorting issue + Added Splitter for better UI
  - [27/02/2025] : v1.3.3 Enhanced graceful shutdown of Indy server
  - [28/02/2025] : v1.3.4 Added application filters for better usability
  - [06/03/2025] : v2.0 - Use of New  TAppWatcherMessage with Packed Record
                          !!! This change makes the version incompatible with v1 !!!
                        - Check if the MASTER is still the MASTER
                        - Added files deployment features
                        - Filtering app list with deployment list
  - [09/04/2025] : v3.0 - Added agent restart feature. (uses AppWatcherStub)
                        - Silent mode to stop and restart the application without notifying the user

  Note :
  -------
  This project is open-source. Contributions are welcome!
  *******************************************************************************)

Unit AppWatcherMaster_main;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, System.Generics.Collections,
    IdCustomTCPServer, IdTCPServer, IdContext, Vcl.StdCtrls, Vcl.ComCtrls, System.SyncObjs,
    AppWatcher_ioHandler, Vcl.Mask, RzEdit, AppWatcher_Lang, Vcl.ExtCtrls, IdStack, System.Generics.Defaults,
    Vcl.Grids, System.IniFiles, Winapi.WinSock, IdScheduler, System.strutils, System.Math,
    IdSchedulerOfThread, IdSchedulerOfThreadPool, IdGlobal, System.DateUtils, System.IOUtils,
    AppWatcher_consts, System.RegularExpressions, IdSSLOpenSSL,
    Vcl.WinXCtrls;

Type
    TAppRec = Record
        ClientIp: String;
        ClientName: String;
        UserName: String;
        AppName: String;
        AppPath: String;
        AppHandle: Integer;
        Procedure init(Msg: TAppWatcherMessage; Context: TIdContext);
        Class Operator Equal(Const A, B: TAppRec): Boolean;
    End;

    TFormAppWatcherMaster = Class(TForm)
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
        Splitter1: TSplitter;
        BtnAppCopy: TButton;
        BtnToggleFilter: TToggleSwitch;
    BtnAgentStart: TButton;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    ChkSilent: TCheckBox;
        Procedure BtnAgentStopClick(Sender: TObject);
        Procedure BtnCancelClick(Sender: TObject);
        Procedure FormCreate(Sender: TObject);
        Procedure BtnListAppsClick(Sender: TObject);

        Procedure BtnStartClick(Sender: TObject);
        Procedure BtnStopAppClick(Sender: TObject);
        Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
        Procedure IdTCPServer1Connect(AContext: TIdContext);
        Procedure IdTCPServer1Disconnect(AContext: TIdContext);
        Procedure IdTCPServer1Execute(AContext: TIdContext);
        Procedure RdioEnglishClick(Sender: TObject);
        Procedure RdioFrenchClick(Sender: TObject);
        Procedure StringGridAppDblClick(Sender: TObject);
        Procedure StringGridAppDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect:
            TRect; State: TGridDrawState);
        Procedure StringGridAppFixedCellClick(Sender: TObject; ACol, ARow: LongInt);
        Procedure StringGridAppKeyDown(Sender: TObject; Var Key: Word; Shift:
            TShiftState);
        Procedure StringGridAppKeyPress(Sender: TObject; Var Key: Char);
        Procedure StringGridAppMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
            Integer);
        Procedure StringGridAppSetEditText(Sender: TObject; ACol, ARow: LongInt; Const
            Value: String);
        Procedure TimerupdateClientTimer(Sender: TObject);
        Procedure BtnAppCopyClick(Sender: TObject);
        Procedure BtnToggleFilterClick(Sender: TObject);
    procedure BtnAgentStartClick(Sender: TObject);
    private
        {Déclarations privées}
        FSortColumn: Integer;
        FSelColumn: Integer;
        FSortAscending: Boolean;
        FIsFirstLoad: Boolean;
        FLang: TAppWatcherLang;
        FLanguageManager: TAppLangManager;
        FAppList: TList<TAppRec>; //Clé = NomMachine | Valeur = Liste des Apps
        FAppListLock: TCriticalSection; //🔒 Protection contre les accès concurrents
        FLastHintCol, FLastHintRow: Integer;
        FLastIniModified: TDateTime;
        FIniFilePathName: String;
        FSSLHandler: TIdServerIOHandlerSSLOpenSSL;
        Function ContainsApp(Const App: TAppRec): Boolean;
        Procedure UpdateClientList;
        Procedure UpdateUI;
        Function HostName(Context: TIdContext): String;
        Procedure SendMessageToClients(Msg: TAppWatcherMessage);
        Procedure UpdateStringGrid;
        Procedure SortGrid(Column: Integer);
        Procedure CloseServer;
        Procedure UpdateStringGridFilter;
    public
        {Déclarations publiques}
    End;

Var
    FormAppWatcherMaster: TFormAppWatcherMaster;

Implementation

Uses
    AppWatcherMaster_Deploy;

{$R *.dfm}

//Fonction qui renvoie l'adresse IP locale de la machine
Function GetLocalIP: String;
Type
    TaPInAddr = Array[0..10] Of PInAddr; //Tableau de pointeurs vers des adresses IP
    PaPInAddr = ^TaPInAddr; //Pointeur vers le tableau d'adresses IP
Var
    phe: PHostEnt; //Structure contenant des informations sur l'hôte (ordinateur)
    pptr: PaPInAddr; //Pointeur vers la liste d'adresses IP de l'hôte
    buffer: Array[0..63] Of AnsiChar; //Tampon pour stocker le nom d'hôte
    I: Integer; //Variable d'itération
    GInitData: TWSADATA; //Structure pour initialiser l'utilisation de sockets
Begin
    //Initialise le résultat à une chaîne vide
    Result := '';

    //Initialise la bibliothèque Windows Sockets

    WSAStartup($101, GInitData);
    Try
        //Récupère le nom de l'hôte local (ordinateur) et le stocke dans le buffer
        GetHostName(buffer, sizeof(buffer));

        //Utilise le nom de l'hôte pour obtenir les informations associées, telles que les adresses IP
        phe := GetHostByName(buffer);

        //Si l'appel échoue (pas d'information disponible pour l'hôte), quitter la fonction
        If phe = Nil Then
            exit;

        //Pointe sur la liste d'adresses IP de l'hôte
        pptr := PaPInAddr(phe^.h_addr_list);

        //Boucle sur toutes les adresses IP disponibles pour l'hôte
        I := 0;
        While pptr^[I] <> Nil Do Begin
            //Convertit l'adresse IP au format texte et la stocke dans le résultat
            Result := StrPas(inet_ntoa(pptr^[I]^));
            Inc(I); //Passe à l'adresse suivante
        End;
    Finally
        //Libère les ressources utilisées par la bibliothèque Windows Sockets
        WSACleanup;
    End;
End;

Procedure TAppRec.init(Msg: TAppWatcherMessage; Context: TIdContext);
Begin
    AppName := Msg.AppName;
    AppPath := ExtractFilePath(Msg.AppPath);
    ClientIp := Context.Binding.PeerIP;
    ClientName := GStack.HostByAddress(ClientIp);
    UserName := Msg.UserName;
    AppHandle := Msg.Handle;
End;

Class Operator TAppRec.Equal(Const A, B: TAppRec): Boolean;
Begin
    Result := (A.ClientIp = B.ClientIp) And
    (A.AppHandle = B.AppHandle);
End;

Function TFormAppWatcherMaster.HostName(Context: TIdContext): String;
Begin
    Try
        Result := Context.Binding.PeerIP + ' = ' + GStack.HostByAddress(Context.Binding.PeerIP);
    Except
        Result := Context.Binding.PeerIP;
    End;

End;

Function TFormAppWatcherMaster.ContainsApp(Const App: TAppRec): Boolean;
Var
    ExistingApp: TAppRec;
Begin
    Result := False;
    For ExistingApp In FAppList Do
        If (ExistingApp = App) Then
            exit(True); //Dès qu'on trouve un match, on sort immédiatement
End;

Procedure TFormAppWatcherMaster.FormCreate(Sender: TObject);
Var
    LangValue, ServerIP, ServerIPini: String;
    Ini: TIniFile;
    Msg: String;
    DoClose: Boolean;
Begin
    DoClose := False;
    FIsFirstLoad := True;
    FSelColumn := 0;
    FSortColumn := 0;

    Try
        //🔹 Vérification des paramètres en ligne de commande (-lang:fr ou -lang:en)
        If FindCmdLineSwitch('lang', LangValue, True, [clstValueNextParam, clstValueAppended]) Then Begin
            LangValue := LowerCase(LangValue);
            If (LangValue <> 'fr') And (LangValue <> 'en') Then
                FLang := LangFr; //🔴 Sécurisation : éviter une langue inconnue
        End Else
            FLang := LangFr;

        //🔹 Trouver le fichier de configuration
        FIniFilePathName := FindConfigPath(AppWatcherIniFileNAme);

        If FIniFilePathName = '' Then Begin
            //Le fichier INI est introuvable
            If FLang = LangEn Then
                Msg := format(MsgIniFileNotFoundEn, [AppWatcherIniFileNAme])
            Else
                Msg := format(MsgIniFileNotFoundFr, [AppWatcherIniFileNAme]);

            MessageDlg(Msg, mtError, [mbok], 0);
            DoClose := True;
        End;

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
            DoClose := True;
            FreeAndNil(FLanguageManager);
        End;

        If DoClose Then
            Application.terminate;

        FAppList := TList<TAppRec>.Create(TDelegatedComparer<TAppRec>.Create(
            Function(Const Left, Right: TAppRec): Integer
            Begin
                Result := CompareText(Left.ClientName, Right.ClientName);
                If Result = 0 Then
                    Result := CompareText(Left.AppName, Right.AppName);
            End));

        FAppListLock := TCriticalSection.Create; //🔒 Création du verrou

        UpdateUI;

        IdTCPServer1.Active := False; //le serveur est bien désactivé

        FLastIniModified := TFile.GetLastWriteTime(FIniFilePathName);
        Ini := TIniFile.Create(FIniFilePathName);
        Try
            //Charger l'IP du serveur et le port
            ServerIPini := Ini.ReadString('MasterConfig', 'ServerIP', '127.0.0.1');
            ServerIP := GetLocalIP;
            //Si l'adresse dans le fichier ini ne correspond pas à l'IP alors on l'écrase
            //De cette manière les clients se connecterons au serveur en cours
            //Attention car c'est le dernier serveur lancé qui prend la main
            If ServerIP <> ServerIPini Then
                Ini.WriteString('MasterConfig', 'ServerIP', ServerIP);

        Finally
            Ini.Free;
        End;

        IdTCPServer1.Active := True; //Activation du serveur après la configuration
    Finally
        FIsFirstLoad := False;
    End;
End;

Procedure TFormAppWatcherMaster.CloseServer;
Var
    Context: TIdContext;
    ContextList: TList<TIdContext>;
Begin
    //🔹 Empêcher de nouvelles connexions
    IdTCPServer1.StopListening;

    //🔹 Fermer toutes les connexions en cours
    ContextList := TList<TIdContext>(IdTCPServer1.Contexts.LockList);
    Try
        For Context In ContextList Do Begin
            Try
                Context.Connection.Disconnect;
            Except
                //Ignorer toute exception lors de la fermeture
            End;
        End;
    Finally
        IdTCPServer1.Contexts.UnlockList;
    End;

    //🔹 Désactiver le serveur une fois que tout est terminé
    IdTCPServer1.Active := False;
End;

Procedure TFormAppWatcherMaster.FormClose(Sender: TObject; Var Action: TCloseAction);
Var
    ClientIp: String;
    Context: TIdContext;

Begin
    TimerupdateClient.enabled := False;
    //🔴 Arrêt propre du serveur avant destruction
    CloseServer;

    FAppListLock.Enter; //🔒 Bloque l'accès pendant la destruction
    Try
        FAppList.Free;
    Finally
        FAppListLock.Leave; //🔓 Libère le verrou après destruction
        FAppListLock.Free; //🔥 Supprime l'objet de verrouillage
    End;

    MemoLogs.Lines.Add(FLanguageManager.GetMessage('MASTER', 'SERVER_STOPPED'));
    FreeAndNil(FLanguageManager);
    FreeAndNil(FormDeployManager);
End;

Function SQLLike(Const Pattern, Text: String): Boolean;
Var
    RegexPattern: String;
Begin
    If (trim(Pattern) = '') Or (trim(Pattern) = '%') Then
        exit(True);

    RegexPattern := '^' + TRegEx.Escape(Pattern) + '$';
    RegexPattern := ReplaceStr(RegexPattern, '\%', '\x01');
    RegexPattern := ReplaceStr(RegexPattern, '\_', '\x02');
    RegexPattern := ReplaceStr(RegexPattern, '%', '.*');
    RegexPattern := ReplaceStr(RegexPattern, '_', '.');
    RegexPattern := ReplaceStr(RegexPattern, '\x01', '%');
    RegexPattern := ReplaceStr(RegexPattern, '\x02', '_');

    Result := TRegEx.IsMatch(Text, RegexPattern, [roIgnoreCase]);
End;

Procedure TFormAppWatcherMaster.UpdateStringGrid;
Var
    idxGrid, idxApp, FilteredCount, i: Integer;
    FilteredList: TList<String>;
    IsFiltered: Boolean;

    Function filterAccept(I: Integer): Boolean;
    Begin
        Result := SQLLike(StringGridApp.Cells[0, 1], FAppList[I].ClientIp) And
        SQLLike(StringGridApp.Cells[1, 1], FAppList[I].ClientName) And
        SQLLike(StringGridApp.Cells[2, 1], FAppList[I].UserName) And
        SQLLike(StringGridApp.Cells[3, 1], FAppList[I].AppName) And
        SQLLike(StringGridApp.Cells[4, 1], FAppList[I].AppHandle.ToString) And
        SQLLike(StringGridApp.Cells[5, 1], FAppList[I].AppPath);
        If IsFiltered Then
            Result := Result And
            FilteredList.Contains(LowerCase(FAppList[I].AppName));

    End;

Begin
    //Désactiver les mises à jour visuelles pendant la mise à jour du contenu
    StringGridApp.BeginUpdate;
    Try
        FAppListLock.Enter; //🔒 Protection contre l'accès concurrent
        Try
            FilteredList := TList<String>.Create;

            //Efface tout sauf l’en-tête
            //🔄 Ajuster le nombre de lignes (au moins 2 pour éviter le bug)
            If FAppList.Count = 0 Then Begin
                StringGridApp.RowCount := 2;
                //StringGridApp.Rows[1].Clear; //Garde une ligne vide si la liste est vide
                exit;
            End;

            // Collecte les fichiers cochés dans AppList

            IsFiltered := false;
            If (BtnToggleFilter.State = tssOn) And Assigned(FormDeployManager) And (FormDeployManager.AppList.Items.Count > 0) Then Begin
                IsFiltered := true;
                FormDeployManager.GetAppCheckedList(FilteredList);
            End;

            //🔹 Étape 1 : Compter combien de lignes seront affichées après filtrage
            FilteredCount := 0;
            For idxApp := 0 To FAppList.Count - 1 Do
                If filterAccept(idxApp) Then
                    Inc(FilteredCount);

            //🔹 Étape 2 : Définir RowCount en une seule fois
            StringGridApp.RowCount := Max(2, FilteredCount + 2); //+2 pour les en-têtes et filtres

            //Remplit le `StringGrid` avec les données triées
            idxGrid := 2;
            For idxApp := 0 To FAppList.Count - 1 Do Begin
                If filterAccept(idxApp) Then Begin
                    StringGridApp.Cells[0, idxGrid] := FAppList[idxApp].ClientIp;
                    StringGridApp.Cells[1, idxGrid] := FAppList[idxApp].ClientName;
                    StringGridApp.Cells[2, idxGrid] := FAppList[idxApp].UserName;

                    StringGridApp.Cells[3, idxGrid] := FAppList[idxApp].AppName;
                    StringGridApp.Cells[4, idxGrid] := FAppList[idxApp].AppHandle.ToString;
                    StringGridApp.Cells[5, idxGrid] := FAppList[idxApp].AppPath;
                    Inc(idxGrid);
                End;
            End;

            StringGridApp.Row := 1; //Repositionne sur la ligne de filtrage après tri
            StringGridApp.Col := FSelColumn; //Garde la colonne sélectionnée
        Finally
            FAppListLock.Leave; //🔓 Libère le verrou après mise à jour
        End;
    Finally
        //Réactiver les mises à jour visuelles après la mise à jour du contenu
        StringGridApp.EndUpdate;
        FreeAndNil(FilteredList);
    End;
End;

Procedure TFormAppWatcherMaster.BtnToggleFilterClick(Sender: TObject);
// Filter avec la liste des applications à déployer
Begin
    If FAppList.Count = 0 Then
        BtnListAppsClick(Nil)
    Else
        UpdateStringGrid;
End;

Procedure TFormAppWatcherMaster.UpdateStringGridFilter;
Begin

    If (BtnToggleFilter.State = tssOn) Then
        UpdateStringGrid
    Else
        BtnToggleFilter.State := tssOn;

End;

Procedure TFormAppWatcherMaster.SortGrid(Column: Integer);
Begin

    If (FSortColumn = Column) And (FSortAscending = Not FSortAscending) Then
        exit; //🔄 Évite de trier inutilement

    //Inverser le sens si on clique sur la même colonne
    If FSortColumn = Column Then
        FSortAscending := Not FSortAscending
    Else
        FSortAscending := True; //Par défaut, tri ascendant

    FSortColumn := Column;

    //Trier la liste FAppList
    FAppList.Sort(TComparer<TAppRec>.Construct(
        Function(Const Left, Right: TAppRec): Integer
        Begin
            Case Column Of
                0: Begin
                        Result := CompareText(Left.ClientIp, Right.ClientIp);
                        If Result = 0 Then
                            Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 Then
                            Result := Left.AppHandle - Right.AppHandle;
                    End;
                1: Begin
                        Result := CompareText(Left.ClientName, Right.ClientName);
                        If Result = 0 Then
                            Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 Then
                            Result := Left.AppHandle - Right.AppHandle;
                    End;

                2: Begin
                        Result := CompareText(Left.UserName, Right.UserName);
                        If Result = 0 Then
                            Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 Then
                            Result := Left.AppHandle - Right.AppHandle;
                    End;

                3: Begin
                        Result := CompareText(Left.AppName, Right.AppName);
                        If Result = 0 Then
                            Result := Left.AppHandle - Right.AppHandle;
                    End;
                4:
                    Result := Left.AppHandle - Right.AppHandle;
                5: Begin
                        Result := CompareText(Left.AppPath, Right.AppPath);
                        If Result = 0 Then
                            Result := CompareText(Left.ClientName, Right.ClientName);
                    End;
            Else
                Result := 0;
            End;

            //Inverser si tri descendant
            If Not FSortAscending Then
                Result := -Result;
        End
        ));

    //Rafraîchir l'affichage
    UpdateStringGrid;
End;

Procedure TFormAppWatcherMaster.StringGridAppKeyDown(Sender: TObject; Var Key:
    Word; Shift: TShiftState);
Var
    I: Integer;
Begin
    If (Key = VK_ESCAPE) And (StringGridApp.Row = 1) Then Begin
        For I := 0 To StringGridApp.ColCount - 1 Do
            StringGridApp.Cells[I, 1] := ''; //Efface toutes les cellules de la ligne de filtre
        UpdateStringGrid;
    End;
    If (Key = VK_RETURN) And (StringGridApp.Row = 1) Then Begin
        UpdateStringGrid;
    End;

    If StringGridApp.Row <> 1 Then Begin
        Key := 0;
    End;
End;

Procedure TFormAppWatcherMaster.StringGridAppKeyPress(Sender: TObject; Var Key:
    Char);
Begin
    If StringGridApp.Row <> 1 Then Begin
        Key := #0;
    End;
End;

Procedure TFormAppWatcherMaster.StringGridAppSetEditText(Sender: TObject; ACol,
    ARow: LongInt; Const Value: String);
Begin
    If ARow = 1 Then
        FSelColumn := ACol;
End;

Procedure TFormAppWatcherMaster.StringGridAppDblClick(Sender: TObject);
Var
    SelectedRow: Integer;
Begin
    SelectedRow := StringGridApp.Row; //Récupère la ligne sélectionnée

    //Vérifie que l'utilisateur ne clique pas sur l'en-tête (ligne 0)
    If (SelectedRow > 0) And (SelectedRow < StringGridApp.RowCount) Then Begin
        EditAppName.Text := StringGridApp.Cells[3, SelectedRow];
    End;
End;

Procedure TFormAppWatcherMaster.StringGridAppFixedCellClick(Sender: TObject;
    ACol, ARow: LongInt);
Begin
    //if ARow = 0 then //Vérifie qu'on clique bien sur l'en-tête
    //FAppListLock.Enter;
    Try
        Begin
            SortGrid(ACol); //Trie en fonction de la colonne
        End;
    Finally
        //FAppListLock.Leave;
    End;
End;

Procedure TFormAppWatcherMaster.StringGridAppDrawCell(Sender: TObject; ACol,
    ARow: LongInt; Rect: TRect; State: TGridDrawState);
Var
    TextRect: TRect;
    Flags: Integer;
    I: Integer;
    posLeft: Integer;
    IsFilterActive: Boolean;
    CellText: String;
Begin
    //Récupérer le texte de la cellule
    CellText := StringGridApp.Cells[ACol, ARow];

    If ARow = 0 Then {//Si c'est la ligne d'en-tête}Begin

        //        StringGridApp.Canvas.Brush.Color := clGray; //Fond gris
        //        StringGridApp.Canvas.Font.Color := clWhite; //Texte blanc
        //        StringGridApp.Canvas.Font.Style := [fsBold]; //Texte en gras

    End Else If ARow = 1 Then {//Si c'est la ligne de filtres}Begin
        //Vérifier si un filtre est appliqué
        IsFilterActive := StringGridApp.Cells[ACol, 1] <> '';
        StringGridApp.Canvas.Font.Style := []; //Texte normal

        If IsFilterActive Then Begin
            StringGridApp.Canvas.Brush.Color := clWebOrange; //Filtre actif
            StringGridApp.Canvas.Font.Color := clWhite; //Texte noir

        End Else Begin
            StringGridApp.Canvas.Brush.Color := clInfoBk; //Filtre inactif
            StringGridApp.Canvas.Font.Color := clBlack; //Texte noir
        End;

    End Else {//Contenu normal}Begin
        StringGridApp.Canvas.Font.Style := []; //Texte normal
        StringGridApp.Canvas.Brush.Color := clWhite;
        StringGridApp.Canvas.Font.Color := clBlack;
    End;

    //Centrage vertical du texte avec `DrawText`
    TextRect := Rect;
    Inc(TextRect.Left, 6);
    Flags := DT_LEFT Or DT_VCENTER Or DT_SINGLELINE;

    StringGridApp.Canvas.FillRect(Rect); //Remplissage du fond
    DrawText(StringGridApp.Canvas.Handle, PChar(CellText), Length(CellText), TextRect, Flags);

End;

Procedure TFormAppWatcherMaster.UpdateUI;
Var
    section: String;
Begin

    //🌍 Mise à jour des boutons
    section := 'MASTER_UI';
    Self.caption := FLanguageManager.GetMessage(section, 'TITLE');

    BtnListApps.caption := FLanguageManager.GetMessage(section, 'BTN_LIST_APPS');
    BtnAppCopy.caption := FLanguageManager.GetMessage(section, 'BTN_LIST_DEPLOY');
    BtnToggleFilter.StateCaptions.CaptionOn := FLanguageManager.GetMessage(section, 'BTN_FILTER_ON');
    BtnToggleFilter.StateCaptions.CaptionOff := FLanguageManager.GetMessage(section, 'BTN_FILTER_OFF');

    BtnStopApp.caption := FLanguageManager.GetMessage(section, 'BTN_STOP_APP');
    BtnStart.caption := FLanguageManager.GetMessage(section, 'BTN_START');
    BtnCancel.caption := FLanguageManager.GetMessage(section, 'BTN_CANCEL');
    BtnAgentStop.caption := FLanguageManager.GetMessage(section, 'BTN_STOP_AGENT');
    BtnAgentStart.caption := FLanguageManager.GetMessage(section, 'BTN_START_AGENT');

    //🏷️ Mise à jour des labels
    LblAppName.caption := FLanguageManager.GetMessage(section, 'LBL_APP_NAME');
    LblDuration.caption := FLanguageManager.GetMessage(section, 'LBL_DURATION');
    ChkSilent.Caption := FLanguageManager.GetMessage(section, 'CHK_SILENT');
    LblClientList.caption := FLanguageManager.GetMessage(section, 'LBL_CLIENT_LIST');
    LblMsg.caption := FLanguageManager.GetMessage(section, 'LBL_MSG');
    LblAppList.caption := FLanguageManager.GetMessage(section, 'LBL_APPLIST');

    //📜 Mise à jour des groupes radio
    RdioFrench.caption := FLanguageManager.GetMessage(section, 'RDIO_FRENCH');
    RdioEnglish.caption := FLanguageManager.GetMessage(section, 'RDIO_ENGLISH');

    //Définition des en-têtes de la StringGRID
    StringGridApp.Cells[0, 0] := FLanguageManager.GetMessage(section, 'STRGRID_IP');
    StringGridApp.Cells[1, 0] := FLanguageManager.GetMessage(section, 'STRGRID_CLINAME');
    StringGridApp.Cells[2, 0] := FLanguageManager.GetMessage(section, 'STRGRID_USERNAME');
    StringGridApp.Cells[3, 0] := FLanguageManager.GetMessage(section, 'STRGRID_APPNAME');
    StringGridApp.Cells[4, 0] := FLanguageManager.GetMessage(section, 'STRGRID_HANDLE');
    StringGridApp.Cells[5, 0] := FLanguageManager.GetMessage(section, 'STRGRID_PATH');
End;

Procedure TFormAppWatcherMaster.SendMessageToClients(Msg: TAppWatcherMessage);
Var
    Context: TIdContext;
    ContextList: TList<TIdContext>;

    RefMsg: String;
Begin
    Case Msg.Command Of
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
        cmdRESTART_AGENT:
            RefMsg := 'RESTART_AGENT_SENT';
    Else
        RefMsg := 'UNKNOWN_COMMAND';
    End;

    //Verrouiller la liste
    //ContextList := IdTCPServer1.Contexts.LockList;
    ContextList := TList<TIdContext>(IdTCPServer1.Contexts.LockList);
    Try
        For Context In ContextList Do Begin
            Try
                Msg.SendMessage(Context.Connection.IOHandler);
                MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', RefMsg), [HostName(Context), Msg.AppName, IntToStr(Msg.Handle)]));
            Except
                MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ERROR_CONTACTING_CLIENT'), [HostName(Context)]));
            End;
        End;
    Finally
        IdTCPServer1.Contexts.UnlockList; //Déverrouiller proprement après la boucle
    End;

End;

Procedure TFormAppWatcherMaster.BtnListAppsClick(Sender: TObject);
// Réinitialiser la liste des applications en cours d'execution
Var
    Msg: TAppWatcherMessage;
Begin
    FAppList.Clear;
    UpdateStringGrid;
    Msg.init(0, cmdWHO, '', '', 0); //Commande LIST
    SendMessageToClients(Msg);

End;

Procedure TFormAppWatcherMaster.UpdateClientList;
Var
    ContextList: TList<TIdContext>;
    Context: TIdContext;
    Liste: String;
    s: String;
Begin
    Liste := '';
    //ListViewClients.Clear;

    ContextList := TList<TIdContext>(IdTCPServer1.Contexts.LockList);
    Try
        For Context In ContextList Do {//Boucle classique sans `in`}Begin
            Liste := Liste + HostName(Context) + #13#10;
            //ListViewClients.Lines.Add(HostName(Context));
        End;
    Finally
        IdTCPServer1.Contexts.UnlockList; //Toujours libérer la liste !
    End;

    If Liste <> '' Then
        setlength(Liste, Length(Liste) - 2);
    If ListViewClients.Lines.Text <> Liste Then Begin
        ListViewClients.Lines.Text := Liste;
        s := FLanguageManager.GetMessage('MASTER', 'CLIENTS_LIST_UPDATED');
        MemoLogs.Lines.Add(s);
    End;
End;


Procedure TFormAppWatcherMaster.BtnStartClick(Sender: TObject);
Var
    Msg: TAppWatcherMessage;
Begin
    Msg.init(0, cmdSTART, '', '', 0,chkSilent.Checked); //Commande START
    SendMessageToClients(Msg);
End;

Procedure TFormAppWatcherMaster.BtnStopAppClick(Sender: TObject);
Var
    Msg: TAppWatcherMessage;
    AppName: String;
Begin
    AppName := trim(EditAppName.Text);
    If AppName = '' Then Begin
        MemoLogs.Lines.Add(FLanguageManager.GetMessage('MASTER', 'ENTER_PROGRAM_NAME'));
        exit;
    End;

    Msg.init(0, cmdSTOP, AppName, '', RzNumericEdit1.IntValue,chkSilent.Checked); //Commande STOP
    SendMessageToClients(Msg);
End;

Procedure TFormAppWatcherMaster.BtnAgentStopClick(Sender: TObject);
Var
    Msg: TAppWatcherMessage;
Begin
    If MessageDlg(FLanguageManager.GetMessage('MASTER', 'STOP_AGENT_REQUEST'), mtConfirmation, [mbyes, mbNo], 0, mbNo) = mryes Then Begin
        Msg.init(0, cmdSTOP_AGENT, '', '', 0); //Commande STOP_AGENT
        SendMessageToClients(Msg);
    End;
End;

procedure TFormAppWatcherMaster.BtnAgentStartClick(Sender: TObject);
Var
    Msg: TAppWatcherMessage;
Begin
        Msg.init(0, cmdRESTART_AGENT, '', '', 0,chkSilent.Checked); //Commande RESTART_AGENT
        SendMessageToClients(Msg);
End;


Procedure TFormAppWatcherMaster.BtnAppCopyClick(Sender: TObject);
Begin
    If Not Assigned(FormDeployManager) Then
        FormDeployManager := TFormDeployManager.Create(Nil);

    FormDeployManager.Execute(FLanguageManager);
End;

Procedure TFormAppWatcherMaster.BtnCancelClick(Sender: TObject);
Var
    Msg: TAppWatcherMessage;
Begin
    Msg.init(0, cmdCANCEL, '', '', 0,chkSilent.Checked); //Commande CANCEL
    SendMessageToClients(Msg);
End;

Procedure TFormAppWatcherMaster.IdTCPServer1Connect(AContext: TIdContext);
Begin
    //✅ Forcer l'UI à se mettre à jour dans le thread principal
    TThread.Queue(Nil,
        Procedure
        Begin
            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'CLIENT_CONNECTED'), [HostName(AContext)]));
        End);
    sleep(50);
End;

Procedure TFormAppWatcherMaster.IdTCPServer1Disconnect(AContext: TIdContext);
Var
    Name: String;
Begin
    Name := HostName(AContext);
    TThread.Queue(Nil,
        Procedure
        Begin
            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'CLIENT_DISCONNECTED'), [Name]));
        End);
End;

Procedure TFormAppWatcherMaster.TimerupdateClientTimer(Sender: TObject);
Var
    ini: TIniFile;
    ServerIPini, ServerIP: String;
Begin
    // Vérifier si le MASTER est toujour MASTER
    If FileExists(FIniFilePathName) And (FLastIniModified <> TFile.GetLastWriteTime(FIniFilePathName)) Then Begin
        FLastIniModified := TFile.GetLastWriteTime(FIniFilePathName);
        Ini := TIniFile.Create(FIniFilePathName);
        Try
            //Charger l'IP du serveur et le port
            ServerIPini := Ini.ReadString('MasterConfig', 'ServerIP', '127.0.0.1');
            ServerIP := GetLocalIP;
            //Si l'adresse dans le fichier ini ne correspond pas à l'IP alors celà veux dire que ce serveur à perdu le contrôle

            If ServerIP <> ServerIPini Then
                MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'LOST_CONTROL'), [ServerIPini]));

        Finally
            Ini.Free;
        End;
    End;
    Try
        TimerupdateClient.Enabled := false;

        UpdateClientList; //Met à jour la liste des clients
    Finally
        TimerupdateClient.Enabled := true;
    End;
End;

Procedure TFormAppWatcherMaster.IdTCPServer1Execute(AContext: TIdContext);
//Procedure de reception des messages depuis les AGENTS
Var
    ReceivedMsg: TAppWatcherMessage;

Begin

    Try
        If Not ReadMessage(AContext.Connection.IOHandler, ReceivedMsg) Then Begin
            sleep(300);
            exit;
        End;

        TThread.Queue(Nil,
            Procedure
            Var
                App: TAppRec;
                I: Integer;
            Begin
                Case ReceivedMsg.Command Of
                    cmdWHO_REPLY: Begin
                            App.init(ReceivedMsg, AContext);

                            //📝 Mettre à jour la liste en mémoire
                            FAppListLock.Enter;
                            Try
                                If Not ContainsApp(App) Then
                                    FAppList.Add(App);

                                //🔽 Créer une liste triable à partir des valeurs du dictionnaire
                                FAppList.Sort;
                            Finally
                                FAppListLock.Leave; //🔓 Libère le verrou
                            End;

                            //🔄 Rafraîchir l'affichage
                            UpdateStringGrid;

                            MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'WHO_REPLY'),
                                [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));

                        End;
                    cmdACK:
                        MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ACK_REPLY'),
                            [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                    cmdNACK:
                        MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'NACK_REPLY'),
                            [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                Else
                    MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'UNKNOW_REPLY'),
                        [HostName(AContext), ReceivedMsg.AppName, IntToStr(ReceivedMsg.Handle)]));
                End;

            End);

    Except
        On E: Exception Do Begin
            TThread.Queue(Nil,
                Procedure
                Begin
                    MemoLogs.Lines.Add(format(FLanguageManager.GetMessage('MASTER', 'ERROR_RECEIVING_ANSWER'), [E.Message]));
                End);
        End;
    End;
End;

Procedure TFormAppWatcherMaster.RdioEnglishClick(Sender: TObject);
Begin
    If FIsFirstLoad Then
        exit;

    FLang := LangEn;
    FLanguageManager.LoadLanguage(LangEn);
    UpdateUI
End;

Procedure TFormAppWatcherMaster.RdioFrenchClick(Sender: TObject);
Begin
    If FIsFirstLoad Then
        exit;

    FLang := LangFr;
    FLanguageManager.LoadLanguage(LangFr);
    UpdateUI
End;

Procedure TFormAppWatcherMaster.StringGridAppMouseMove(Sender: TObject; Shift:
    TShiftState; X, Y: Integer);
Var
    Col, Row: Integer;
    CellRect: TRect;
    CellText, HintText: String;
    MousePos: TPoint;
Begin
    //Déterminer la cellule sous la souris
    StringGridApp.MouseToCell(X, Y, Col, Row);

    //Vérifier si la cellule est valide
    If (Col >= 0) And (Row >= 0) Then Begin
        StringGridApp.MouseToCell(X, Y, Col, Row);

        //N'affiche le hint que si on change de cellule
        If (Col <> FLastHintCol) Or (Row <> FLastHintRow) Then Begin
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
        End;
    End;
End;

End.

