(*******************************************************************************
  Project : AppWatcher
  Unit    : AppWatcherMaster_Deploy.pas
  Author  : mbaumsti
  GitHub  : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date    : 07/03/2025
  Version : 3.1
  License : MIT

  Description :
  -------------
  This unit allows to manage the list of files to install (executables and others)
  from their source location to the destination location in production.

  Features :
  -----------
- Add a file to the list
- Remove a file
- Select files to update
- Ask the Master to control the use of the file in production
- Start the update and copy the files from the source to the destination

  Change Log :
  ------------
  - [07/03/2025] : Initial creation
  - [10/03/2025] : v2.0.1 Bug in item deletion and selection after sorting (DeleteCopyItem and AppListDblClick )
                          adding `GetRealIndexFromSelected` who retrieves the correct index
  - [21/04/2025] : v3.1.0  - Default sorting on the status column
                           - Deployment with executable backup rotations

  Note :
  -------
  This project is open-source. Contributions are welcome!
  *******************************************************************************)

Unit AppWatcherMaster_Deploy;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, System.JSON, Vcl.StdCtrls, Vcl.CheckLst,
    System.IOUtils, System.DateUtils, System.Generics.Collections, AppWatcher_Lang, AppWatcher_consts,
    system.types, inifiles;

Type

    TDeployItem = Record
        Name, SourceDir, DestDir: String;
    End;

    TFormDeployManager = Class(TForm)
        AppList: TListView;
        BtnRefresh: TButton;
        BtnAddFile: TButton;
        BtnDel: TButton;
        BtnDeployExecute: TButton;
        BtnModif: TButton;
        BtnVersions: TButton;
        Procedure BtnRefreshClick(Sender: TObject);
        Procedure BtnAddFileClick(Sender: TObject);
        Procedure AppListDblClick(Sender: TObject);
        Procedure FormCreate(Sender: TObject);
        Procedure BtnDelClick(Sender: TObject);
        Procedure CreateParams(Var Params: TCreateParams); override;
        Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
        Procedure BtnDeployExecuteClick(Sender: TObject);
        Procedure AppListColumnClick(Sender: TObject; Column: TListColumn);
        Procedure BtnModifClick(Sender: TObject);
        Procedure BtnVersionsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    private
        { Déclarations privées }
        FColumns: TDictionary<String, TListColumn>;
        FCopyList: Array Of TDeployItem;
        FLanguageManager: TAppLangManager;
    private
        FSortColumn: Integer; // Colonne actuellement triée
        FSortAscending: Boolean; // Sens du tri
        Procedure SaveToJSON;
        Procedure CreateListColumns;
        Procedure AddCopyItem(Const Filename, AName, ASourceDir, ADestDir: String);
        Procedure DeleteCopyItem(Index: integer);
        Procedure RefreshListView(ListView: TListView);
        Procedure LoadFromJSON(ListView: TListView);
        Function GetRealIndexFromSelected: Integer;
    public
        { Déclarations publiques }
        Procedure GetAppCheckedList(Var ChkLst: TList<String>);
        Procedure Execute(LngManager: TAppLangManager);

    End;

Var
    FormDeployManager: TFormDeployManager;

Implementation

Uses
    AppWatcherMaster_AddFile, AppWatcherMaster_Restore, AppWatcherMaster_Backup, AppWatcherMaster_DeployOptions;

{$R *.dfm}

Const
    cColSel: String = 'Sel';
    cColStatus: String = 'Status';
    cColFileName: String = 'FileName';
    cColSourceDir: String = 'SourceDir';
    cColSourceDate: String = 'SourceDate';
    cColDestDir: String = 'DestDir';
    cColDestDate: String = 'DestDate';
    cColIdx: String = 'idx';

    //=================================================================================================================

Function CompareAppItems(Item1, Item2: TListItem; ParamSort: Integer): Integer; stdcall;
Var
    Text1, Text2: String;
    SortColumn: Integer;
    SortAscending: Boolean;
Begin
    // Extraire les paramètres
    SortColumn := LoWord(ParamSort); // Colonne à trier
    SortAscending := HiWord(ParamSort) <> 0; // Sens du tri

    // Sécurité : vérifier que l'index de colonne est valide
    If (SortColumn < 1) Or (SortColumn - 1 >= Item1.SubItems.Count) Then
        Exit(0);

    // Récupérer les valeurs des cellules pour la comparaison
    Text1 := Item1.SubItems[SortColumn - 1]; // -1 car la première colonne est `Caption`
    Text2 := Item2.SubItems[SortColumn - 1];

    If (StrToIntDef(Text1, -1) <> -1) And (StrToIntDef(Text2, -1) <> -1) Then Begin
        // Ce sont des colonnes numériques
        Text1 := Text1.PadLeft(10, '0');
        Text2 := Text2.PadLeft(10, '0');
    End;

    // Appliquer le tri
    If SortAscending Then
        Result := CompareText(Text1, Text2) // Tri croissant
    Else
        Result := CompareText(Text2, Text1); // Tri décroissant
End;

//=============================================================================================================

Function TFormDeployManager.GetRealIndexFromSelected: Integer;
Begin
    Result := -1;
    If (AppList.Selected = Nil) Or (AppList.Selected.Index < 0) Then Exit;

    // 🔹 Lire l'index réel depuis la colonne cachée
    Result := StrToIntDef(AppList.Selected.SubItems[FColumns[cColIdx].Index - 1], -1);
End;

Procedure TFormDeployManager.GetAppCheckedList(Var ChkLst: TList<String>);
Var
    i: Integer;
Begin
    If Not Assigned(ChkLst) Then
        ChkLst := TList<String>.Create;
    ChkLst.Clear;
    For i := 0 To AppList.Items.Count - 1 Do Begin
        If AppList.Items[i].Checked Then
            ChkLst.Add(LowerCase(AppList.Items[i].SubItems[FColumns[cColFileName].Index - 1]));
    End;
End;

Procedure TFormDeployManager.CreateParams(Var Params: TCreateParams);
Begin
    Inherited CreateParams(Params);
    With Params Do Begin
        ExStyle := ExStyle Or WS_EX_APPWINDOW; // Rend la fenêtre indépendante
        WndParent := GetDesktopWindow; // Déclare la fenêtre comme principale
    End;
End;

Procedure TFormDeployManager.Execute(LngManager: TAppLangManager);
Var
    SortParam: integer;
Begin
    FLanguageManager := LngManager;

    Self.caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'TITLE');
    FColumns[cColSel].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_SEL');
    FColumns[cColStatus].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_STATUS');
    FColumns[cColFileName].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_FILENAME');
    FColumns[cCOlSourceDir].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_SOURCEDIR');
    FColumns[cColSourceDate].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_SOURCEDATE');
    FColumns[cColDestDir].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_DESTDIR');
    FColumns[cColDestDate].Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'COL_DESTDATE');

    BtnRefresh.Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'BTN_REFRESH');
    BtnAddFile.Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'BTN_ADDFILE');
    BtnDel.Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'BTN_DELFILE');
    BtnDeployExecute.Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'BTN_DEPLOY');
    BtnVersions.Caption := FLanguageManager.GetMessage('MASTER_DEPLOY', 'BTN_VERSIONS');

    LoadFromJSON(AppList);
    FSortColumn := FColumns[cColStatus].Index; // tri par défaut sur la colonne status
    FSortAscending := True; // On commence en ascendant

    // Encoder les paramètres dans un Integer pour les transmettre
    SortParam := MakeLong(FSortColumn, Ord(FSortAscending));

    // Trier la liste
    AppList.CustomSort(@CompareAppItems, SortParam);

    Show;
    //    SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE);
End;

Procedure TFormDeployManager.FormClose(Sender: TObject;
    Var Action: TCloseAction);
Begin

    FreeAndNil(DlgAddFile);
End;

Procedure TFormDeployManager.FormCreate(Sender: TObject);
Begin
    FSortColumn := -1; // Aucun tri au début
    FSortAscending := True;

    CreateListColumns;
End;

procedure TFormDeployManager.FormDestroy(Sender: TObject);
begin
   FreeAndNil(FColumns);
end;

Procedure TFormDeployManager.CreateListColumns;
Var
    Col: TListColumn;

    Procedure AddColumn(Const AColumnName, Acaption: String; AWidth: Integer);
    Begin
        FColumns.Add(AColumnName, AppList.Columns.Add());
        FColumns[AColumnName].Caption := Acaption;
        FColumns[AColumnName].Width := AWidth;
    End;

Begin
    FColumns := TDictionary<String, TListColumn>.Create();
    AddColumn(cColSel, 'Sél.', 60);
    AddColumn(cColStatus, 'Statut', 100);
    AddColumn(cColFileName, 'Fichier', 150);
    AddColumn(cColSourceDir, 'Source', 250);
    AddColumn(cColSourceDate, 'Date source', 150);
    AddColumn(cColDestDir, 'Destination', 250);
    AddColumn(cColDestDate, 'Date Destination', 150);
    // 🔹 Ajout d'une colonne INVISIBLE pour stocker l'index réel
    Col := AppList.Columns.Add;
    Col.Caption := 'Index';
    Col.Width := 0; // Rend la colonne totalement invisible
    Col.AutoSize := False;
    FColumns.Add(cColIdx, Col);
End;

Procedure TFormDeployManager.SaveToJSON;
// Sauver vers JSON
Var
    FilePath: String;
    JsonArr: TJSONArray;
    JsonObj: TJSONObject;
    i: integer;
Begin
    FilePath := FindConfigPath(AppWatcherJsonFileName);
    If FilePath = '' Then exit;

    JsonArr := TJSONArray.Create;
    Try
        For i := 0 To High(FCopyList) Do Begin
            JsonObj := TJSONObject.Create;
            JsonObj.AddPair('Name', FCopyList[i].Name);
            JsonObj.AddPair('SourceDir', FCopyList[i].SourceDir);
            JsonObj.AddPair('DestDir', FCopyList[i].DestDir);
            JsonArr.AddElement(JsonObj);
        End;
        TFile.WriteAllText(FilePath, JsonArr.ToJSON);
    Finally
        JsonArr.Free;
    End;
End;

Procedure TFormDeployManager.AddCopyItem(Const Filename, AName, ASourceDir, ADestDir: String);
Var
    i, idx: Integer;
Begin
    For i := 0 To High(FCopyList) Do
        If SameText(FCopyList[i].Name, AName) And
        SameText(FCopyList[i].SourceDir, ASourceDir) And
        SameText(FCopyList[i].DestDir, ADestDir) Then Begin
            ShowMessage(FLanguageManager.GetMessage('MASTER_DEPLOY', 'MSG_EXIST'));
            Exit;
        End;

    SetLength(FCopyList, Length(FCopyList) + 1);
    idx := High(FCopyList);
    FCopyList[idx].Name := AName;
    FCopyList[idx].SourceDir := ASourceDir;
    FCopyList[idx].DestDir := ADestDir;

    SaveToJSON;
    RefreshListView(AppList);

    // 🔹 Sélectionner automatiquement la nouvelle ligne après ajout
    For i := 0 To AppList.Items.Count - 1 Do Begin
        If SameText(AppList.Items[i].SubItems[FColumns[cColidx].Index - 1], idx.ToString) Then Begin
            AppList.Selected := AppList.Items[i];
            AppList.ItemFocused := AppList.Items[i];
            AppList.Items[i].MakeVisible(False); // 🔹 Fait défiler si besoin
            Break;
        End;
    End;

End;

Procedure TFormDeployManager.DeleteCopyItem(Index: integer);
// Supprimer une entrée
Var
    i: integer;
Begin
    If (Index < 0) Or (Index > High(FCopyList)) Then Exit;

    For i := Index To High(FCopyList) - 1 Do
        FCopyList[i] := FCopyList[i + 1];

    SetLength(FCopyList, Length(FCopyList) - 1);

    SaveToJSON;
    RefreshListView(AppList);
End;

Procedure TFormDeployManager.BtnAddFileClick(Sender: TObject);
Var
    Source, Dest: String;

Begin
    Source := '';
    Dest := '';

    If AppList.Selected <> Nil Then Begin

        Source := TPath.Combine(
            AppList.Selected.SubItems[FColumns[cColSourceDir].Index - 1],
            AppList.Selected.SubItems[FColumns[cColFileName].Index - 1]
            );
        Dest := AppList.Selected.SubItems[FColumns[cColDestDir].Index - 1];
    End;

    If Not Assigned(DlgAddFile) Then
        DlgAddFile := TDlgAddFile.Create(Self); // Créer avec `Self` comme parent

    Try

        If DlgAddFile.Execute(Source, Dest, FLanguageManager) Then
            AddCopyItem(AppWatcherJsonFileName, ExtractFileName(Source), ExtractFilePath(Source), Dest);

    Finally

        FreeAndNil(DlgAddFile);
    End;
End;

Procedure TFormDeployManager.BtnDeployExecuteClick(Sender: TObject);
Var
    i: Integer;
    SourceFile, DestFile: String;
    CopyStatus: Boolean;
    Settings: TBackupSettings;
    OptDlg: TDlgDeployOptions;

Begin
    If AppList.Items.Count = 0 Then Begin
        ShowMessage(FLanguageManager.GetMessage('MASTER_DEPLOY', 'MSG_NOFILES'));
        Exit;
    End;

    // lecture des valeurs par défaut
    Settings := LoadBackupSettings;

    //  dialogue d’options
    OptDlg := TDlgDeployOptions.Create(Self);
    Try
        If Not OptDlg.Execute(FLanguageManager, Settings) Then
            Exit; // utilisateur a annulé
    Finally
        OptDlg.Free;
    End;

    For i := 0 To AppList.Items.Count - 1 Do Begin
        If Not AppList.Items[i].Checked Then
            Continue; // Ignorer les fichiers non sélectionnés

        // Récupérer les chemins source et destination
        SourceFile := TPath.Combine(
            AppList.Items[i].SubItems[FColumns[cColSourceDir].Index - 1],
            AppList.Items[i].SubItems[FColumns[cColFileName].Index - 1]
            );
        DestFile := TPath.Combine(
            AppList.Items[i].SubItems[FColumns[cColDestDir].Index - 1],
            AppList.Items[i].SubItems[FColumns[cColFileName].Index - 1]
            );

        // Vérifier si le fichier source existe
        If Not FileExists(SourceFile) Then Begin
            ShowMessage(Format(FLanguageManager.GetMessage('MASTER_DEPLOY', 'MSG_NOTFOUND'), [SourceFile]));
            Continue;
        End;

        Try
            // Créer le répertoire destination s'il n'existe pas
            If Not DirectoryExists(ExtractFilePath(DestFile)) Then
                ForceDirectories(ExtractFilePath(DestFile));

            // Sauvegarde avant écrasement
            If FileExists(DestFile) Then
                BackupWithTimestamp(DestFile, Settings);

            // Copier le fichier en écrasant s'il existe déjà
            TFile.Copy(SourceFile, DestFile, True);
            CopyStatus := True;
        Except
            On E: Exception Do Begin
                ShowMessage(Format(FLanguageManager.GetMessage('MASTER_DEPLOY', 'MSG_COPYERROR'), [SourceFile, E.Message]));
                CopyStatus := False;
            End;
        End;

        // Mettre à jour le statut après copie réussie
        If CopyStatus Then Begin
            AppList.Items[i].SubItems[FColumns[cColDestDate].Index - 1] := DateTimeToStr(TFile.GetLastWriteTime(DestFile));
            AppList.Items[i].SubItems[FColumns[cColStatus].Index - 1] := FLanguageManager.GetMessage('MASTER_DEPLOY', 'STATUS_UPTODATE');
            AppList.Items[i].Checked := False; // Désactiver après copie
        End;
    End;

    ShowMessage('Copie terminée !');
End;

Procedure TFormDeployManager.BtnVersionsClick(Sender: TObject);
Var
    RealIdx: Integer;
    DestFile: String;
    Settings: TBackupSettings;
    Dlg: TDlgRestore;
Begin
    RealIdx := GetRealIndexFromSelected;
    If RealIdx = -1 Then Exit;

    DestFile := TPath.Combine(
        FCopyList[RealIdx].DestDir,
        FCopyList[RealIdx].Name);

    Settings := LoadBackupSettings;

    Dlg := TDlgRestore.Create(Self);
    Try
        If Dlg.Execute(DestFile, Settings, FLanguageManager) Then
            RefreshListView(AppList); // dates/statuts mis à jour
    Finally
        Dlg.Free;
    End;
End;

Procedure TFormDeployManager.BtnDelClick(Sender: TObject);
Var
    RealIndex: Integer;
Begin
    RealIndex := GetRealIndexFromSelected;
    If RealIndex <> -1 Then
        DeleteCopyItem(RealIndex);
End;

Procedure TFormDeployManager.AppListColumnClick(Sender: TObject; Column: TListColumn);
Var
    SortParam: Integer;
Begin
    // Vérifier si on reclique sur la même colonne
    If FSortColumn = Column.Index Then
        FSortAscending := Not FSortAscending // Inverser le tri
    Else Begin
        FSortColumn := Column.Index;
        FSortAscending := True; // Nouveau tri, on commence en ascendant
    End;

    // Encoder les paramètres dans un Integer pour les transmettre
    SortParam := MakeLong(FSortColumn, Ord(FSortAscending));

    // Trier la liste
    AppList.CustomSort(@CompareAppItems, SortParam);
End;

Procedure TFormDeployManager.AppListDblClick(Sender: TObject);
Var
    Source, Dest: String;
    RealIndex, i: Integer;
Begin
    RealIndex := GetRealIndexFromSelected;
    If RealIndex = -1 Then Exit;

    Source := TPath.Combine(FCopyList[RealIndex].SourceDir, FCopyList[RealIndex].Name);
    Dest := FCopyList[RealIndex].DestDir;

    If Not Assigned(DlgAddFile) Then
        DlgAddFile := TDlgAddFile.Create(Application);

    Try
        DlgAddFile.PopupMode := pmAuto; // Assure que le dialogue reste attaché à FormCopyManager
        DlgAddFile.PopupParent := Self;

        If DlgAddFile.Execute(Source, Dest, FLanguageManager) Then Begin
            FCopyList[RealIndex].SourceDir := ExtractFilePath(Source);
            FCopyList[RealIndex].Name := ExtractFileName(Source);
            FCopyList[RealIndex].DestDir := Dest;

            SaveToJSON;
            RefreshListView(AppList);

            For i := 0 To AppList.Items.Count - 1 Do Begin
                If SameText(AppList.Items[i].SubItems[FColumns[cColidx].Index - 1], RealIndex.ToString) Then Begin
                    AppList.Selected := AppList.Items[i];
                    AppList.ItemFocused := AppList.Items[i];
                    AppList.Items[i].MakeVisible(False); // 🔹 Fait défiler si besoin
                    Break;
                End;
            End;
        End;
    Finally
        FreeAndNil(DlgAddFile);
    End;
End;

Procedure TFormDeployManager.BtnModifClick(Sender: TObject);
Begin
    AppListDblClick(Nil);
End;

Procedure TFormDeployManager.BtnRefreshClick(Sender: TObject);
Begin
    RefreshListView(AppList);
End;

Procedure TFormDeployManager.RefreshListView(ListView: TListView);
// Actualiser liste
Var
    Item: TDeployItem;
    i: integer;
    SourceFile, DestFile: String;
    NeedsCopy: Boolean;
    SourceDate, DestDate: TDateTime;
    Status: String;
    ListItem: TListItem;
    SortParam: Integer;
Begin
    ListView.Clear;
    For i := 0 To high(FCopyList) Do Begin

        Item := FCopyList[i];

        SourceFile := TPath.Combine(Item.SourceDir, Item.Name);
        DestFile := TPath.Combine(Item.DestDir, Item.Name);

        If Not FileExists(SourceFile) Then Begin
            Status := FLanguageManager.GetMessage('MASTER_DEPLOY', 'STATUS_MISSING_SOURCE'); //'Source absente';
            NeedsCopy := False;
            SourceDate := 0;
        End Else Begin
            SourceDate := TFile.GetLastWriteTime(SourceFile);
            If Not FileExists(DestFile) Then Begin
                Status := FLanguageManager.GetMessage('MASTER_DEPLOY', 'STATUS_MISSING_DEST'); //'Dest. absente';
                NeedsCopy := True;
                DestDate := 0;
            End Else Begin
                DestDate := TFile.GetLastWriteTime(DestFile);
                If SourceDate > DestDate Then Begin
                    Status := FLanguageManager.GetMessage('MASTER_DEPLOY', 'STATUS_TO_COPY'); //'À copier';
                    NeedsCopy := True;
                End Else Begin
                    Status := FLanguageManager.GetMessage('MASTER_DEPLOY', 'STATUS_UPTODATE'); //'À jour';
                    NeedsCopy := False;
                End;
            End;
        End;

        ListItem := ListView.Items.Add;

        ListItem.Checked := NeedsCopy;
        ListItem.SubItems.AddStrings(['', '', '', '', '', '', '']);

        ListItem.SubItems[FColumns[cColStatus].Index - 1] := Status;
        ListItem.SubItems[FColumns[cColFileName].Index - 1] := Item.Name;
        ListItem.SubItems[FColumns[cCOlSourceDir].Index - 1] := Item.SourceDir;
        ListItem.SubItems[FColumns[cColSourceDate].Index - 1] := DateTimeToStr(SourceDate);
        ListItem.SubItems[FColumns[cColDestDir].Index - 1] := Item.DestDir;

        ListItem.SubItems[FColumns[cColDestDate].Index - 1] := DateTimeToStr(DestDate);
        ListItem.SubItems[FColumns[cColidx].Index - 1] := IntToStr(i);
    End;

    // Appliquer le tri après mise à jour
    If FSortColumn < 0 Then
        FSortColumn := FColumns[cColFileName].Index; // Par défaut, trier par "Nom"

    SortParam := MakeLong(FSortColumn, Ord(FSortAscending));
    ListView.CustomSort(@CompareAppItems, SortParam);

End;

Procedure TFormDeployManager.LoadFromJSON(ListView: TListView);
// Chargement du json
Var
    FileName: String;
    JsonArr: TJSONArray;
    JsonObj: TJSONObject;
    JsonText: String;
    Item: TDeployItem;
    i: integer;

Begin
    FileName := FindConfigPath(AppWatcherJsonFileName);
    If FileName = '' Then Exit;

    JsonText := TFile.ReadAllText(Filename);
    JsonArr := TJSONObject.ParseJSONValue(JsonText) As TJSONArray;

    SetLength(FCopyList, JsonArr.Count);

    For i := 0 To JsonArr.Count - 1 Do Begin
        JsonObj := JsonArr.Items[i] As TJSONObject;
        Item.Name := JsonObj.GetValue('Name').Value;
        Item.SourceDir := JsonObj.GetValue('SourceDir').Value;
        Item.DestDir := JsonObj.GetValue('DestDir').Value;

        FCopyList[i] := Item;

    End;

    RefreshListView(AppList);
End;

End.

