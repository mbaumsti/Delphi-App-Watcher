object FormDeployManager: TFormDeployManager
  Left = 0
  Top = 0
  Caption = 'Deployment Manager'
  ClientHeight = 704
  ClientWidth = 1282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    1282
    704)
  TextHeight = 17
  object AppList: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1276
    Height = 651
    Margins.Bottom = 50
    Align = alClient
    Checkboxes = True
    Columns = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = AppListColumnClick
    OnDblClick = AppListDblClick
  end
  object BtnRefresh: TButton
    Left = 4
    Top = 662
    Width = 210
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Rafraichir liste'
    TabOrder = 1
    OnClick = BtnRefreshClick
  end
  object BtnAddFile: TButton
    Left = 427
    Top = 662
    Width = 210
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Ajouter fichier '#224' d'#233'ployer'
    TabOrder = 3
    OnClick = BtnAddFileClick
  end
  object BtnDel: TButton
    Left = 638
    Top = 662
    Width = 210
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Enlever fichier du d'#233'ploiement'
    TabOrder = 4
    OnClick = BtnDelClick
  end
  object BtnDeployExecute: TButton
    Left = 850
    Top = 662
    Width = 210
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Lancer d'#233'ploiement'
    TabOrder = 5
    OnClick = BtnDeployExecuteClick
  end
  object BtnModif: TButton
    Left = 216
    Top = 662
    Width = 210
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Modifier'
    TabOrder = 2
    OnClick = BtnModifClick
  end
end
