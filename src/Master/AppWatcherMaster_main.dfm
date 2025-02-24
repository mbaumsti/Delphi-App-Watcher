object FormAppWatcherMaster: TFormAppWatcherMaster
  Left = 0
  Top = 0
  Caption = 'App Watcher Master'
  ClientHeight = 691
  ClientWidth = 1249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    1249
    691)
  TextHeight = 17
  object LblAppName: TLabel
    Left = 1024
    Top = 115
    Width = 130
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Programme '#224' stopper'
  end
  object LblDuration: TLabel
    Left = 1024
    Top = 176
    Width = 69
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Dur'#233'e (sec.)'
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 418
    Width = 1249
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 421
  end
  object BtnListApps: TButton
    Left = 1024
    Top = 80
    Width = 210
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Liste App'
    TabOrder = 0
    OnClick = BtnListAppsClick
  end
  object BtnStopApp: TButton
    Left = 1024
    Top = 206
    Width = 210
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Stopper'
    TabOrder = 1
    OnClick = BtnStopAppClick
  end
  object EditAppName: TEdit
    Left = 1024
    Top = 139
    Width = 210
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object BtnStart: TButton
    Left = 1024
    Top = 268
    Width = 210
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Red'#233'marrer tout'
    TabOrder = 3
    OnClick = BtnStartClick
  end
  object BtnCancel: TButton
    Left = 1024
    Top = 237
    Width = 210
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Annuler Stop'
    TabOrder = 4
    OnClick = BtnCancelClick
  end
  object RzNumericEdit1: TRzNumericEdit
    Left = 1164
    Top = 172
    Width = 45
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 5
    DisplayFormat = ',0;(,0)'
    Value = 30.000000000000000000
  end
  object UpDown1: TUpDown
    Left = 1209
    Top = 172
    Width = 16
    Height = 25
    Anchors = [akTop, akRight]
    Associate = RzNumericEdit1
    Max = 600
    Position = 30
    TabOrder = 6
  end
  object RdioFrench: TRadioButton
    Left = 1024
    Top = 19
    Width = 112
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Fran'#231'ais'
    Checked = True
    TabOrder = 7
    TabStop = True
    OnClick = RdioFrenchClick
  end
  object RdioEnglish: TRadioButton
    Left = 1128
    Top = 20
    Width = 113
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'English'
    TabOrder = 8
    OnClick = RdioEnglishClick
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 421
    Width = 1009
    Height = 270
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 240
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    object LblMsg: TLabel
      Left = 318
      Top = 1
      Width = 59
      Height = 17
      Caption = 'Messages'
    end
    object LblClientList: TLabel
      Left = 11
      Top = 1
      Width = 91
      Height = 17
      Caption = 'Liste des clients'
    end
    object MemoLogs: TMemo
      AlignWithMargins = True
      Left = 316
      Top = 25
      Width = 690
      Height = 242
      Margins.Left = 6
      Margins.Top = 25
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 343
    end
    object ListViewClients: TMemo
      AlignWithMargins = True
      Left = 6
      Top = 25
      Width = 301
      Height = 242
      Margins.Left = 6
      Margins.Top = 25
      Align = alLeft
      Lines.Strings = (
        'ListViewClients')
      TabOrder = 1
      ExplicitHeight = 343
    end
  end
  object PanelTop: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 1009
    Height = 418
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 240
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 10
    ExplicitHeight = 320
    object LblAppList: TLabel
      Left = 10
      Top = 3
      Width = 125
      Height = 17
      Caption = 'Liste des applications'
    end
    object StringGridApp: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 25
      Width = 1003
      Height = 390
      Margins.Top = 25
      Align = alClient
      ColCount = 6
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedColClick, goFixedRowClick, goFixedHotTrack, goFixedColDefAlign, goFixedRowDefAlign]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnDblClick = StringGridAppDblClick
      OnDrawCell = StringGridAppDrawCell
      OnFixedCellClick = StringGridAppFixedCellClick
      OnMouseMove = StringGridAppMouseMove
      ExplicitHeight = 292
      ColWidths = (
        105
        130
        96
        171
        84
        376)
    end
  end
  object BtnAgentStop: TButton
    Left = 1027
    Top = 326
    Width = 210
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Arr'#234'ter les agents'
    TabOrder = 11
    OnClick = BtnAgentStopClick
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 2510
    OnConnect = IdTCPServer1Connect
    OnDisconnect = IdTCPServer1Disconnect
    OnExecute = IdTCPServer1Execute
    Left = 147
    Top = 81
  end
  object TimerupdateClient: TTimer
    Interval = 3000
    OnTimer = TimerupdateClientTimer
    Left = 189
    Top = 92
  end
end
