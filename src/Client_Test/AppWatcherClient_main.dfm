object FormAppWatcherClient: TFormAppWatcherClient
  Left = 0
  Top = 0
  Caption = 'FormAppWatcherClient - Main'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 618
    Height = 398
    Margins.Bottom = 40
    Align = alClient
    Lines.Strings = (
      'Fichier de configuration "AppWatcher_lang_fr.ini"  introuvable.'
      'Fichier de configuration "AppWatcher.ini"  introuvable.')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 409
    Width = 165
    Height = 25
    Caption = 'Second Form'
    TabOrder = 1
    OnClick = Button1Click
  end
  object RdioStopAccepted: TRadioButton
    Left = 209
    Top = 413
    Width = 113
    Height = 17
    Caption = 'STOP Accepted'
    TabOrder = 2
  end
  object rdioStopRefused: TRadioButton
    Left = 325
    Top = 414
    Width = 113
    Height = 17
    Caption = 'STOP Refused'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object AppWatcherClient1: TAppWatcherClient
    Memo = Memo1
    OnCommandReceived = AppWatcherClient1CommandReceived
    OnStopRequested = AppWatcherClient1StopRequested
    OnGetAppParams = AppWatcherClient1GetAppParams
    Left = 381
    Top = 165
  end
end
