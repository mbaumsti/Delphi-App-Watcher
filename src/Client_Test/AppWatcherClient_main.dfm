object FormAppWatcherClient: TFormAppWatcherClient
  Left = 0
  Top = 0
  Caption = 'FormAppWatcherClient - Second'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
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
      'Configuration file "AppWatcher.ini" not found.')
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
  object AppWatcherClient1: TAppWatcherClient
    Memo = Memo1
    OnCommandReceived = AppWatcherClient1CommandReceived
    OnStopRequested = AppWatcherClient1StopRequested
    OnGetAppParams = AppWatcherClient1GetAppParams
    Lang = langEn
    Left = 381
    Top = 165
  end
end
