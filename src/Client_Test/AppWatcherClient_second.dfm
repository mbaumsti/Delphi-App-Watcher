object FormAppWatcherClient2: TFormAppWatcherClient2
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
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    Lines.Strings = (
      'Fichier de configuration "AppWatcher_lang_fr.ini"  introuvable.'
      'Fichier de configuration "AppWatcher.ini"  introuvable.')
    TabOrder = 0
  end
  object AppWatcherClient1: TAppWatcherClient
    Memo = Memo1
    Left = 306
    Top = 226
  end
end
