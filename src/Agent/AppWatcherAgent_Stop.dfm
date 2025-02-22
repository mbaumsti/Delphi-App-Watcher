object FormStopNotification: TFormStopNotification
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Notification d'#39'arr'#234't'
  ClientHeight = 140
  ClientWidth = 599
  Color = clBtnFace
  CustomTitleBar.ShowCaption = False
  CustomTitleBar.ShowIcon = False
  CustomTitleBar.SystemButtons = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  TextHeight = 15
  object LabelMessage: TLabel
    Left = 21
    Top = 14
    Width = 549
    Height = 65
    AutoSize = False
    Caption = 'LabelMessage'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ProgressBar1: TProgressBar
    Left = 18
    Top = 83
    Width = 557
    Height = 17
    Max = 40
    Step = 1
    TabOrder = 0
  end
  object BtnFermer: TButton
    Left = 18
    Top = 108
    Width = 96
    Height = 25
    Caption = 'Sortir'
    ModalResult = 8
    TabOrder = 1
    Visible = False
    OnClick = BtnFermerClick
  end
end
