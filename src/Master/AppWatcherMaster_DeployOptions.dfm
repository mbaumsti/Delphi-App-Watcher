object DlgDeployOptions: TDlgDeployOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options de d'#233'ploiement'
  ClientHeight = 128
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 17
  object LblMax: TLabel
    Left = 26
    Top = 49
    Width = 192
    Height = 17
    Caption = 'Nombre de versions '#224' conserver'
  end
  object ChkRotate: TCheckBox
    Left = 23
    Top = 20
    Width = 315
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Activer la rotation des sauvegardes'
    TabOrder = 0
    OnClick = ChkRotateClick
  end
  object SpinMax: TSpinEdit
    Left = 254
    Top = 45
    Width = 84
    Height = 27
    MaxValue = 99
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object BtnOK: TButton
    Left = 104
    Top = 84
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BtnCancel: TButton
    Left = 184
    Top = 84
    Width = 75
    Height = 25
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
end
