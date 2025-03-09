object DlgAddFile: TDlgAddFile
  Left = 0
  Top = 0
  Caption = 'Ajouter un fichier'
  ClientHeight = 150
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    500
    150)
  TextHeight = 17
  object lblSource: TLabel
    Left = 10
    Top = 20
    Width = 87
    Height = 17
    Caption = 'Fichier source :'
  end
  object lblDest: TLabel
    Left = 10
    Top = 60
    Width = 137
    Height = 17
    Caption = 'R'#233'pertoire destination :'
  end
  object BtnBrowseSource: TSpeedButton
    Left = 450
    Top = 17
    Width = 25
    Height = 22
    Caption = '...'
    OnClick = BtnBrowseSourceClick
  end
  object BtnBrowseDest: TSpeedButton
    Left = 450
    Top = 57
    Width = 25
    Height = 22
    Caption = '...'
    OnClick = BtnBrowseDestClick
  end
  object EditSource: TEdit
    Left = 140
    Top = 17
    Width = 308
    Height = 25
    TabOrder = 0
  end
  object EditDest: TEdit
    Left = 140
    Top = 57
    Width = 308
    Height = 25
    TabOrder = 1
  end
  object OKBtn: TButton
    Left = 300
    Top = 110
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 400
    Top = 110
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
end
